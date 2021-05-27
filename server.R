
server <- function(input, output, session){
  
  #### SEARCH ####
  
  updateSelectizeInput(session, 'rapport_orgnrnavn', 
                       selected = "",
                       choices = pool %>% tbl("fullarsrapport_db") %>% select(orgnrnavn) %>% collect() %>% pull() %>% unique(),
                       options = list(maxOptions = 10), 
                       server = TRUE)
  
  
  onRestored(function(state) {
    updateSelectizeInput(session, "rapport_orgnrnavn", 
                         choices = pool %>% tbl("fullarsrapport_db") %>% select(orgnrnavn) %>% collect() %>% pull() %>% unique(),
                         selected = state$input$rapport_orgnrnavn, 
                         server = TRUE)
  })
  
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  
  output$queryText <- renderText({
    
    paste0("Orgnr: ", strsplit(as.character(input$rapport_orgnrnavn), ": ")[[1]][1], "\n",
           "Navn: ", strsplit(as.character(input$rapport_orgnrnavn), ": ")[[1]][2], "\n",
           "År: ", as.character(input$rapport_aar)) 
  })
  
  orginfotext <- reactive({
    
    paste0("Orgnr: ", strsplit(as.character(input$rapport_orgnrnavn), ": ")[[1]][1], "\n",
           "Navn: ", strsplit(as.character(input$rapport_orgnrnavn), ": ")[[1]][2], "\n",
           "År: ", as.character(input$rapport_aar)) 
    
  })
  
  output$sokestring_tab1 <- renderText({orginfotext()})
  output$sokestring_tab2 <- renderText({orginfotext()})
  output$sokestring_tab3 <- renderText({orginfotext()})
  output$sokestring_tab4 <- renderText({orginfotext()})
  output$sokestring_tab5 <- renderText({orginfotext()})
  output$sokestring_tab6 <- renderText({orginfotext()})
  
  
  #### ACCOUNTING ####
  
  regnskapInput <- reactive({
    
    query <- paste0('{"query":{"bool":{"must":{"match_phrase":{"orgnrnavn":"', as.character(input$rapport_orgnrnavn), '"}},
                      "filter":{ "term":{ "aar":"',as.character(input$rapport_aar),'" }}}}}')
    
    df <- Search(conn, index = "regnskap", body = query, asdf = TRUE, size = 10000)$hits$hit
    df <- as_tibble(df)
    
    shiny::validate(
      need(!is_empty(df), "Ingen regnskap tilgjengelig.")
    )
    
    df %>%
      dplyr::select(starts_with("_source")) %>%
      rename_all(funs(str_replace(., "_source.", "")))
    
  })
  
  output$regnskap <- renderDT({
    
    regnskapInput() %>%
      DT::datatable(., filter = "top", rownames = FALSE, options = list(pageLength = 50))
    
  })
  
  #### NOTES ####
  
  noteinput <- reactive({
    
    notesok <- paste0('{"query": {"bool": {"must": {"match_phrase": {"orgnrnavn": "', as.character(input$rapport_orgnrnavn), '"}},
                      "filter": {"term": {"aar": "',as.character(input$rapport_aar),'"}}}}}')
    
    df <- Search(conn, index = "noter", body = notesok, asdf = TRUE, size = 10000)$hits$hit
    df <- as_tibble(df)
    
    shiny::validate(
      need(!is_empty(df), "Ingen årsrapport tilgjengelig.")
    )
    
    df %>% 
      distinct() %>%
      rename_all(funs(str_replace(., "_source.", "")))
    
  })
  
  output$notedf <- renderDataTable({ 
    
    noteinput()[5:10] %>%
      DT::datatable(., filter = "top", rownames = FALSE)
    
  })
  
  output$notetabell <- renderDataTable({
    
    noteinput()[input[["notedf_rows_all"]],][11] %>%
      unnest(cols = c(`tabell`)) %>%
      DT::datatable(., rownames = FALSE)
    
  })
  
  #### FULL ANNUAL REPORT ####
  
  rapportInput <- reactive({
    
    query <- paste0('{"query": {"bool": {"must": {"match_phrase": {"orgnrnavn": "', as.character(input$rapport_orgnrnavn), '"}},
                    "filter":{"term": {"aar":"',as.character(input$rapport_aar),'"}}}}}')
    
    df <- Search(conn, index = "fullarsrapport", body = query, asdf = TRUE, size = 10000)$hits$hit
    df <- as_tibble(df)
    
    shiny::validate(
      need(!is_empty(df), "Ingen årsrapport tilgjengelig.")
    )
    
    df %>%
      dplyr::select("_source.text")
    
  })
  
  output$fullarsrapport <- renderText({
    
    rapportInput() %>%
      pull(.)
    
  })
  
  #### TIFF ####
  
  
  file <- reactive({
    
    file <- paste0("../Aarsregnskaper_", as.character(input$rapport_aar), "/", as.character(str_extract(input$rapport_orgnrnavn, "[0-9]{3}")), "/", as.character(str_extract(input$rapport_orgnrnavn, "[0-9]{9}")), "+", as.character(input$rapport_aar), ".tif")
    
  })
  
  tiff <- reactive({
    
    shiny::validate(
      need(file.exists(file()), "Ingen bildefil tilgjengelig i skæppen enda.")
    )
    
    image_read(normalizePath(file.path(file()))) 
    
  })
  
  output$image <- renderImage({
    
    tiff <- tiff() %>%
      .[input$side] %>%
      #image_append(., stack=T) %>%
      image_resize("100%") %>%
      image_write(tempfile(fileext='png'), format = 'png')
    
    list(src = tiff, contentType = "image/png", alt = "tiff", width = "800px")
    
  }, deleteFile = TRUE)
  
  observe({
    
    if(input$rapport_orgnrnavn != "" & file.exists(file()))
      return(updateSliderInput(session, "side",
                               max = length(image_read(normalizePath(file.path(file())))),
                               step = 1))
  })
  
  
  #### PDF ####
  
  output$pdfview <- renderUI({
    tags$iframe(style="height:1000px; width:100%", src="annual_report_kahoot.pdf")
  })
  
  #### MACRO ####
  
  output$makro_n <- renderText({
    
    aarbuttn <- paste0(input$aarbuttn, collapse = ",")
    
    naeringsbucket <- list()
    naeringbuttn <- for(i in 1:length(input$naeringbuttn)){
      naeringsbucket[[i]] <- paste0('{"term":{"nace1_sn07":"', input$naeringbuttn[i], '"}}')
    }
    naringstring <- str_c(as.character(naeringsbucket), collapse = ",")
    
    q <- paste0('
                {
                  "query": {
                    "bool": {
                      "must": [
                        {
                          "term": {"text": "',as.character(input$nokkelord1),'"}
                        },
                        {
                          "bool": {
                            "should": [
                              {"terms": {"aar": [',c(aarbuttn),']}},',naringstring,'
                            ],
                            "minimum_should_match": 2,
                            "boost" : 1.0
                          }
                        }
                      ]
                    }
                  }
                }')
    
    as.character(Search(conn, index = "fullarsrapport", body = q, asdf = TRUE, size = 0, track_total_hits = TRUE)$hits[[1]][[1]])
    
    #as.character(elastic::count(conn, index='fullarsrapport', q=q)) # as.character(input$nokkelord1)
    
  })
  
  observeEvent(input$makrobuttn, {
    
    makrodf <- reactive({
      
      aarbuttn <- paste0(input$aarbuttn, collapse = ",")
      
      naeringsbucket <- list()
      naeringbuttn <- for(i in 1:length(input$naeringbuttn)){
        naeringsbucket[[i]] <- paste0('{"term":{"nace1_sn07":"', input$naeringbuttn[i], '"}}')
      }
      naringstring <- str_c(as.character(naeringsbucket), collapse = ",")
      
      q <- paste0('
                {
                  "query": {
                    "bool": {
                      "must": [
                        {
                          "term": {"text": "',as.character(input$nokkelord1),'"}
                        },
                        {
                          "bool": {
                            "should": [
                              {"terms": {"aar": [',c(aarbuttn),']}},',naringstring,'
                            ],
                            "minimum_should_match": 2,
                            "boost" : 1.0
                          }
                        }
                      ]
                    }
                  }
                }')
      
      res <- Search(conn, index = "fullarsrapport", body = q, asdf = TRUE,
                    track_total_hits = TRUE, time_scroll = "1m")
      
      out <- res$hits$hits
      hits <- 1
      
      while(hits != 0){
        res <- scroll(conn, res$`_scroll_id`, time_scroll="1m")
        hits <- length(res$hits$hits)
        if(hits > 0)
          out <- c(out, res$hits$hits)
      }
      
      df <- new_tibble(list("orgnrnavn" = out$`_source.orgnrnavn`,
                            "aar" = out$`_source.aar`,
                            "naering" = out$`_source.nace1_sn07`))
      
      for (i in 11:length(out)) {
        
        orgnrnavn <- out[[i]]$`_source`$orgnrnavn
        aar <- out[[i]]$`_source`$aar
        naering <- out[[i]]$`_source`$nace1_sn07
        
        tryCatch({
          df <- df %>%
            add_row("orgnrnavn" = orgnrnavn,
                    "aar" = aar,
                    "naering" = naering)
        }, error=function(e){})
        
      }
      
      return(df)
      
      shiny::validate(
        need(nrow(df)!=0, "Ingen enheter å vise.")
      )
      
    })
    
    output$makro_df <- renderDT({
      
      df <- makrodf() %>%
        separate(orgnrnavn, into = c("Orgnr", "Navn"), sep = ":") %>%
        rename("År" = "aar")
      
      DT::datatable(df, filter = "top", rownames = FALSE)
      
    })
    
  })
  
  observeEvent(input$hide | nchar(input$nokkelord1)==0, {
    
    output$makro_df <- renderDT({
      
      df <- new_tibble(list("Orgnr" = NULL,
                            "Navn" = NULL,
                            "År" = NULL))
      
      DT::datatable(df, filter = "top", rownames = FALSE)
      
    })
  })
  
  output$downloaddata <- downloadHandler(
    
    filename = paste("aarsrapporter_", as.character(Sys.Date()), "_.csv"),
    
    content = function(file){
      write.csv(as_tibble(makrodf()), file, row.names = FALSE)
    })
  
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  
  setBookmarkExclude(c("notetabell", "naeringbuttn", "nokkelord1", "aarbuttn", "makrobuttn", "tabell", "app"))
  
}

#enableBookmarking(store = "url")
shiny::shinyApp(ui = ui, server = server, enableBookmarking = "url")

