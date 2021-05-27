
ui <- function(request){
  navbarPage(title = "Årsregnskæppen", id = "app",
             theme = shinytheme("flatly"), 
             #tags$head(includeCSS("www/app.css")), theme = "www/app.css",
             
             tabPanel("Søk", id = "tab1",
                      
                      h3("Velg foretak og år"),
                      
                      br(),
                      
                      fluidRow(column(width=12, 
                                      verbatimTextOutput("queryText"))),
                      br(),
                      br(),
                      
                      fluidRow(
                        
                        column(width = 8,
                               selectizeInput("rapport_orgnrnavn", "Skriv inn orgnr eller navn:",
                                              choices = NULL,
                                              width = 500)),
                        
                        column(width = 4,
                               selectInput("rapport_aar", "Velg et ar:",
                                           choices = c("2018", "2019", "2020"),
                                           selected = "2019"))),
                      
                      #bookmarkButton(),
                      
                      fluidRow(column(width = 12,
                                      verbatimTextOutput("sokestring_tab1")
                      ))
                      
             ),
             
             tabPanel("Konvertert tekstfil", id = "tab2",
                      
                      br(),
                      
                      fluidRow(column(width = 12,
                                      verbatimTextOutput("sokestring_tab2")
                      )),
                      
                      br(),
                      
                      fluidRow(column(width = 12,
                                      wellPanel(
                                        
                                        withSpinner(verbatimTextOutput("fullarsrapport"), type = 2)
                                        
                                      )))
                      
             ),
             
             tabPanel("Tiff", id = "tab3",
                      
                      br(),
                      
                      fluidRow(column(width = 12,
                                      verbatimTextOutput("sokestring_tab3")
                      )),
                      
                      fluidRow(column(width = 8,
                                      
                                      withSpinner(imageOutput("image"), type = 2)),
                               
                               column(width = 3,
                                      
                                      sliderInput("side", "Velg en side:",
                                                  width = "1000px",
                                                  value = 1,
                                                  min = 1, max = 10))
                               
                      )),
             
             tabPanel("Pdf", id = "tab4",
                      
                      br(),
                      
                      fluidRow(column(width = 12, class = "sokestring",
                                      verbatimTextOutput("sokestring_tab4")
                      )),
                      
                      fluidRow(column(width = 12,
                                      
                                      uiOutput("pdfview")))
                      
             ),
             
             tabPanel("Noter", id = "tab5",
                      
                      h3("Noter"),
                      
                      br(),
                      
                      fluidRow(column(width = 12,
                                      verbatimTextOutput("sokestring_tab5")
                      )),
                      
                      br(),
                      
                      fluidRow(
                        
                        tags$head(
                          tags$link(
                            rel = "stylesheet",
                            type = "text/css",
                            href = "app.css")
                        ),
                        
                        column(width = 12,
                               p("Filtrer ut foretak og notenummer for å se oversiktlig tabell for noter."),
                               actionButton("tabell", label = "Vis tabell i note", icon=icon('table')),
                               
                               bsModal(id = "notetabell", #title = "Se tabell i note",
                                       trigger = "tabell", size = "large",
                                       textOutput("notetabellinfo"),
                                       dataTableOutput("notetabell"))
                        )
                      ),
                      
                      br(),
                      br(),
                      
                      fluidRow(column(width = 12,
                                      wellPanel(
                                        DT::DTOutput("notedf")
                                      )))
             ),
             
             tabPanel("Regnskap", id = "tab6",
                      
                      h3("Regnskap"),
                      
                      br(),
                      
                      fluidRow(column(width = 12,
                                      wellPanel(
                                        verbatimTextOutput("sokestring_tab6")
                                      ))),
                      
                      fluidRow(column(width = 12,
                                      wellPanel(
                                        DT::DTOutput("regnskap")
                                      ))
                      )
             ),
             
             tabPanel("Makro", id = "tab7",
                      
                      h3("Makrosok populasjon"),
                      
                      fluidRow(
                        
                        column(width = 6,
                               
                               textInput("nokkelord1", "Skriv inn nokkelord:")
                               
                        )),
                      
                      fluidRow(
                        
                        column(width = 6,
                               
                               checkboxGroupButtons("aarbuttn",
                                                    label = "Velg år:",
                                                    choices = c("2018", "2019", "2020"),
                                                    selected = "2020")),
                        
                        column(width = 6,
                               
                               selectizeInput("naeringbuttn",
                                              label = "Velg næring:",
                                              choices = factor(naring),
                                              selected = "46.693",
                                              multiple = TRUE))),
                      
                      fluidRow(
                        
                        column(width = 12,
                               wellPanel(
                                 verbatimTextOutput("makro_n"))
                        )),
                      
                      fluidRow(
                        
                        column(width = 12,
                               
                               h4("Obs: Å hente tabellen kan ta litt tid.")
                        )),
                      
                      fluidRow(
                        
                        column(width = 4,
                               
                               actionBttn("makrobuttn", "Vis tabell", color = "primary")),
                        
                        column(width = 4,
                               
                               actionBttn("hide", "Skjul tabell", color = "primary")),
                        
                        column(width = 4,
                               
                               downloadButton("downloaddata", "Last ned data"))),
                      
                      fluidRow(
                        column(width = 12,
                               wellPanel(
                                 withSpinner(DTOutput("makro_df"), type = 2)))
                      )
             )
  )}

