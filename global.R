
library(dplyr)
library(tibble)
library(tidyr)
library(purrr)
library(stringr)
library(RSQLite)
library(DBI)
library(dbplyr)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinyBS)
library(shinycssloaders)
library(DT)
library(pool)
library(elastic)
library(shiny)
library(dotenv)
library(magick)

conn <- connect(
  host = "",
  port = 9200,
  path = NULL,
  transport_schema = "http",
  user = Sys.getenv("VAR1"),
  pwd = Sys.getenv("VAR2"),
  errors = "complete"
)

pool <- pool::dbPool(RSQLite::SQLite(), dbname = "arsrapporter.sqlite")

onStop(function() {
  poolClose(pool)
})

options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

naring <- pool %>% 
  tbl("fullarsrapport_db") %>% 
  select(nace1_sn07) %>% 
  collect() %>% na.omit() %>% 
  pull() %>% 
  unique()

options(shiny.maxRequestSize = 9000 * 10024 ^ 10) # For big images.
