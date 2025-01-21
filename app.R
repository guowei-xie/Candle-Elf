library(shiny)
library(shinythemes)
library(shinycssloaders)
library(config)
library(RSQLite)
library(tidyverse)
library(hrbrthemes)
library(zoo)
library(patchwork)
library(TTR)
library(rlang)
library(plotly)
library(lubridate)
source("src/search.R")
source("src/chart.R")
source("src/helper.R")
source("src/global.R")
source("ui/search_ui.R")
source("ui/practice_ui.R")
source("module/search_server.R")
source("module/practice_server.R")

ui <- fluidPage(
  theme = shinytheme(cnf$theme),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "scripts.js")
  ),
  
  navbarPage(
    title = cnf$title,
    
    tabPanel("发现 >>", search_ui("search")),
    tabPanel("练习 ", practice_ui("practice"))
  )
)

server <- function(input, output, session) {
  callModule(search_server, "search")
  callModule(practice_server, "practice")
}

shinyApp(ui, server)
