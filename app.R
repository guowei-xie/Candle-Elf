library(shiny)
library(shinythemes)
library(shinycssloaders)
library(config)
library(tidyverse)
library(duckdb)
library(hrbrthemes)
library(zoo)
library(patchwork)
library(TTR)
source("src/search.R")
source("src/chart.R")
source("src/helper.R")
source("src/global.R")
source("ui/search_ui.R")
source("module/search_server.R")

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  
  navbarPage(
    title = cnf$title,
    
    tabPanel("搜索 >>", search_ui("search"))
    # tabPanel("回测 >>", backtest_ui("backtest")),
    # tabPanel("练习 ", practice_ui("practice"))
  )
)

server <- function(input, output, session) {
  callModule(search_server, "search")
}

shinyApp(ui, server)
