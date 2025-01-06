library(shiny)
library(shinythemes)
library(shinycssloaders)
library(config)
library(tidyverse)
library(duckdb)
library(hrbrthemes)
library(zoo)
library(patchwork)
source("src/search.R")
source("src/chart.R")
source("ui/search_ui.R")
source("module/search_server.R")

theme_set(theme_ipsum(base_family = "Kai", base_size = 8))

cnf <- config::get(config = "duckDB")

duck_db <- dbConnect(
  duckdb(),
  dbdir = paste0(cnf$path, "/", cnf$name),
  read_only = FALSE
)

basic_dat <- dbGetQuery(duck_db, "select * from stock_basic") %>%
  mutate(stock_name = paste0(name, "(", ts_code, ")"))
daily_dat <- dbGetQuery(duck_db, "select * from stock_daily")
limit_dat <- dbGetQuery(duck_db, "select * from stock_limit")

dir_mp <- c(
  "up" = "#f03b20",
  "down" = "#31a354"
)

ma_mp <- c(
  "ma_5" = "#252525",
  "ma_10" = "#08519c",
  "ma_20" = "#fd8d3c",
  "ma_30" = "#006d2c",
  "ma_60" = "#c51b8a"
)

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
