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

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  tabsetPanel(
    br(),
    tabPanel(
      "搜索",
      br(),
      sidebarPanel(
        width = 3,
        selectInput(
          inputId = "search_stock",
          label = "搜索个股",
          choices = basic_dat$stock_name,
          selected = NULL
        ),
        selectInput(
          inputId = "search_func",
          label = "搜索形态",
          choices = list(
            "涨停首板" = "fst_limt_up",
            "收复均线" = "recover_ma"
          ),
          selected = "fst_limt_up",
          multiple = TRUE
        ),
        
        dateInput(
          inputId = "search_start_date",
          label = "搜索范围（开始）",
          value = Sys.Date() - 730
        ),
        dateInput(
          inputId = "search_end_date",
          label = "搜索范围（结束）:"
        ),
        
        conditionalPanel(
          condition = "input['search_func'].indexOf('fst_limt_up') != -1",
          numericInput(
            inputId = "search_interval",
            label = "N个交易日内首板",
            value = 5,
            min = 1
          )
        ),
        
        conditionalPanel(
          condition = "input['search_func'].indexOf('recover_ma') != -1",
          numericInput(
            inputId = "search_recover_ma_nums",
            label = "收复均线数量（可选）",
            value = 1,
            min = 0
          ),
          selectInput(
            inputId = "search_recover_ma_days",
            label = "收复均线名称（可选）",
            choices = list(
              "5日均线" = "ma_5",
              "10日均线" = "ma_10",
              "20日均线" = "ma_20",
              "30日均线" = "ma_30",
              "60日均线" = "ma_60"
            ),
            selected = NULL,
            multiple = TRUE
          ),
        ),
        
        
        numericInput(
          inputId = "search_period_start",
          label = "附带图形的前N日行情",
          value = 30,
          min = 0
        ),
        numericInput(
          inputId = "search_period_end",
          label = "附带图形的后N日行情",
          value = 30,
          min = 0
        ),
        fluidRow(
          column(
            width = 4,
            actionButton(
              inputId = "search_random",
              label = "换股",
              width = "100%",
              icon = icon("random"),
              style = "background-color: #d3d3d3; color: #000;"
            )
          ),
          column(
            width = 8,
            actionButton(
              inputId = "search_action",
              label = "开始搜索",
              width = "100%",
              icon = icon("search")
            )
          )
        ),
      ),
      mainPanel(
        uiOutput("search_charts")
      )
    ),
    tabPanel(
      "回测",
      br(),
      sidebarPanel(),
      mainPanel()
    )
  )
)

server <- function(input, output, session) {
  dat <- reactive({
    req(input$search_stock)
    showPageSpinner(color = "black")
    code <- sub(".*\\((.*?)\\).*", "\\1", input$search_stock)

    daily <- filter(daily_dat, ts_code == code)
    limit <- filter(limit_dat, ts_code == code)
    basic <- filter(basic_dat, ts_code == code)

    daily %>%
      left_join(limit, by = c("ts_code", "trade_date")) %>%
      left_join(basic, by = "ts_code") %>% 
      mutate(
        ma_5 = rollapply(close, width = 5, FUN = mean, align = "left", fill = NA),
        ma_10 = rollapply(close, width = 10, FUN = mean, align = "left", fill = NA),
        ma_20 = rollapply(close, width = 20, FUN = mean, align = "left", fill = NA),
        ma_30 = rollapply(close, width = 30, FUN = mean, align = "left", fill = NA),
        ma_60 = rollapply(close, width = 60, FUN = mean, align = "left", fill = NA)
      )
  })

  search <- eventReactive(input$search_action, {
    req(dat())
    req(input$search_func)
    search_func <- input$search_func
    
    tmp <- dat()

    if ("fst_limt_up" %in% search_func) {
      tmp <- search_up_limit(
        df = tmp,
        intvl = input$search_interval,
        start_date = input$search_start_date,
        end_date = input$search_end_date
      )
    }
    
    if ("recover_ma" %in% search_func) {
      tmp <- search_recover_ma(
        df = tmp,
        nums = input$search_recover_ma_nums,
        lines = input$search_recover_ma_days,
        start_date = input$search_start_date,
        end_date = input$search_end_date
      )
    }
    
    
    res <- 
      tmp$trade_date %>% 
      map(~ search_trade_period(
        trdate = .x, 
        daily = dat(), 
        bfr_days = input$search_period_start,
        aft_days = input$search_period_end)
      )
    
    names(res) <- tmp$trade_date
    
    return(res)
  })
  
  charts <- reactive({
    req(search())
    df <- search()
    res <- map2(df, names(df), ~ K_chart(.x, .y))
    hidePageSpinner()
    return(res)
  })
  
  output$search_charts <- renderUI({
    req(charts())
    
    charts_num <- length(charts())
  
    if(charts_num){
      tagList(
        lapply(seq_along(charts()), function(i) {
          plotOutput(outputId = paste0("search_chart_", i))
        })
      )
    }else {
      tagList(
        div("未搜索到结果", style = "font-size: 18px; color: #ff6347; text-align: center; padding: 20px;")
      )
    }
    
  })

  observe({
    req(charts())
    walk2(seq_along(charts()), charts(), ~ {
      local({
        output_id <- paste0("search_chart_", .x)
        output[[output_id]] <- renderPlot({ .y })
      })
    })
  })
  
  observeEvent(input$search_random, {
    rdm <- sample(basic_dat$stock_name, 1)
    
    updateSelectInput(
      session,
      inputId = "search_stock",
      selected = rdm
    )
  })
  

}

shinyApp(ui, server)
