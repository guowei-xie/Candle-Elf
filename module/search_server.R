search_server <- function(input, output, session) {
  ns <- session$ns
  
  dat <- reactive({
    req(input$stock)
    code <- sub(".*\\((.*?)\\).*", "\\1", input$stock)
    
    daily <- filter(daily_dat, ts_code == code)
    limit <- filter(limit_dat, ts_code == code)
    basic <- filter(basic_dat, ts_code == code)
    
    df <- daily %>%
      left_join(limit, by = c("ts_code", "trade_date")) %>%
      left_join(basic, by = "ts_code") %>% 
      mutate(
        ma_5 = rollapply(close, width = 5, FUN = mean, align = "left", fill = NA),
        ma_10 = rollapply(close, width = 10, FUN = mean, align = "left", fill = NA),
        ma_20 = rollapply(close, width = 20, FUN = mean, align = "left", fill = NA),
        ma_30 = rollapply(close, width = 30, FUN = mean, align = "left", fill = NA),
        ma_60 = rollapply(close, width = 60, FUN = mean, align = "left", fill = NA)
      )
    
    if(!is.null(input$start_date)) {
      start_date <- gsub("-", "", input$start_date)
      df <- filter(df, trade_date >= start_date)
    }
    
    if(!is.null(input$end_date)) {
      end_date <- gsub("-", "", input$end_date)
      df <- filter(df, trade_date <= end_date)
    }
    
    return(df)
  })
  
  search <- eventReactive(input$search, {
    req(dat())
    req(input$search_func)
    showPageSpinner(color = "black")
    search_func <- input$search_func
    
    tmp <- dat()
    
    if ("fst_limt_up" %in% search_func) {
      tmp <- search_up_limit(
        df = tmp,
        intvl = input$fst_limt_up_interval
      )
    }
    
    if ("recover_ma" %in% search_func) {
      tmp <- search_recover_ma(
        df = tmp,
        nums = input$recover_ma_nums,
        lines = input$recover_ma_days
      )
    }
    
    if ("vol_times" %in% search_func) {
      tmp <- search_vol_times(
        df = dat(),
        rct_days = input$vol_times_rct_days,
        times = input$vol_times_times
      ) %>% 
        filter(trade_date %in% tmp$trade_date)
    }
    
    res <- 
      tmp$trade_date %>% 
      head(30) %>% 
      map(~ search_trade_period(
        trdate = .x, 
        daily = dat(), 
        bfr_days = input$period_start,
        aft_days = input$period_end
        )
      )
    
    names(res) <- head(tmp$trade_date, 30)
    
    return(res)
  })
  
  charts <- reactive({
    req(search())
    df <- search()
    res <- map2(df, names(df), ~ K_chart(.x, .y))
    hidePageSpinner()
    return(res)
  })
  
  output$charts <- renderUI({
    req(charts())
    
    charts_num <- length(charts())
    
    if(charts_num){
      tagList(
        lapply(seq_along(charts()), function(i) {
          plotOutput(outputId = ns(paste0("chart_", i)))
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
        output_id <- paste0("chart_", .x)
        output[[output_id]] <- renderPlot({ .y })
      })
    })
  })
  
  observeEvent(input$random, {
    rdm <- sample(basic_dat$stock_name, 1)
    
    updateSelectInput(
      session,
      inputId = "stock",
      selected = rdm
    )
  })
  
  
}