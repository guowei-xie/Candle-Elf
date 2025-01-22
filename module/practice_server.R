practice_server <- function(input, output, session) {
  ns <- session$ns
  toggle_cnf_visible <- reactiveVal(FALSE) # 配置面板的显示控制
  steps <- reactiveVal(0) # 单次练习的前进步数
  open_status <- reactiveVal("open") # 开盘状态
  next_stock <- reactiveVal(0) # 练习票数
  hold_lots <- reactiveVal(0) # 持仓数量
  account <- reactiveVal(100000) # 资金账户
    
  # 配置面板
  observeEvent(input$toggle_cnf_btn, {
    shinyjs::toggle("toggle_cnf_div")
    current_state <- !toggle_cnf_visible()
    toggle_cnf_visible(current_state)
    
    if (current_state) {
      updateActionButton(session, "toggle_cnf_btn", label = "收起配置", icon = icon("angle-up"))
    } else {
      updateActionButton(session, "toggle_cnf_btn", label = "展开配置", icon = icon("angle-down"))
    }
  })
  
  # 重置步数与开盘状态
  observeEvent(c(input$change, next_stock()), {
    steps(0)
    open_status("open")
  })

  # 完整练习数据
  practice_dat <- eventReactive(c(input$change, next_stock()), {
    code <- sample(stock_basic$ts_code, 1)
    start <- format(Sys.Date() %m-% years(input$years), "%Y%m%d")

    daily <- try_api(
      api,
      api_name = "daily",
      ts_code = code
    )

    limit <- try_api(
      api,
      api_name = "stk_limit",
      ts_code = code
    )

    dat <- daily %>%
      left_join(limit, by = c("ts_code", "trade_date")) %>%
      left_join(stock_basic, by = "ts_code") %>%
      add_ma_price() %>%
      add_boll_bands_price() %>% 
      filter(trade_date >= start)

    if (input$graph == "random") {
      trade_date <- search_random_trade_date(dat)
    }

    if (!is.null(trade_date)) {
      res <- search_trade_period(
        trdate = trade_date,
        daily = dat,
        bfr_days = cnf$history_days,
        aft_days = cnf$practice_days
      )
    } else {
      res <- data.frame()
    }

    return(res)
  })

  # 练习图的动态数据
  practice_chart_dat <- eventReactive(steps(), {
    req(practice_dat())

    dat <- practice_dat()
    idx_max <- nrow(dat)
    days <- floor(steps())

    if (idx_max) {
      res <- dat %>%
        arrange(desc(trade_date)) %>%
        mutate(index = 1:n()) %>%
        filter(
          index <= idx_max - days &
            index >= cnf$practice_days - days
        )
    } else {
      res <- data.frame()
    }
    return(res)
  })
  
  # 交易
  observeEvent(input$trade, {
    curr_dat <- head(practice_chart_dat(), 1)
    
    # 价格
    if(open_status() == "close"){
      price <- curr_dat$pre_close
    }else{
      price <- input$price
    }
    
    req(price)
    
    if(hold_lots()){
      # 卖出
      trade_res <- trade_sell(hold_lots(), curr_dat, price)
      hold_lots(hold_lots() - trade_res)
      message("卖出：", trade_res)
    }else{
      # 买入
      trade_res <- trade_buy(account(), curr_dat, price)
      hold_lots(hold_lots() + trade_res)
      message("买入：", trade_res)
    }
    
    # 开盘（条件单）交易成功，前进1步
    if(open_status() == "open" & trade_res != 0){
      steps(steps() + 1)
      message("开盘交易成功，前进1，前进后steps=", steps())
    }
    # 开盘（条件单）交易失败，前进0.5步
    if(open_status() == "open" & trade_res == 0){
      steps(steps() + 0.5)
      message("开盘交易失败，前进0.5，前进后steps=", steps())
    }
    # 收盘（收盘价）交易成功或失败，前进0.5步
    if(open_status() == "close"){
      steps(steps() + 0.5)
      message("收盘交易，前进0.5，前进后steps=", steps())
    }
    
  })
  
  # 观望
  observeEvent(input$next_step, {
    steps(steps() + 0.5)
    message("观望，前进0.5，前进后steps=", steps())
  })
  
  # 开盘状态控制
  observeEvent(steps(), {
    if (steps() %% 1 == 0) {
      open_status("open")
    } else {
      open_status("close")
    }
  })
  
  # 主图渲染
  output$practice_chart <- renderPlot({
    candlestick_chart(
      df = practice_chart_dat(),
      status = open_status(),
      args = list(ma_lines = paste0("ma_", c("5","10", "20", "30", "60")))
    ) %>%
      add_boll_bands()
  })
  
  
  
  
}
