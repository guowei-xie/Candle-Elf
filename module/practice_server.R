practice_server <- function(input, output, session) {
  ns <- session$ns
  toggle_cnf_visible <- reactiveVal(FALSE) # 配置面板的显示控制
  attempt <- reactiveVal(0) # 搜索重试次数限制
  steps <- reactiveVal(0) # 单次练习的前进步数
  open_status <- reactiveVal("open") # 开盘状态
  next_stock <- reactiveVal(0) # 练习票数
  hold_lots <- reactiveVal(0) # 持仓数量
  hold_value <- reactiveVal(0) # 持仓价值
  account <- reactiveVal(100000) # 资金账户
  record <- reactiveVal(data.frame()) # 个股交易记录
    
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
  
  # # 重置步数与开盘状态
  # observeEvent(c(input$change, next_stock()), {
  #   steps(0)
  #   open_status("open")
  # })
  
  # 继续训练并重置状态
  observeEvent(next_stock(), {
    steps(0)
    open_status("open")
  })
  
  # 完整练习数据
  practice_dat <- eventReactive(c(input$change, next_stock()), {
    code <- sample(stock_basic$ts_code, 1)
    start <- format(Sys.Date() %m-% years(input$years), "%Y%m%d")
    # 每日行情
    daily <- try_api(
      api,
      api_name = "daily",
      ts_code = code
    )
    # 涨停数据
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

    # 搜索练习图形
    if (input$graph == "random") {
      trade_date <- search_random_trade_date(dat)
    }

    # 有搜索结果时返回数据
    if (!is.null(trade_date)) {
      res <- search_trade_period(
        trdate = trade_date,
        daily = dat,
        bfr_days = cnf$history_days,
        aft_days = cnf$practice_days
      )
    } else if (attempt() <= cnf$attempt_times) {
      # 无搜索结果时重试
      next_stock(next_stock() + 1)
    } else {
      # 重试无果后返回空数据
      res <- data.frame()
    }
  
    return(res)
  })

  # 练习图的动态数据
  practice_chart_dat <- eventReactive(steps(), {
    req(practice_dat())

    dat <- practice_dat()
    idx_max <- nrow(dat) # 总K线数
    days <- floor(steps()) # 交易日进度

    if (idx_max) {
      # 控制K线图显示范围
      res <- dat %>%
        arrange(desc(trade_date)) %>%
        mutate(index = 1:n()) %>%
        filter(
          index <= idx_max - days &
            index >= cnf$practice_days - days
        )
    } else {
      res <- dat # 返回空
    }
    return(res)
  })
  
  # K线状态控制
  observeEvent(steps(), {
    if (steps() %% 1 == 0) {
      open_status("open")
    } else {
      open_status("close")
    }
  })
  
  
  # 交易动作
  observeEvent(input$trade, {
    curr_dat <- head(practice_chart_dat(), 1)
    
    # 价格管理
    if(open_status() == "close"){
      # 收盘后只能填写当日收盘价
      price <- curr_dat$close
    }else{
      # 收盘前自定义条件价
      price <- input$price
    }
    
    req(price)
    
    if(hold_lots()){
      # 持仓时，交易即卖出
      dire <- "sell"
    }else{
      # 空仓时，交易即买入
      dire <- "buy"
    }
    
    # 持仓管理
    # 成交量(手)
    lots <- trade_lots(
      dire, 
      curr_dat, 
      price, 
      accnt = account(),
      hold = hold_lots()
    )
    
    hold_lots(hold_lots() + lots) # 持仓手数
    account(account() - lots * 100 * price) # 账户变化
    
    # 个股成交记录
    if(lots != 0) {
      recd <- trade_record(dire, lots, price, curr_dat)
      record(bind_rows(record(), recd))
    }
    
    # 步数管理
    # 盘中交易成功，前进1步
    if(open_status() == "open" & lots != 0){
      steps(steps() + 1)
    }
    # 盘中交易失败，前进0.5步
    if(open_status() == "open" & lots == 0){
      steps(steps() + 0.5)
    }
    # 收盘交易成功或失败，前进0.5步
    if(open_status() == "close"){
      steps(steps() + 0.5)
    }
  })
  
  # 观望动作
  observeEvent(input$next_step, {
    steps(steps() + 0.5)
  })
  
  # 持仓价值统计
  observeEvent(steps(), {
    curr_dat <- head(practice_chart_dat(), 1)
    
    if(open_status() == "close"){
      price <- curr_dat$close
    }else{
      price <- curr_dat$open
    }
    
    hold_value(hold_lots() * 100 * price)
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
  
  # 统计字样渲染
  # 资金账户
  output$account <- renderUI({
    tags$div(
      style = "text-align: center;",
      tags$small("资金账户"),
      tags$br(),
      tags$span(style = "font-size: 24px; font-weight: bold;", as.integer(account()))
    )
  })
  
  # 持仓价值
  output$hold_value <- renderUI({
    tags$div(
      style = "text-align: center;",
      tags$small("持仓价值"),
      tags$br(),
      tags$span(style = "font-size: 24px; font-weight: bold;", as.integer(hold_value()))
    )
  })
  
  # 累计收益
  output$cum_rate <- renderUI({
    cum_rate <- (account() + hold_value()) / cnf$init_account - 1.0
    cum_rate <- paste0(round(cum_rate, 4) * 100, "%")
    
    if(cum_rate >= 0) {
      col <- "red"
    }else{
      col <- "green"
    }
    
    tags$div(
      style = "text-align: center;",
      tags$small("累计收益"),
      tags$br(),
      tags$span(style = paste0("font-size: 24px; font-weight: bold; color: ", col), cum_rate)
    )
  })
  
  # 按钮状态标签渲染
  observe({
    if(hold_lots() == 0){
      next_step_label <- "空仓观望"
      if(open_status() == "open") {
        trade_label <- "条件价买入"
      }else{
        trade_label <- "收盘价买入"
      }
    }else{
      next_step_label <- "持仓观望"
      if(open_status() == "open") {
        trade_label <- "条件价卖出"
      }else{
        trade_label <- "收盘价卖出"
      }
    }
    
    updateActionButton(session, "next_step", label = next_step_label)
    updateActionButton(session, "trade", label = trade_label)
  })
  
  
  # 练习结束事件
  observe({
    curr_date <- head(practice_chart_dat(), 1)$trade_date
    end_date <- head(practice_dat(), 1)$trade_date
    if(curr_date == end_date & open_status() == "close") {
      # 结束弹窗
      showModal(modalDialog(
        title = "这是一个弹窗",
        "这是弹窗中的文字内容。",
        footer = tagList(
          actionButton(ns("modal_continue"), "继续训练"),
          actionButton(ns("modal_close"), "关闭"),
        ),
        size = "l"
      ))
      
      # 操作按钮替换
      toggle("next_step")
      toggle("trade")
      toggle("price")
      toggle("continue")
    }
  })
  
  # 关闭弹窗
  observeEvent(input$modal_close, {
    removeModal()
  })
  
  # 继续训练
  observeEvent(input$modal_continue, {
    removeModal()
    toggle("next_step")
    toggle("trade")
    toggle("price")
    toggle("continue")
    next_stock(next_stock() + 1)
  })
  
  observe({
    message("----------")
    message("交易记录：")
    glimpse(record())
    message("持仓(手)：")
    print(hold_lots())
    message("资金账户：")
    print(account())
    message("持仓价值：")
    print(hold_value())
  })
  
  
  
  
}
