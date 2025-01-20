practice_server <- function(input, output, session) {
  ns <- session$ns
  
  stock_dat <- reactive({
    message("Run:stock_dat1--------------------")
    code <- sample(stock_basic$ts_code, 1)
    start <- format(Sys.Date() %m-% years(3), "%Y%m%d")
    daily <-
      try_api(api, api_name = "daily", ts_code = code) %>%
      filter(trade_date >= start)
    
    if(input$graph == "random"){
      trade_date <- search_random_trade_date(daily)
    }
    
    if(!is.null(trade_date)){
      res <- search_trade_period(
        trdate = trade_date,
        daily = daily,
        bfr_days = cnf$history_days, 
        aft_days = cnf$practice_days
      )
    }else{
      res <- data.frame()
    }
    
    message("Run:stock_dat2--------------------")
    return(res)
  })
  
  practice_dat <- reactive({
    message("Run:practice_dat1--------------------")
    req(stock_dat())
    practice_days <- 1
    
    dat <- stock_dat()

    idx_max <- nrow(dat)
    
    if(idx_max){
      res <- dat %>% 
        arrange(desc(trade_date)) %>% 
        mutate(index = 1:n()) %>% 
        filter(
          index <= idx_max - practice_days & 
            index >= cnf$practice_days - practice_days 
        )
    }else{
      res <- data.frame()
    }
    message("Run:practice_da2--------------------")
    return(res)
  })
  
  output$practice_chart <- plotly::renderPlotly({
    message("Run:render1 --------------------")
    glimpse(practice_dat())
    candlestick_chart(
      df = practice_dat(),
      status = "close", 
      args = list(ma_lines = "ma_5")
    ) 
      # add_boll_bands()
    
    message("Run:render2 --------------------")
  })
  
  
  
}