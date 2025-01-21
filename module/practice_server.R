practice_server <- function(input, output, session) {
  ns <- session$ns

  practice_days <- reactiveVal(0)
  open_status <- reactiveVal("open")
  next_stock <- reactiveVal(0)

  stock_dat <- eventReactive(c(input$change, next_stock()), {
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


  practice_dat <- eventReactive(practice_days(), {
    req(stock_dat())

    dat <- stock_dat()
    idx_max <- nrow(dat)
    
    days <- floor(practice_days())

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

  # Reset status
  observeEvent(c(input$change, next_stock()), {
    practice_days(0)
    open_status("open")
  })

  # Update status
  observeEvent(input$next_step, {
    practice_days(practice_days() + 0.5)
    
    if (practice_days() %% 1 == 0) {
      open_status("open")
    } else {
      open_status("close")
    }
  })

  # output$practice_chart <- plotly::renderPlotly({
  output$practice_chart <- renderPlot({
    candlestick_chart(
      df = practice_dat(),
      status = open_status(),
      args = list(ma_lines = paste0("ma_", c("5","10", "20", "30", "60")))
    ) %>%
      add_boll_bands()
  })
}
