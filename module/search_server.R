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
      add_ma_price() %>%
      add_boll_bands_price()

    if (!is.null(input$start_date)) {
      start_date <- gsub("-", "", input$start_date)
      df <- filter(df, trade_date >= start_date)
    }

    if (!is.null(input$end_date)) {
      end_date <- gsub("-", "", input$end_date)
      df <- filter(df, trade_date <= end_date)
    }

    return(df)
  })

  args <- reactive({
    list(
      up_limit_interval = input$up_limit_interval,
      recover_ma_nums = input$recover_ma_nums,
      recover_ma_days = input$recover_ma_days,
      vol_times_rct_days = input$vol_times_rct_days,
      vol_times_times = input$vol_times_times,
      close_pct_chg_upper = input$close_pct_chg_upper,
      close_pct_chg_lower = input$close_pct_chg_lower
    )
  })

  search <- eventReactive(input$search, {
    req(dat())
    req(input$search_func)

    showPageSpinner(color = "black")

    trds <- input$search_func %>%
      map(~ str_glue("{.}(dat(), args())")) %>%
      map(~ eval(parse_expr(.))) %>%
      Reduce(intersect, .) %>%
      sort(decreasing = TRUE) %>%
      head(cnf$display_charts)
    

    res <- trds %>%
      map(~ search_trade_period(
        trdate = .x,
        daily = dat(),
        bfr_days = cnf$display_days,
        aft_days = cnf$display_days
      ))

    names(res) <- head(trds, cnf$display_charts)

    return(res)
  })

  charts <- reactive({
    req(search())
    dfs <- search()
    res <- map2(dfs, names(dfs), ~ {
      .x %>%
        candlestick_chart(args = list(ma_lines = input$display_lines)) %>%
        add_boll_bands() %>%
        add_triangle(tr_date = .y) %>%
        add_vol()
    })

    hidePageSpinner()
    return(res)
  })

  output$charts <- renderUI({
    req(charts())

    charts_num <- length(charts())

    if (charts_num) {
      tagList(
        lapply(seq_along(charts()), function(i) {
          plotOutput(outputId = ns(paste0("chart_", i)))
        })
      )
    } else {
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
        output[[output_id]] <- renderPlot({
          .y
        })
      })
    })
  })

  observeEvent(input$random, {
    if (is.null(input$market)) {
      rdm <- sample(basic_dat$stock_name, 1)
    } else {
      rdm <- basic_dat %>%
        filter(market %in% input$market) %>%
        pull(stock_name) %>%
        sample(1)
    }

    updateSelectInput(
      session,
      inputId = "stock",
      selected = rdm
    )
  })
}
