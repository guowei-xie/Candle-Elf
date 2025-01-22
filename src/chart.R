# Dynamic CandleStick
candlestick_chart <- function(df, status = "close", args = list()){

  ttl <- "{name}({ts_code}): {start_date}-{end_date}"
  
  ttl <- 
    df %>% 
    group_by(name, ts_code) %>% 
    summarise(
      start_date = min(trade_date),
      end_date = max(trade_date),
      .groups = "drop"
    ) %>% 
    with(., stringr::str_glue(ttl))
  
  pvt_ma <- \(d){
    d %>% 
      pivot_longer(
        cols = starts_with("ma_"),
        names_to = "ma_name",
        values_to = "ma_price"
      ) %>% 
      select(trade_date, ma_name, ma_price)
  }
  
  plt_df <- df %>%
    mutate(
      candle_lower = pmin(open, close),
      candle_upper = pmax(open, close),
      candle_middle = (candle_lower + candle_upper) / 2,
      candle_max = high,
      candle_min = low,
      direction = ifelse(
        open < close | (open == close & close >= pre_close),
        "up",
        "down"
      ),
      direction = factor(direction, levels = names(dir_mp))
    )  
  
  ma_df <- pvt_ma(plt_df)
  
  if(status == "open"){
    lst_row <- head(plt_df, 1)
    
    plt_df <- lst_row %>% 
      mutate(
        candle_lower = open,
        candle_upper = open,
        candle_middle = (candle_lower + candle_upper) / 2,
        candle_max = open,
        candle_min = open,
        direction = ifelse(
          open >= pre_close,
          "up",
          "down"
        ),
        direction = factor(direction, levels = names(dir_mp))
      )  %>% 
      rbind(plt_df[-1, ])
    
    ma_df <- lst_row %>% 
      mutate(
        ma_5 = ma_5 - (close - open) / 5,
        ma_10 = ma_10 - (close - open) / 10,
        ma_20 = ma_20 - (close - open) / 20,
        ma_30 = ma_30 - (close - open) / 30,
        ma_60 = ma_60 - (close - open) / 60
      ) %>% 
      rbind(plt_df[-1, ]) %>% 
      pvt_ma()
  }
  
  plt <- plt_df %>% 
    ggplot(aes(x = trade_date)) +
    geom_boxplot(
      aes(
        lower = candle_lower,
        middle = candle_middle,
        upper = candle_upper,
        ymin = candle_min,
        ymax = candle_max,
        col = direction,
        fill = direction
      ),
      stat = "identity",
      size = .3,
      width = .5
    ) +
    scale_color_manual(values = c(dir_mp, ma_mp)) +
    scale_fill_manual(values = c(dir_mp)) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 10, color = "black"),
      axis.ticks.y = element_line(color = "black"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      legend.position = "none",
      panel.border = element_rect(color = "black", size = 0.8, fill = NA)
    ) +
    labs(x = "", y = "", title = ttl)
    
  
  ma_lines <- pluck(args, "ma_lines")

    if(!is.null(ma_lines)) {
    ma_df <- filter(ma_df, ma_name %in% ma_lines)
    plt <- plt +
      geom_line(
        data = ma_df,
        aes(y = ma_price, group = ma_name, col = as.factor(ma_name))
      )
  }
  
  return(plt)
}

# Add volume bar chart to the candlestick chart.
add_vol <- function(kplt){
  bar <- kplt$data %>% 
    ggplot(aes(x = trade_date, y = vol)) +
    geom_bar(
      aes(fill = direction), 
      stat = "identity",
      width = .5
    ) +
    scale_fill_manual(values = c(dir_mp)) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      legend.position = "none"
    ) +
    labs(x = "", y = "")
  
  res <- 
    kplt / bar + 
    plot_layout(heights = c(4, 1))
  
  return(res)
}

# Add a triangle symbol above the candlestick chart.
add_triangle <- function(kplt, tr_date, col = "salmon"){
  height <- max(kplt$data$high)
  
  kplt +
    geom_point(
      x = tr_date,
      y = height,
      shape = 25,
      size = 3,
      color = col
    ) +
    labs(subtitle = paste0("标记交易日：", tr_date))
}


# Add Bollinger Bands to the candlestick chart.
add_boll_bands <- function(kplt){
  boll_df <- kplt$data %>% 
    pivot_longer(
      cols = ends_with("_band"),
      names_to = "band_line",
      values_to = "band_price"
    ) %>% 
    select(trade_date, band_line, band_price)
  
  kplt +
    geom_line(
      data = boll_df,
      aes(y = band_price, group = band_line),
      col = "black",
      lty = 4
    )
}




