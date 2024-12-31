# K chart
K_chart <- function(df, vl = NULL) {
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
  
  plt_df <-
    df %>%
    mutate(
      candle_lower = pmin(open, close),
      candle_upper = pmax(open, close),
      candle_middle = (candle_lower + candle_upper) / 2,
      direction = ifelse(
        open < close | (open == close & close >= pre_close),
        "up",
        "down"
      ),
      direction = factor(direction, levels = names(dir_mp))
    )
  
  ttl <- "{name}({ts_code}): {start_date}-{end_date}"
  
  ttl <- 
    plt_df %>% 
    group_by(name, ts_code) %>% 
    summarise(
      start_date = min(trade_date),
      end_date = max(trade_date),
      .groups = "drop"
    ) %>% 
    with(., stringr::str_glue(ttl))
  
  ma_df <- 
    plt_df %>% 
    pivot_longer(
      cols = starts_with("ma_"),
      names_to = "ma_days",
      values_to = "ma_close"
    ) %>% 
    select(trade_date, ma_days, ma_close)
  
  plt <- 
    plt_df %>%
    ggplot(aes(x = trade_date)) +
    geom_boxplot(
      aes(
        lower = candle_lower,
        middle = candle_middle,
        upper = candle_upper,
        ymin = low,
        ymax = high,
        col = direction,
        fill = direction
      ),
      stat = "identity",
      size = .8,
      width = .5
    ) +
    geom_line(
      data = ma_df,
      aes(
        y = ma_close, 
        group = ma_days,
        col = as.factor(ma_days)
      )
    ) +
    scale_color_manual(values = c(dir_mp, ma_mp)) +
    scale_fill_manual(values = c(dir_mp)) +
    theme(
      axis.text.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      legend.position = "none"
    ) +
    labs(x = "", y = "", title = ttl)
  
  if(!is.null(vl)) {
    plt <- plt +
      geom_vline(
        xintercept = vl,
        col = "salmon", 
        lty = 4
    )
  }
  
  bar <- plt_df %>% 
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
    plt / bar + 
    plot_layout(heights = c(4, 1))
    
  
  return(res)
}
