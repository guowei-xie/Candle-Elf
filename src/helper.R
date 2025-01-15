# Moving Average
add_ma_price <- function(df){
  df %>% 
    arrange(desc(trade_date)) %>% 
    mutate(
      ma_5 = rollapply(close, width = 5, FUN = mean, align = "left", fill = NA),
      ma_10 = rollapply(close, width = 10, FUN = mean, align = "left", fill = NA),
      ma_20 = rollapply(close, width = 20, FUN = mean, align = "left", fill = NA),
      ma_30 = rollapply(close, width = 30, FUN = mean, align = "left", fill = NA),
      ma_60 = rollapply(close, width = 60, FUN = mean, align = "left", fill = NA)
    )
}


# Bollinger Bands
add_boll_bands_price <- function(df){
  df %>%
    arrange(trade_date) %>%
    mutate(
      BB = BBands(HLC = close, n = 20, maType = "SMA", sd = 2),
      middle_band = BB[, "mavg"],  
      upper_band = BB[, "up"],     
      lower_band = BB[, "dn"]
    ) %>%
    select(-BB)
}
