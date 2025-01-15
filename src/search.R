# 检索交易日区间
search_trade_period <- function(trdate, daily, bfr_days = 0, aft_days = 0) {
  daily$idx <- 1:nrow(daily)
  idx <- daily[daily$trade_date == trdate, "idx"]
  
  if (!length(idx)) {
    message("There is no trading data for this day: ", trdate)
    return(data.frame())
  }
  
  bfr_idx <- idx + bfr_days
  aft_idx <- idx - aft_days
  
  res <-
    daily %>%
    filter(idx <= bfr_idx & idx >= aft_idx)
  
  return(res)
}


# 搜索策略：涨停
search_up_limit <- function(df, args) {
  df <- filter(df, !is.na(up_limit))
  
  if (!nrow(df)) {
    return(NULL)
  }
  
  rdf <- df %>%
    arrange(desc(trade_date)) %>% 
    mutate(
      is_up_limit = if_else(close == up_limit, 1, 0),
      index = 1:n()
    ) %>%
    filter(is_up_limit == 1) %>% #
    mutate(diff_days = lead(index) - index) %>%
    select(-index)
  
  intvl <- pluck(args, "up_limit_interval")
  
  if (!is.null(intvl)) {
    rdf <- filter(rdf, diff_days >= intvl | is.na(diff_days))
  }
  
  res <- rdf$trade_date
  
  return(res)
}


# 搜索策略：收复均线(收复数量或指定均线)
search_recover_ma <- function(df, args) {
  nums <- pluck(args, "recover_ma_nums")
  lines <- pluck(args, "recover_ma_days")
  
  if(is.null(nums) & is.null(lines)) return(NULL)
  
  pvt <- df %>%
    pivot_longer(
      cols = starts_with("ma_"),
      names_to = "ma_days",
      values_to = "ma_close"
    ) %>%
    mutate(
      is_recover = ifelse(
        (pre_close <= ma_close | open <= ma_close) & close > ma_close,
        1, 0
      )
    )
  
  if(!is.null(nums)) {
    res <- pvt %>% 
      group_by(trade_date) %>% 
      summarise(
        recover_num = sum(is_recover),
        .groups = "drop"
      ) %>% 
      filter(recover_num >= nums) %>% 
      pull(trade_date)
  }
  
  if(!is.null(lines)){
    res <- pvt %>% 
      filter(ma_days %in% lines) %>% 
      group_by(trade_date) %>% 
      summarise(
        recover_num = sum(is_recover),
        .groups = "drop"
      ) %>% 
      filter(recover_num == length(lines)) %>% 
      pull(trade_date) %>% 
      unique()
  }
  
  return(res)
}


# 搜索策略：放量倍增
search_vol_times <- function(df, args){
  rct_days <- pluck(args, "vol_times_rct_days")
  times <- pluck(args, "vol_times_times")
  
  if(is.null(rct_days) | is.null(times)) {
    return(NULL)
  }
  
  res <- df %>% 
    arrange(desc(trade_date)) %>% 
    mutate(
      ma_vol = rollapply(vol, width = rct_days, FUN = mean, align = "left", fill = NA),
      ma_vol = lead(ma_vol),
      vol_times = vol / ma_vol
    ) %>% 
    filter(vol_times >= times) %>% 
    pull(trade_date)
  
  return(res)
}


# 搜索策略：收盘涨幅区间
search_close_pct_chg_range <- function(df, args){
  upper <- pluck(args, "close_pct_chg_upper")
  lower <- pluck(args, "close_pct_chg_lower")
  
  if(!is.null(upper)) {
    rdf <- filter(df, pct_chg <= upper)
  }
  
  if(!is.null(lower)){
    rdf <- filter(df, pct_chg >= lower)
  }
  
  res <- rdf$trade_date
  return(res)
}


# 搜索策略：突破布林带上轨
search_through_bband_upper <- function(df, ...){
  res <- df %>% 
    filter(open < upper_band & close > upper_band) %>% 
    pull(trade_date)
  
  return(res)
}


# 搜索策略：反弹突破布林带中轨
search_rebound_bband_middle <- function(df, ...){
  res <- df %>% 
    mutate(
      is_through_middle = pre_close < middle_band & close > middle_band,
      is_reach_lower = low <= lower_band
    ) %>% 
    filter(is_through_middle | is_reach_lower) %>% 
    mutate(is_reach_lower_closest = lag(is_reach_lower)) %>% 
    filter(is_through_middle & is_reach_lower_closest) %>% 
    pull(trade_date)
  
  return(res)
}
