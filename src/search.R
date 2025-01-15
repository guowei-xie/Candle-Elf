# 搜索策略：涨停
search_up_limit <- function(df, intvl = NULL) {
  df <- filter(df, !is.na(up_limit))

  if (!nrow(df)) {
    return(NULL)
  }

  res <-
    df %>%
    mutate(
      is_up_limit = if_else(close == up_limit, 1, 0),
      index = 1:n()
    ) %>%
    filter(is_up_limit == 1) %>% #
    mutate(diff_days = lead(index) - index) %>%
    select(-index)

  if (!is.null(intvl)) {
    res <- filter(res, diff_days >= intvl)
  }

  return(res)
}


# 搜索策略：收复均线(收复数量或指定均线)
search_recover_ma <- function(df, nums = NULL, lines = NULL) {
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
    fd <- pvt %>% 
      group_by(trade_date) %>% 
      summarise(
        recover_num = sum(is_recover),
        .groups = "drop"
      ) %>% 
      filter(recover_num >= nums) %>% 
      pull(trade_date)
  }
  
  if(!is.null(lines)){
    fd <- pvt %>% 
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
  
  res <- filter(df, trade_date %in% fd)
  
  return(res)
}


# 搜索策略：放量倍增
search_vol_times <- function(df, rct_days, times){
  res <- 
    df %>% 
    mutate(
      ma_vol = rollapply(vol, width = rct_days, FUN = mean, align = "left", fill = NA),
      ma_vol = lead(ma_vol),
      vol_times = vol / ma_vol
    ) %>% 
    filter(vol_times >= times) 
  
  return(res)
}


# 搜索策略：收盘涨幅区间
search_close_pct_chg_range <- function(df, upper = NULL, lower = NULL){
  if(!is.null(upper)) {
    df <- filter(df, pct_chg <= upper)
  }
  
  if(!is.null(lower)){
    df <- filter(df, pct_chg >= lower)
  }
  
  return(df)
}


# 搜索策略：突破布林带上轨
search_through_bband_upper <- function(df){
  df <- df %>% 
    filter(open < upper_band & close > upper_band)
  return(df)
}


# 搜索策略：反弹突破布林带中轨
search_rebound_bband_middle <- function(df){
  df <- 
    df %>% 
    mutate(
      is_through_middle = pre_close < middle_band & close > middle_band,
      is_reach_lower = low <= lower_band
    ) %>% 
    filter(is_through_middle | is_reach_lower) %>% 
    mutate(is_reach_lower_closest = lag(is_reach_lower)) %>% 
    filter(is_through_middle & is_reach_lower_closest)
  
  return(df)
}


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
