# 涨停搜索
search_up_limit <- function(df, intvl = NULL, start_date = NULL, end_date = NULL) {
  start_date <- gsub("-", "", start_date)
  end_date <- gsub("-", "", end_date)

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

  # Apply the interval filter if provided
  if (!is.null(intvl)) {
    res <- res %>%
      filter(diff_days >= intvl)
  }

  # Apply the start_date filter if provided
  if (!is.null(start_date)) {
    res <- res %>%
      filter(trade_date >= start_date)
  }

  # Apply the end_date filter if provided
  if (!is.null(end_date)) {
    res <- res %>%
      filter(trade_date <= end_date)
  }

  return(res)
}


# 收复均线(制定数量或指定均线)
search_recover_ma <- function(df, nums = NULL, lines = NULL, start_date = NULL, end_date = NULL) {
  if(is.null(nums) & is.null(lines)) return(NULL)
  start_date <- gsub("-", "", start_date)
  end_date <- gsub("-", "", end_date)
  
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
  
  if (!is.null(start_date)) {
    res <- res %>%
      filter(trade_date >= start_date)
  }
  
  if (!is.null(end_date)) {
    res <- res %>%
      filter(trade_date <= end_date)
  }
  
  return(res)
}



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
