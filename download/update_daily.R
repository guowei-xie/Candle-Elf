# Update stock data everyday
library(tidyverse)
library(config)
library(duckdb)
library(Tushare)

message(as.character(Sys.time()))

cnf <- config::get(config = "duckDB")

duck_db <- dbConnect(
  duckdb(),
  dbdir = paste0(cnf$path, "/", cnf$name),
  read_only = FALSE
)

token <- config::get(config = "tushare")$token
api <- Tushare::pro_api(token = token)

trade_calendar <- 
  api(api_name = "trade_cal") %>% 
  filter(is_open == 1)

today_date <- format(Sys.Date(), "%Y%m%d")

last_trade_date <- dbGetQuery(
  duck_db, 
  "select max(trade_date) as date from stock_daily"
) %>% 
  pull(date)

update_trade_dates <- trade_calendar %>% 
  filter(cal_date > last_trade_date & cal_date <= today_date) %>% 
  pull(cal_date)


# Update stock basic data to database
message("Update stock basic data to database...")
stock_basic <- api(api_name = "stock_basic")
if(nrow(stock_basic)){
  dbWriteTable(
    duck_db, 
    "stock_basic",
    stock_basic, 
    row.names = FALSE, 
    overwrite = TRUE
  )
}

# Update daily data ------------------------------------------------------------
update_daily_date <- function(tbl_name, api_name, dates){
  message("Update ", tbl_name)
  
  for(i in 1:length(dates)) {
    trdate <- dates[[i]]
    
    daily_df <- try(api(
      api_name = api_name,
      trade_date = trdate
    ))
    
    if (!inherits(daily_df, "try-error")) {
      dbWriteTable(
        duck_db, 
        tbl_name,
        daily_df, 
        row.names = FALSE, 
        append = TRUE
      )
      
      message(tbl_name, " update completed: ", trdate, " number of rows: ", nrow(daily_df))
    }else{
      message("Trading day ", trdate, " no data, it may be that the data source has not been updated.")
      message("It will skip the trading day and continue to update.")
    }
    Sys.sleep(1)
  }
}

# Update stock daily data to database 
update_daily_date("stock_daily", "daily", update_trade_dates)
# Update stock limit data to database 
update_daily_date("stock_limit", "stk_limit", update_trade_dates)

DBI::dbDisconnect(duck_db, shutdown = TRUE)

