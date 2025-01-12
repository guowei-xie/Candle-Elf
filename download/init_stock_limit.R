# Initialize all stocks limit data
message(as.character(Sys.time()), " Table initialization: stcok_limit")
library(config)
library(duckdb)
library(Tushare)
library(plyr)

cnf <- config::get(config = "duckDB")

duck_db <- dbConnect(
  duckdb(),
  dbdir = paste0(cnf$path, "/", cnf$name),
  read_only = FALSE
)

token <- config::get(config = "tushare")$token
api <- Tushare::pro_api(token = token)
stock_basic <- api(api_name = "stock_basic")
stock_nums <- nrow(stock_basic)
cache_N <- 0
cache_df <- data.frame()

progress.bar <- plyr::create_progress_bar("text")
progress.bar$init(stock_nums)
for(i in 1:stock_nums) {
  scd <- stock_basic$ts_code[[i]]
  sdf <- try({api(api_name = "stk_limit", ts_code = scd)}, silent = TRUE)
  
  if (!inherits(sdf, "try-error")) {
    cache_df <- rbind(cache_df, sdf)
  }else{
    message("\nCatch error:", scd)
  }
  
  cache_N <- cache_N + 1
  
  if(cache_N %% 500 == 0 | i == stock_nums){
    dbWriteTable(
      duck_db, 
      "stock_limit",
      cache_df, 
      row.names = FALSE, 
      append = TRUE
    )
    
    cache_df <- data.frame()
    message("\nCache data to database, current: ", i)
  }
  
  Sys.sleep(0.5)
  progress.bar$step()
}

DBI::dbDisconnect(duck_db, shutdown = TRUE)
