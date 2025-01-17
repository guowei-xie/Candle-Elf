# Initialize all stocks limit data
library(config)
library(RSQLite)
library(Tushare)
library(plyr)

tbl_name <- "stock_limit"
message(as.character(Sys.time()), " Table initialization: ", tbl_name)

cnf <- config::get(config = "sqlite")

db <- dbConnect(
  RSQLite::SQLite(),
  paste0(cnf$path, "/", cnf$name)
)

get_tbls <- dbGetQuery(db, "SELECT name FROM sqlite_master WHERE type='table'")

if(tbl_name %in% get_tbls$name){
  rows <- dbExecute(db, paste("DELETE FROM", tbl_name))
  message("Deleted ", rows, " rows from ", tbl_name)
}

token <- Sys.getenv("tushare_token")

if(token == ""){
  stop("Please configure environment variables tushare_token first. If you have any questions, please refer to Readme")
}

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
      db, 
      tbl_name,
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

DBI::dbDisconnect(db)
