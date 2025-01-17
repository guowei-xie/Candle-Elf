# Initialize all stocks daily data
library(config)
library(RSQLite)
library(Tushare)
library(plyr)

tbl_name <- "stock_basic"
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

if(nrow(stock_basic)){
  dbWriteTable(
    db, 
    tbl_name,
    stock_basic, 
    row.names = FALSE, 
    overwrite = TRUE
  )
}

DBI::dbDisconnect(db)
