cnf <- config::get(config = "duckDB")

duck_db <- dbConnect(
  duckdb(),
  dbdir = paste0(cnf$path, "/", cnf$name),
  read_only = FALSE
)

basic_dat <- dbGetQuery(duck_db, "select * from stock_basic") %>%
  mutate(stock_name = paste0(name, "(", ts_code, ")"))

daily_dat <- dbGetQuery(duck_db, "select * from stock_daily")

limit_dat <- dbGetQuery(duck_db, "select * from stock_limit")

theme_set(theme_ipsum(base_family = "Kai", base_size = 8))

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
