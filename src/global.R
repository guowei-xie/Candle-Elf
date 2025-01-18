# Read configuration
cnf <- config::get()

# Request stock base data
api <- Tushare::pro_api(token = Sys.getenv("tushare_token"))

stock_basic <- 
  try_api(api, api_name = "stock_basic") %>% 
  mutate(stock_name = paste0(name, "(", ts_code, ")"))

# Set chart theme
theme_set(theme_ipsum(base_family = "Kai", base_size = 8))

# Color mapping
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
