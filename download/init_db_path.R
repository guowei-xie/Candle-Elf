library(config)

cnf <- config::get(config = "sqlite")
path <- cnf$path

if (!dir.exists(path)) {
  dir.create(path, recursive = TRUE)
  cat("Create database path:", path, "\n")
} 
