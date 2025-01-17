#!/bin/bash
echo "Start downloading data, it may take a long time"
Rscript download/init_db_path.R
Rscript download/init_stock_basic.R
Rscript download/init_stock_daily.R
Rscript download/init_stock_limit.R
