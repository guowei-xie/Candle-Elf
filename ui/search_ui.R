search_ui <- function(id){
  ns <- NS(id)
  
  fluidPage(
    sidebarPanel(
      width = 3,
      selectInput(
        inputId = ns("stock"),
        label = "搜索个股",
        choices = stock_basic$stock_name,
        selected = NULL
      ),
      dateInput(
        inputId = ns("scope_start_date"),
        label = "搜索范围（开始）",
        value = Sys.Date() - 730
      ),
      dateInput(
        inputId = ns("scope_end_date"),
        label = "搜索范围（结束）:"
      ),
      
      hr(),
      # 形态条件
      selectInput(
        inputId = ns("search_func"),
        label = "搜索形态条件",
        choices = list(
          "涨停首板" = "search_up_limit",
          "收复均线" = "search_recover_ma",
          "放量倍增" = "search_vol_times",
          "收盘涨幅" = "search_close_pct_chg_range",
          "突破布林上轨" = "search_through_bband_upper",
          "反弹布林中轨" = "search_rebound_bband_middle"
        ),
        selected = NULL,
        multiple = TRUE
      ),
      
      # 涨停首板条件
      conditionalPanel(
        condition = sprintf("input['%s'].includes('search_up_limit')", ns("search_func")),
        numericInput(
          inputId = ns("up_limit_interval"),
          label = tags$small("涨停首板条件：N个交易日内首板"),
          value = 5,
          min = 1
        )
      ),
      
      # 收复均线条件
      conditionalPanel(
        condition = sprintf("input['%s'].includes('search_recover_ma')", ns("search_func")),
        numericInput(
          inputId = ns("recover_ma_nums"),
          label = tags$small("收复均线条件：收复均线数量(可选)"),
          value = 1,
          min = 0
        ),
        selectInput(
          inputId = ns("recover_ma_days"),
          label = tags$small("收复均线条件：收复均线名称(可选)"),
          choices = list(
            "5日均线" = "ma_5",
            "10日均线" = "ma_10",
            "20日均线" = "ma_20",
            "30日均线" = "ma_30",
            "60日均线" = "ma_60"
          ),
          selected = NULL,
          multiple = TRUE
        ),
      ),
      
      # 放量倍增选项
      conditionalPanel(
        condition = sprintf("input['%s'].includes('search_vol_times')", ns("search_func")),
        numericInput(
          inputId = ns("vol_times_rct_days"),
          label = tags$small("放量倍增条件：与近N日平均交易量比较"),
          value = 1,
          min = 1
        ),
        
        numericInput(
          inputId = ns("vol_times_times"),
          label = tags$small("放量倍增条件：放量倍数"),
          value = 1.5,
          min = 0
        )
      ),
      
      # 涨跌幅选项
      conditionalPanel(
        condition = sprintf("input['%s'].includes('search_close_pct_chg_range')", ns("search_func")),
        numericInput(
          inputId = ns("close_pct_chg_upper"),
          label = tags$small("收盘涨幅条件：区间上限(%)"),
          value = NULL,
          min = -30,
          max = 30
        ),
        numericInput(
          inputId = ns("close_pct_chg_lower"),
          label = tags$small("收盘涨幅条件：区间下限(%)"),
          value = NULL,
          min = -30,
          max = 30,
        )
      ),
      
      fluidRow(
        column(
          width = 5,
          actionButton(
            inputId = ns("random"),
            label = "换股",
            width = "100%",
            icon = icon("random")
          )
        ),
        column(
          width = 7,
          actionButton(
            inputId = ns("search"),
            label = "开始搜索",
            width = "100%",
            icon = icon("search")
          )
        )
      ),
      
      hr(),
      tags$small("自定义选项"),
      br(),
      selectInput(
        inputId = ns("market"),
        label = tags$small("随机范围"),
        choices = c("主板", "创业板", "北交所", "科创板"),
        selected = NULL,
        multiple = TRUE
      ),
      
      selectInput(
        inputId = ns("display_lines"),
        label = tags$small("显示均线"),
        choices = list(
          "5日均线" = "ma_5",
          "10日均线" = "ma_10",
          "20日均线" = "ma_20",
          "30日均线" = "ma_30",
          "60日均线" = "ma_60"
        ),
        selected = c("ma_5", "ma_10", "ma_20"),
        multiple = TRUE
      ),
      
      checkboxGroupInput(
        inputId = ns("display_items"), 
        label = tags$small("辅助图显示"), 
        choices = list(
          "布林带" = "display_bband",
          "成交量" = "display_vol"
        ),
        selected = "display_bband")
    ),
    
    mainPanel(
      uiOutput(ns("charts"))
    )
  )
  
}