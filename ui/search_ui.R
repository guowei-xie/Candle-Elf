search_ui <- function(id){
  ns <- NS(id)
  
  fluidPage(
    sidebarPanel(
      width = 3,
      selectInput(
        inputId = ns("stock"),
        label = "搜索个股",
        choices = basic_dat$stock_name,
        selected = NULL
      ),
      dateInput(
        inputId = ns("start_date"),
        label = "搜索范围（开始）",
        value = Sys.Date() - 730
      ),
      dateInput(
        inputId = ns("end_date"),
        label = "搜索范围（结束）:"
      ),
      
      numericInput(
        inputId = ns("period_start"),
        label = "向前显示N日",
        value = 30,
        min = 0
      ),
      numericInput(
        inputId = ns("period_end"),
        label = "向后显示N日",
        value = 30,
        min = 0
      ),
      
      hr(),
      # 形态条件
      selectInput(
        inputId = ns("search_func"),
        label = "搜索形态条件",
        choices = list(
          "涨停首板" = "fst_limt_up",
          "收复均线" = "recover_ma",
          "放量倍增" = "vol_times"
        ),
        selected = "fst_limt_up",
        multiple = TRUE
      ),
      
      # 涨停首板条件
      conditionalPanel(
        condition = sprintf("input['%s'].includes('fst_limt_up')", ns("search_func")),
        numericInput(
          inputId = ns("fst_limt_up_interval"),
          label = tags$small("涨停首板条件：N个交易日内首板"),
          value = 5,
          min = 1
        )
      ),
      
      # 收复均线条件
      conditionalPanel(
        condition = sprintf("input['%s'].includes('recover_ma')", ns("search_func")),
        numericInput(
          inputId = ns("recover_ma_nums"),
          label = tags$small("收复均线条件：收复均线数量（可选）"),
          value = 1,
          min = 0
        ),
        selectInput(
          inputId = ns("recover_ma_days"),
          label = tags$small("收复均线条件：收复均线名称（可选）"),
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
        condition = sprintf("input['%s'].includes('vol_times')", ns("search_func")),
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
      
      
      fluidRow(
        column(
          width = 5,
          actionButton(
            inputId = ns("random"),
            label = "换股",
            width = "100%",
            icon = icon("random"),
            style = "background-color: #d3d3d3; color: #000;"
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
    ),
    
    mainPanel(
      uiOutput(ns("charts"))
    )
  )
  
}