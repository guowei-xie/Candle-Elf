practice_ui <- function(id){
  ns <- NS(id)
  
  fluidPage(
    mainPanel(
      actionButton(
        inputId = ns("toggle_cnf_btn"), 
        label = "展开配置",
        icon = icon("angle-down"),
        style = "background-color: transparent; border: none; color: grey"
      ),
      
      hidden(div(
        id = ns("toggle_cnf_div"),
        br(),
        fluidRow(
          column(
            width = 4,
            selectInput(
              inputId = ns("graph"),
              label = "练习图形",
              choices = list("随机波段" = "random"),
              selected = "random"
            )
          ),

          column(
            width = 4,
            selectInput(
              inputId = ns("market"),
              label = "选股范围(市场)",
              choices = c("主板", "创业板", "北交所", "科创板"),
              selected = "主板",
              multiple = TRUE
            )
          ),

          column(
            width = 4,
            selectInput(
              inputId = ns("years"),
              label = "选股范围(年份)",
              choices = setNames(1:10, paste0("近", 1:10, "年")),
              selected = 3
            )
          ),
        ),
        actionButton(
          inputId = ns("change"),
          label = "重新搜索",
          width = "100%",
          icon = icon("random")
        ),
        hr()
      )),
      br(),
      plotOutput(ns("practice_chart"))
    ),
    
    sidebarPanel(
      fluidRow(
        column(
          width = 4,
          uiOutput(ns("account"))
        ),
        column(
          width = 4,
          uiOutput(ns("hold_value"))
        ),
        column(
          width = 4,
          uiOutput(ns("cum_rate"))
        )
      ),
      br(),
      hr(),
      
      fluidRow(
        style = "display: flex; align-items: center;",
        column(
          width = 4,
          actionButton(
            inputId = ns("next_step"),
            label = "观望",
            width = "100%",
            icon = icon("")
          )
        ),
        
        column(
          width = 5,
          actionButton(
            inputId = ns("trade"),
            label = "交易",
            width = "100%",
            icon = icon("")
          ),
        ),
        
        column(
          width = 3,
          div(
            style = "margin-top: -10px;",  # 向上调整一个像素
            numericInput(
              inputId = ns("price"),
              label = tags$small("价格"),
              value = 12,
              min = 12,
              max = 12
            )
          )
        )
      ),
      
      
      
    )
  )
}
