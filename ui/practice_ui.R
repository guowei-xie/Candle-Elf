practice_ui <- function(id){
  ns <- NS(id)
  
  fluidPage(
    mainPanel(
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
      
      # plotlyOutput(ns("practice_chart"))
      plotOutput(ns("practice_chart"))
    ),
    
    sidebarPanel(
      actionButton(
        inputId = ns("change"),
        label = "换一个",
        width = "100%",
        icon = icon("random")
      ),
      actionButton(
        inputId = ns("next_step"),
        label = "继续",
        width = "100%",
        icon = icon("")
      )
    )
  )
}
