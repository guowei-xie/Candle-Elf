practice_ui <- function(id){
  ns <- NS(id)
  
  fluidPage(
    mainPanel(
      plotlyOutput("main_chart")
    ),
    
    sidebarPanel()
    
  )
  
  
}