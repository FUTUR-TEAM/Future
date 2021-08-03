library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)


ui <- mainModuleUI("main_module")

server <- function(input, output, session){
    callModule(mainModule, "main_module")
}




shinyApp(ui = ui, server = server)
