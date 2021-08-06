#' Application that displays Future App
#' @return a shiny app
#'
#' @export
runFutureApp <- function() {

  ui <- mainModuleUI("main_module")

  server <- function(input, output, session){
    callModule(mainModule, "main_module")
  }

  shiny::shinyApp(ui = ui, server = server)
}
