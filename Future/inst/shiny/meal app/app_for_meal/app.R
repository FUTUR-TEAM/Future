library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)


ui <- mainModuleUI("main_module")

server <- function(input, output, session){
    callModule(mainModule, "main_module")
}




shinyApp(ui = ui, server = server)




# server <- function(input, output, session) {
#     shinyjs::addClass(selector = "body", class = "sidebar-collapse")
#
#     product_table <-
#         read.table(
#             system.file("caloric_table.txt", package = "Future"),
#             sep = ";",
#             header = TRUE
#         )
#     updatePickerInput(session = session, inputId = "product01", choices = c("", product_table$Nazwa))
#     updatePickerInput(session = session, inputId = "product02", choices = c("", product_table$Nazwa))
#     updatePickerInput(session = session, inputId = "product03", choices = c("", product_table$Nazwa))
#     updatePickerInput(session = session, inputId = "product04", choices = c("", product_table$Nazwa))
#     updatePickerInput(session = session, inputId = "product05", choices = c("", product_table$Nazwa))
#
#     observeEvent(input$add01, {
#         shinyjs::show("box02")
#         shinyjs::hide("add01")
#     })
#     observeEvent(input$add02, {
#         shinyjs::show("box03")
#         shinyjs::hide("add02")
#         shinyjs::hide("remove02")
#     })
#     observeEvent(input$add03, {
#         shinyjs::show("box04")
#         shinyjs::hide("add03")
#         shinyjs::hide("remove03")
#     })
#     observeEvent(input$add04, {
#         shinyjs::show("box05")
#         shinyjs::hide("add04")
#         shinyjs::hide("remove04")
#     })
#
#     observeEvent(input$remove02, {
#         shinyjs::hide("box02")
#         shinyjs::show("add01")
#         shinyjs::show("remove01")
#         updateSelectInput(inputId = "product02", selected = "")
#         updateNumericInput(inputId = "weight02", value = 0)
#     })
#     observeEvent(input$remove03, {
#         shinyjs::hide("box03")
#         shinyjs::show("add02")
#         shinyjs::show("remove02")
#         updateSelectInput(inputId = "product03", selected = "")
#         updateNumericInput(inputId = "weight03", value = 0)
#     })
#     observeEvent(input$remove04, {
#         shinyjs::hide("box04")
#         shinyjs::show("add03")
#         shinyjs::show("remove03")
#         updateSelectInput(inputId = "product04", selected = "")
#         updateNumericInput(inputId = "weight04", value = 0)
#     })
#     observeEvent(input$remove05, {
#         shinyjs::hide("box05")
#         shinyjs::show("add04")
#         shinyjs::show("remove04")
#         updateSelectInput(inputId = "product05", selected = "")
#         updateNumericInput(inputId = "weight05", value = 0)
#     })
#
#
#
#     rv <- reactiveValues()
#
#     observeEvent(input$click, {
#         req(input$product01)
#         rv$out <-
#             energy_of_meal(
#                 list_of_products = list(
#                     input$product01,
#                     input$product02,
#                     input$product03,
#                     input$product04,
#                     input$product05
#                 ),
#                 weight_of_products = list(
#                     as.numeric(input$weight01),
#                     as.numeric(input$weight02),
#                     as.numeric(input$weight03),
#                     as.numeric(input$weight04),
#                     as.numeric(input$weight05)
#                 )
#             )
#     })
#
#     output$kcalMeal <- renderText({
#         validate(
#             need(rv$out, message = "Wybierz produkt")
#         )
#         paste("Kalorycznosc posilku wynosi", rv$out, "kcal.")
#     })
#
# }

