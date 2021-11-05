#' @title Ui part of module for calculating energy of meal
#'
#' @param id character, shiny id
#'
#' @import shinydashboard
#' @import shiny
#' @import shinyjs
#' @import shinyFeedback
#' @import datatable
#'
#' @export
mainModuleUI <- function(id){

   ns <- NS(id)

   header <- dashboardHeader(title = "Future")

   sidebar <- dashboardSidebar()

   body <- dashboardBody(
      fluidPage(
         shinyFeedback::useShinyFeedback(),
         shinyjs::useShinyjs(),
         fluidRow(
            column(6,
                shinyWidgets::actionBttn(inputId = ns("add_one"),
                                         "Dodaj kolejny produkt"),
                div(id = ns("placeholder"))),
            column(5, offset = 1,
                   shinyWidgets::actionBttn(
                      inputId = ns("click"),
                      label = "Oblicz",
                      size = "sm",
                      style = "jelly",
                      color = "success"
                   ),
                   verbatimTextOutput(ns("kcalMeal")),
                   plotly::plotlyOutput(ns("percentMacro")),
                   verbatimTextOutput(ns("glycemicIndex")),
                   shinyjs::hidden(
                      textInput(inputId = ns("name_of_meal"),
                                label = "nazwa posilku"),
                      shinyWidgets::actionBttn(
                         inputId = ns("save"),
                         label = "Zapisz posilek",
                         size = "sm",
                         style = "jelly",
                         color = "primary"
                      )
                   ),
                   dataTableOutput(ns("list_of_meals")),
                   dataTableOutput(ns("meta_data_of_meals")))
         )
      ))

   dashboardPage(header, sidebar, body)

}

#' @title Server part of module for calculating energy of meal
#'
#' @description Module for adding products and their weights to the meal.
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#'
#' @export
mainModule <- function(input, output, session){

   shinyjs::addClass(selector = "body", class = "sidebar-collapse")

   rv <- reactiveValues(
      n = 0,
      ingreadients_of_meal = data.table::data.table(list_of_products = NULL, weight_of_products = NULL),
      list_of_meals = list()
   )

##### loading caloric table and updating inputs with data from table #####

      product_table <-
         utils::read.table(
            system.file("caloric_table.txt", package = "Future"),
            sep = ";",
            header = TRUE
         )

##### adding new products for the meal #####

   observeEvent(input$add_one, {
      rv$n <- rv$n + 1

      insertUI(
         selector = paste0("#", session$ns("placeholder")),
         where = "beforeEnd",
         ui = div(
            id = paste0(session$ns("box"), rv$n),
            column(8,
                   shinyWidgets::pickerInput(
                      inputId = paste0(session$ns("product"), rv$n),
                      label = "wybierz produkt",
                      choices = c("", product_table$Nazwa),
                      options = list(`live-search` = TRUE)
                      )),
            column(4,
                   numericInput(
                      inputId = paste0(session$ns("weight"), rv$n),
                      label = "Wpisz gramature produktu",
                      value = 100
                      )),
            shinyWidgets::actionBttn(inputId = paste0(session$ns("remove"), rv$n),
                                     label = "usun produkt",
                                     color = "danger")
         ))
   })

      lapply(X = 1:100,
          FUN = function(i){

             observeEvent(input[[paste0("remove", i)]],{
                rv[[paste0("box", i)]] <- input[[paste0("remove", i)]]
                removeUI(
                   selector = paste0("#", session$ns("box"), i)
                )

                rv$n <- rv$n - 1
             })
          })


##### creating lists for calculating energy of meal #####

   observeEvent(input$click, {
      lapply(
         X = 1:rv$n,
         FUN = function(i) {

            validate(
               need(paste0("#", session$ns("product"), rv$n),
                    message = "Wybierz produkt")
               )

            list_of_products <- list()
            for (i in 1:rv$n) {
               list_of_products[[i]] <- input[[paste0("product", i)]]
            }

            weight_of_products <- list()
            for (i in 1:rv$n) {
               weight_of_products[[i]] <- as.numeric(input[[paste0("weight", i)]])
            }

            rv$out_kcalMeal <-
               energy_of_meal(list_of_products, weight_of_products)

            rv$out_macroMeal <-
               macronutrients_of_meal(list_of_products, weight_of_products)

            rv$out_glycemicIndex <-
               glycemic_index_of_meal(list_of_products, weight_of_products)

            shinyjs::show("name_of_meal")
            shinyjs::show("save")
         }
      )
   })

##### saving a list of ingredients of the prepared meal #####

      observeEvent(input$save, {

               list_of_products <- list()
               for (i in 1:rv$n) {
                  list_of_products[[i]] <- input[[paste0("product", i)]]
               }

               weight_of_products <- list()
               for (i in 1:rv$n) {
                  weight_of_products[[i]] <- as.numeric(input[[paste0("weight", i)]])
               }

               rv$ingreadients_of_meal <-
                  rbind(rv$ingreadients_of_meal, data.table::data.table(list_of_products, weight_of_products))


               rv$list_of_meals[[length(rv$list_of_meals) + 1]] <- input$name_of_meal
      })

##### displaying energy of preparing meal #####

   output$kcalMeal <- renderText({

      validate(need(input[[paste0("product", rv$n)]],
                    message = "Wybierz produkt"))

      paste("Kalorycznosc posilku wynosi", rv$out_kcalMeal, "kcal.")
   })

   output$percentMacro <- plotly::renderPlotly({

      req(rv$out_macroMeal)

      output_macroMeal <- lapply(
         1:rv$n,
         FUN = function(x) {
            inputId <- paste0("weight", x)
            out_macroMeal_exist <-
               as.numeric(isolate(reactiveValuesToList(input))[[inputId]]) > 0
            shinyFeedback::feedbackWarning(inputId,!out_macroMeal_exist,
                                           "Uzupelnij wage produktu")

            out_macroMeal_exist
         }
      )

      req(all(unlist(output_macroMeal)), cancelOutput = TRUE)

      macro_pie_chart(rv$out_macroMeal)
   })

   output$glycemicIndex <- renderText({
      validate(
         need(rv$out_glycemicIndex, message = "Wybierz produkt")
         )

      paste0("Indeks glikemiczny posilku wynosi ", rv$out_glycemicIndex, ".")
   })

   output$list_of_meals <- renderDataTable({
      data.table::data.table(rv$ingreadients_of_meal)
   })

   output$meta_data_of_meals <- renderDataTable({
      data.table::data.table(rv$list_of_meals)
   })
}
