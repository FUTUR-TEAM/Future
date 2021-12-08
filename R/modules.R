#' @title Ui part of module for calculating energy of meal
#'
#' @param id character, shiny id
#'
#' @import shinydashboard
#' @import shiny

#'
#' @export
mainModuleUI <- function(id){

   ns <- NS(id)

   header <- dashboardHeader(title = "Future")

   sidebar <- dashboardSidebar(sidebarMenu(
      menuItem("Tworzenie posilku", tabName = "tworzenie_posilku"),
      menuItem("Zapisane posilki", tabName = "zapisz_posilek")
   ))

   body <- dashboardBody(
      tabItems(
         tabItem(tabName = "tworzenie_posilku",
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
                              shinyjs::hidden(textInput(
                                 inputId = ns("name_of_meal"),
                                 label = "nazwa posilku")),
                              shinyjs::hidden(
                                 shinyWidgets::actionBttn(
                                    inputId = ns("save"),
                                    label = "Zapisz posilek",
                                    size = "sm",
                                    style = "jelly",
                                    color = "primary"
                                 )
                              ))
                       )
                    )
                 ),

         tabItem(tabName = "zapisz_posilek",
                 DT::DTOutput(ns("meta_data_of_meals")),
                 dataTableOutput(ns("meals_table"))
                 )
      ))


   dashboardPage(dashboardHeader(title = "Future"),
                 sidebar,
                 body)
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



   log4r_logger <- log4r::logger(threshold = "INFO")

   log4r_info <- function(msg) {
      log4r::info(log4r_logger, msg)
   }

   log4r_debug <- function(msg) {
      log4r::debug(log4r_logger, msg)
   }



   rv <- reactiveValues(
      n = 0,
      ingreadients_of_meal = data.table::data.table(list_of_products = NULL, weight_of_products = NULL),
      meals_table = data.table::data.table(meal_name = NULL)
   )

##### loading caloric table and updating inputs with data from table #####

      product_table <-
         utils::read.table(
            system.file("caloric_table.txt", package = "Future"),
            sep = ";",
            header = TRUE
         )

      observe({
         log4r_debug("loading ingreadients.tsv")
         if (system.file("ingreadients.tsv", package = "Future") != "") {
            rv$ingreadients_of_meal <- utils::read.table(
               system.file("ingreadients.tsv", package = "Future"),
               header = TRUE,
               fill = TRUE,
               sep = "\t"
            )
         }

##### this is placeholder!!!! It will be replaced with a more mature solution in the future #####
         if (T) {
            log4r_info("ingreadients.tsv loaded successfully")
         }

         log4r_debug("loading meals.tsv")
         if(system.file("meals.tsv", package = "Future") != ""){
         rv$meals_table <- utils::read.table(
            system.file("meals.tsv", package = "Future"),
            header = TRUE,
            fill = TRUE,
            sep = "\t"
         )
         }
         if (T) {
            log4r_info("meals.tsv loaded successfully")
         }
      })

##### adding new products for the meal #####

   observeEvent(input$add_one, {
      log4r_debug("adding box for adding new product")
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
                      value = ""
                      )),
            shinyWidgets::actionBttn(inputId = paste0(session$ns("remove"), rv$n),
                                     label = "usun produkt",
                                     color = "danger")
         ))
      if (T) {
         log4r_info("box for adding new product added successfully")
      }
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
      log4r_debug("calculating parameters of meal")
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
      if (T) {
         log4r_info("parameters of meal calculated successfully")
      }
   })

##### saving a list of ingredients of the prepared meal #####

      observeEvent(input$save, {
         log4r_debug("saving a list of ingreadients of the prepared meal")

         if (input$name_of_meal %in% rv$meals_table$meal_name) {

            showNotification("Posilek o takiej nazwie juz istnieje", type = "error")

         } else {
               list_of_products <- list()
               for (i in 1:rv$n) {
                  list_of_products[[i]] <- input[[paste0("product", i)]]
               }

               weight_of_products <- list()
               for (i in 1:rv$n) {
                  weight_of_products[[i]] <- as.numeric(input[[paste0("weight", i)]])
               }

               rv$meals_table <- rbind(rv$meals_table, data.table::data.table(meal_name = input$name_of_meal))

               info_about_prepared_meal <-
                  cbind(input$name_of_meal,
                        data.table::data.table(list_of_products = unlist(list_of_products),
                                               weight_of_products = unlist(weight_of_products)))

               colnames(info_about_prepared_meal)[[1]] <- "meal_name"

               rv$ingreadients_of_meal <-
                  rbind(rv$ingreadients_of_meal,
                        info_about_prepared_meal)

               utils::write.table(
                  rv$meals_table,

##### TODO: possibility to change file path #####

                  file = file.path(system.file(package = "Future"), "meals.tsv"),
                  sep = "\t",
                  row.names = FALSE
               )

               utils::write.table(
                  rv$ingreadients_of_meal,
                  file = file.path(system.file(package = "Future"), "ingreadients.tsv"),
                  sep = "\t",
                  row.names = FALSE
               )

               showNotification("Zapisano posilek.")
         }
         if (T) {
            log4r_info("list of ingreadients of the prepared meal saved successfully")
         }
      })

##### displaying energy of preparing meal #####

   output$kcalMeal <- renderText({
      log4r_debug("calculating energy of the prepared meal")

      validate(need(input[[paste0("product", rv$n)]],
                    message = "Wybierz produkt"))

      if (T) {
         log4r_info("energy of the prepared meal calculated successfully")
      }

      paste("Kalorycznosc posilku wynosi", rv$out_kcalMeal, "kcal.")
   })

   output$percentMacro <- plotly::renderPlotly({
      log4r_debug("preparing plot with macronutrietns of the prepared meal")

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

      if (T) {
         log4r_info("plot with macronutrietns of the prepared meal prepared successfully")
      }

      macro_pie_chart(rv$out_macroMeal)
   })

   output$glycemicIndex <- renderText({
      log4r_debug("calculating glycemic index of the prepared meal")

      validate(
         need(rv$out_glycemicIndex, message = "Wybierz produkt")
         )

      if (T) {
         log4r_info("glycemic index of the prepared meal calculated successfully")
      }

      paste0("Indeks glikemiczny posilku wynosi ", rv$out_glycemicIndex, ".")
   })

   output$meta_data_of_meals <- DT::renderDT({
      log4r_debug("preparing data table with saved meals")

      if (T) {
         log4r_info("data table with saved meals prepared successfully")
      }

      rv$meals_table
   })

   output$meals_table <- renderDataTable({
      log4r_debug("preparing data table with ingreadients of saved meal")

      row_selected <- input$meta_data_of_meals_row_last_clicked
      req(row_selected)
      name_selected <- rv$meals_table[[row_selected, "meal_name"]]

      if (T) {
         log4r_info("data table with ingreadients of saved meal preparing successfully")
      }

      rv$ingreadients_of_meal[rv$ingreadients_of_meal$meal_name == name_selected, ]
   })
}
