#' @title Ui part of module for calculating energy of meal
#'
#' @param id character, shiny id
#'
#' @export
mainModuleUI <- function(id){

   ns <- NS(id)

   header <- dashboardHeader(title = "Future")

   sidebar <- dashboardSidebar()

   body <- dashboardBody(
      fluidPage(
         shinyjs::useShinyjs(),
         column(6,
                div(id = ns("box01"), box(
                   width = 12,
                   column(6,
                          pickerInput(
                             inputId = ns("product01"),
                             label = "Wybierz produkt",
                             choices = "",
                             options = list(`live-search` = TRUE)
                          )),
                   column(4,
                          numericInput(
                             inputId = ns("weight01"),
                             label = "Wpisz gramature produktu",
                             value = 0
                          )),
                   shinyWidgets::actionBttn(
                      inputId = ns("add01"),
                      label = "Dodaj produkt",
                      size = "sm",
                      style = "jelly",
                      color = "primary"
                   )
                )),
                hidden(div(id = ns("box02"), box(
                   width = 12,
                   column(
                      6,
                      pickerInput(
                         inputId = ns("product02"),
                         label = "Wybierz produkt",
                         choices = "",
                         options = list(`live-search` = TRUE)
                      )
                   ),
                   column(
                      4,
                      numericInput(
                         inputId = ns("weight02"),
                         label = "Wpisz gramature produktu",
                         value = 0
                      )
                   ),
                   shinyWidgets::actionBttn(
                      inputId = ns("add02"),
                      label = "Dodaj produkt",
                      size = "sm",
                      style = "jelly",
                      color = "primary"
                   ),
                   shinyWidgets::actionBttn(
                      inputId = ns("remove02"),
                      label = "Usun produkt",
                      size = "sm",
                      style = "jelly",
                      color = "danger"
                   ),
                ))),
                hidden(div(id = ns("box03"), box(
                   width = 12,
                   column(
                      6,
                      pickerInput(
                         inputId = ns("product03"),
                         label = "Wybierz produkt",
                         choices = "",
                         options = list(`live-search` = TRUE)
                      )
                   ),
                   column(
                      4,
                      numericInput(
                         inputId = ns("weight03"),
                         label = "Wpisz gramature produktu",
                         value = 0
                      )
                   ),
                   shinyWidgets::actionBttn(
                      inputId = ns("add03"),
                      label = "Dodaj produkt",
                      size = "sm",
                      style = "jelly",
                      color = "primary"
                   ),
                   shinyWidgets::actionBttn(
                      inputId = ns("remove03"),
                      label = "Usun produkt",
                      size = "sm",
                      style = "jelly",
                      color = "danger"
                   ),
                ))),
                hidden(div(id = ns("box04"), box(
                   width = 12,
                   column(
                      6,
                      pickerInput(
                         inputId = ns("product04"),
                         label = "Wybierz produkt",
                         choices = "",
                         options = list(`live-search` = TRUE)
                      )
                   ),
                   column(
                      4,
                      numericInput(
                         inputId = ns("weight04"),
                         label = "Wpisz gramature produktu",
                         value = 0
                      )
                   ),
                   shinyWidgets::actionBttn(
                      inputId = ns("add04"),
                      label = "Dodaj produkt",
                      size = "sm",
                      style = "jelly",
                      color = "primary"
                   ),
                   shinyWidgets::actionBttn(
                      inputId = ns("remove04"),
                      label = "Usun produkt",
                      size = "sm",
                      style = "jelly",
                      color = "danger"
                   ),
                ))),
                hidden(div(id = ns("box05"), box(
                   width = 12,
                   column(
                      6,
                      pickerInput(
                         inputId = ns("product05"),
                         label = "Wybierz produkt",
                         choices = "",
                         options = list(`live-search` = TRUE)
                      )
                   ),
                   column(
                      4,
                      numericInput(
                         inputId = ns("weight05"),
                         label = "Wpisz gramature produktu",
                         value = 0
                      )
                   ),
                   shinyWidgets::actionBttn(
                      inputId = ns("remove05"),
                      label = "Usun produkt",
                      size = "sm",
                      style = "jelly",
                      color = "danger"
                   ),
                ))),
         ),
         column(5, offset = 1, shinyWidgets::actionBttn(inputId = ns("click"),
                                                        label = "Oblicz",
                                                        size = "sm",
                                                        style = "jelly",
                                                        color = "success"),
                verbatimTextOutput(ns("kcalMeal")))
      )
   )

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

   rv <- reactiveValues()

##### loading caloric table and updating inputs with data from table #####

      product_table <-
         read.table(
            system.file("caloric_table.txt", package = "Future"),
            sep = ";",
            header = TRUE
         )
      updatePickerInput(session = session, inputId = "product01", choices = c("", product_table$Nazwa))
      updatePickerInput(session = session, inputId = "product02", choices = c("", product_table$Nazwa))
      updatePickerInput(session = session, inputId = "product03", choices = c("", product_table$Nazwa))
      updatePickerInput(session = session, inputId = "product04", choices = c("", product_table$Nazwa))
      updatePickerInput(session = session, inputId = "product05", choices = c("", product_table$Nazwa))

##### adding new products for the meal #####

      observeEvent(input$add01, {
         shinyjs::show("box02")
         shinyjs::hide("add01")
      })
      observeEvent(input$add02, {
         shinyjs::show("box03")
         shinyjs::hide("add02")
         shinyjs::hide("remove02")
      })
      observeEvent(input$add03, {
         shinyjs::show("box04")
         shinyjs::hide("add03")
         shinyjs::hide("remove03")
      })
      observeEvent(input$add04, {
         shinyjs::show("box05")
         shinyjs::hide("add04")
         shinyjs::hide("remove04")
      })

##### removing products from the meal #####

      observeEvent(input$remove02, {
         shinyjs::hide("box02")
         shinyjs::show("add01")
         shinyjs::show("remove01")
         updateSelectInput(inputId = "product02", selected = "")
         updateNumericInput(inputId = "weight02", value = 0)
      })
      observeEvent(input$remove03, {
         shinyjs::hide("box03")
         shinyjs::show("add02")
         shinyjs::show("remove02")
         updateSelectInput(inputId = "product03", selected = "")
         updateNumericInput(inputId = "weight03", value = 0)
      })
      observeEvent(input$remove04, {
         shinyjs::hide("box04")
         shinyjs::show("add03")
         shinyjs::show("remove03")
         updateSelectInput(inputId = "product04", selected = "")
         updateNumericInput(inputId = "weight04", value = 0)
      })
      observeEvent(input$remove05, {
         shinyjs::hide("box05")
         shinyjs::show("add04")
         shinyjs::show("remove04")
         updateSelectInput(inputId = "product05", selected = "")
         updateNumericInput(inputId = "weight05", value = 0)
      })

##### creating lists for calculating energy of meal
      observeEvent(input$click, {
         req(input$product01)
         rv$out <-
            energy_of_meal(
               list_of_products = list(
                  input$product01,
                  input$product02,
                  input$product03,
                  input$product04,
                  input$product05
               ),
               weight_of_products = list(
                  as.numeric(input$weight01),
                  as.numeric(input$weight02),
                  as.numeric(input$weight03),
                  as.numeric(input$weight04),
                  as.numeric(input$weight05)
               )
            )
      })

##### displaying energy of preparing meal
      output$kcalMeal <- renderText({
         validate(
            need(rv$out, message = "Wybierz produkt")
         )
         paste("Kalorycznosc posilku wynosi", rv$out, "kcal.")
      })

}


