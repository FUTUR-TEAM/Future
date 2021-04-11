ui <- fluidPage(

  wellPanel(
    h2("Wybierz wartosci"),
    fluidRow(
      column(2, numericInput("weight", "Waga" , value = 100)),
      column(
        4,
        offset = 1,
        sliderInput("growth", "Wzrost", min = 0.5, max = 2.5, value = 1, step = 0.01)
      ),
      column(
        2,
        offset = 1,
        selectInput("sex", label = "Wybierz plec:", choices = c("female", "male"))
      )
    ),
    fluidRow(
      div("BMI:"),
      verbatimTextOutput("sequence")
    )
  )
)

server <- function(input, output, session) {

  output$sequence <- renderText({
    req(input$weight)
    out <- Future::BMI(input$weight, input$growth, input$sex)
    sprintf("BMI jest równe: %.2f. Ocena sylwetyk: %s.", out$BMI, out$figure)
  })
}

shinyApp(ui, server)
