#' @title Interactive pie chart of percent of macronutrients in the prepared meal
#'
#' @description Function for creating interactive pie chart of percent of macronutrients in prepared meal
#'
#' @param macro_percent data frame, data frame of amount of macronutrients used to prepare the meal
#'
#' @examples
#' list_of_products <- list("Banan", "Truskawki")
#' weight_of_products <- list(100, 100)
#' macro_percent <- macronutrients_of_meal(list_of_products, weight_of_products)
#'
#' @export
macro_pie_chart <- function(macro_percent){

  assertthat::assert_that(is.data.frame(macro_percent),
                          msg = "macro_percent must be data frame")
  assertthat::assert_that(all(macro_percent$macro_name %in% c("protein", "fat", "carbohydrates")),
                          msg = "macro_percent must have 3 rows: 'protein', 'fat', 'carbohydrates'")
  assertthat::assert_that(all(colnames(macro_percent) %in% c("macro_name", "sum", "percent")),
                          msg = "macro_percent must have 3 columns: 'macro_name', 'sum', 'percent'")

data <- macro_percent[, c('macro_name', 'percent')]

colors <- c('rgb(208,223,247)', 'rgb(245,188,199)', 'rgb(161,240,174)')

pie_chart <-
  plotly::plot_ly(
    data,
    labels = ~ macro_name,
    values = ~ percent,
    type = 'pie',
    textposition = 'inside',
    texttemplate = ~ paste(macro_percent$macro_name, '%{percent:.2%f}'),
    insidetextfont = list(color = "#000000"),
    hoverinfo = 'text',
    text = ~ paste(macro_percent$sum, ' gram'),
    marker = list(colors = colors,
                  line = list(color = '#FFFFFF', width = 1)),
    showlegend = FALSE
  )

pie_chart <-
  pie_chart %>% plotly::layout(
    title = 'The percentage of macronutrients in the prepared meal',
    xaxis = list(
      showgrid = FALSE,
      zeroline = FALSE,
      showticklabels = FALSE
    ),
    yaxis = list(
      showgrid = FALSE,
      zeroline = FALSE,
      showticklabels = FALSE
    )
  )

pie_chart
}
