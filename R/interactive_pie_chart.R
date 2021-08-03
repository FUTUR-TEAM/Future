#' @title Interactive pie chart of percent of macronutrients in the prepared meal
#'
#' @description Function for creating interactive pie chart of percent of macronutrients in prepared meal
#'
#' @param macro_percent data frame, data frame of amount of macronutrients used to prepare the meal
#'
#' @examples
#' list_of_products <- list("Banan", "Truskawki")
#' weight_of_products <- list(100, 100)
#' macronutrients_of_meal(list_of_products, weight_of_products)
#'
#' @import plotly
#' @export
macro_pie_chart <- function(macro_percent){

macro_percent <- data.frame("Categorie" = rownames(macro_percent), macro_percent)
data <- macro_percent[, c('macro_name', 'percent')]

colors <- c('rgb(208,223,247)', 'rgb(245,188,199)', 'rgb(161,240,174)')

# TODO: ustawic zaokraglenie na stale *,00
pie_chart <- plot_ly(data, labels = ~macro_name, values = ~percent, type = 'pie',
                     textposition = 'inside',
                     textinfo = 'label+percent',
                     insidetextfont = list(color = "#000000"),
                     hoverinfo = 'text',
                     text = ~paste(macro_percent$sum, ' gram'),
                     marker = list(colors = colors,
                                   line = list(color = '#FFFFFF', width = 1)),
                     showlegend = FALSE)
pie_chart <- pie_chart %>% layout(title = 'The percentage of macronutrients in the prepared meal',
                                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

pie_chart
}

# macro_percent_graph <- function(macronutrients_df){
#   # Compute percentages
#   macronutrients_df$fraction = macronutrients_df$sum / sum(macronutrients_df$sum)
#
#   # Compute the cumulative percentages (top of each rectangle)
#   macronutrients_df$ymax = cumsum(macronutrients_df$fraction)
#
#   # Compute the bottom of each rectangle
#   macronutrients_df$ymin = c(0, utils::head(macronutrients_df$ymax, n=-1))
#
#   # Compute label position
#   macronutrients_df$labelPosition <- (macronutrients_df$ymax + macronutrients_df$ymin) / 2
#
#   # Compute a good label
#
#   macronutrients_df$label <- paste0(macronutrients_df$sum, "\n value: ", macronutrients_df$percent)
#
#   # Make the plot
#   plot <- ggplot(macronutrients_df, aes(ymax=.data$ymax, ymin=.data$ymin, xmax=4, xmin=3, fill=.data$macro_name)) +
#     geom_rect() +
#     geom_label( x=3.5, aes(y=.data$labelPosition, label=.data$label), size=5, parse = TRUE) +
#     scale_fill_brewer(palette=4) +
#     coord_polar(theta="y") +
#     xlim(c(2, 4)) +
#     theme_void()
#
#   plot
# }
