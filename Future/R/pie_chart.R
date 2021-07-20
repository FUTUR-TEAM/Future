#' @title Pie chart of percent of macronutrients in the prepared meal
#'
#' @description Function for creating pie chart of percent of macronutrients in prepared meal
#'
#' @param macronutrients_df data frame, data frame of amount of macronutrients used to prepare the meal
#'
#' @examples
#' list_of_products <- list("Kawior", "Agrest")
#' weight_of_products <- list(50, 100)
#' example_df <- macronutrients_of_meal(list_of_products, weight_of_products)
#' macro_percent_graph(example_df)
#'
#' @import ggplot2
#' @export
macro_percent_graph <- function(macronutrients_df){
  # Compute percentages
  macronutrients_df$fraction = macronutrients_df$sum / sum(macronutrients_df$sum)

  # Compute the cumulative percentages (top of each rectangle)
  macronutrients_df$ymax = cumsum(macronutrients_df$fraction)

  # Compute the bottom of each rectangle
  macronutrients_df$ymin = c(0, head(macronutrients_df$ymax, n=-1))

  # Compute label position
  macronutrients_df$labelPosition <- (macronutrients_df$ymax + macronutrients_df$ymin) / 2

  # Compute a good label
  macronutrients_df$label <- paste0(macronutrients_df$macro_name, "\n value: ", macronutrients_df$percent)

  # Make the plot
  plot <- ggplot(macronutrients_df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=macro_name)) +
    geom_rect() +
    geom_label( x=3.5, aes(y=labelPosition, label=label), size=5, parse = TRUE) +
    scale_fill_brewer(palette=4) +
    coord_polar(theta="y") +
    xlim(c(2, 4)) +
    theme_void()

  plot
}
