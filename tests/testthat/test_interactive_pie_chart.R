testthat::test_that(desc = "interactive pie chart works!", {

  list_of_products <- list("Banan", "Truskawki")
  weight_of_products <- list(100, 100)
  macro_percent <- macronutrients_of_meal(list_of_products, weight_of_products)
  result <- macro_pie_chart(macro_percent)

  testthat::expect_equal(class(result), c("plotly", "htmlwidget"))
  testthat::expect_equal(length(result), 8)
})

testthat::test_that(desc = "interactive pie chart has expected error messeges", {

  list_of_products <- list("Banan", "Truskawki")
  weight_of_products <- list(100, 100)
  macro_percent <- macronutrients_of_meal(list_of_products, weight_of_products)
  testthat::expect_error(macro_pie_chart(character()),
                         "macro_percent must be data frame")

  macro_percent_additional_column <- cbind(macro_percent, 1)
  testthat::expect_error(macro_pie_chart(macro_percent_additional_column),
                         "macro_percent must have 3 columns: 'macro_name', 'sum', 'percent'")

  macro_percent_additional_row <- rbind(macro_percent, 1)
  testthat::expect_error(macro_pie_chart(macro_percent_additional_row),
                         "macro_percent must have 3 rows: 'protein', 'fat', 'carbohydrates'")

  macro_percent_missing_column <- macro_percent[ ,2:3]
  testthat::expect_error(macro_pie_chart(macro_percent_missing_column),
                         "macro_percent must have 3 columns: 'macro_name', 'sum', 'percent'")

  })


