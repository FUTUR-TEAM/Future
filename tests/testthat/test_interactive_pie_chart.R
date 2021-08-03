testthat::test_that(desc = "interactive pie chart works!", {

  list_of_products <- list("Banan", "Truskawki")
  weight_of_products <- list(100, 100)
  macro_percent <- macronutrients_of_meal(list_of_products, weight_of_products)
  result <- macro_pie_chart(macro_percent)

  testthat::expect_equal(length(result), 8)
  testthat::expect_true(is.data.frame(macro_percent))
  testthat::expect_equal(object = macro_percent$sum, expected = c(1.6, 0.8, 24.2))
  testthat::expect_equal(object = macro_percent$percent, expected = c(6.02, 3.01, 90.98))
})

