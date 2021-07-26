testthat::test_that(desc = "macronutrients works!", {

  result <- macronutrients("Kawior", 50)
  testthat::expect_equal(length(result), 5)
  testthat::expect_equal(object = result$product_name, expected = "Kawior")
  testthat::expect_equal(object = result$weight_of_product, expected = 50)
  testthat::expect_equal(object = result$protein, expected = 12)
  testthat::expect_equal(object = result$fat, expected = 9)
  testthat::expect_equal(object = result$carbohydrates, expected = 2)
})

testthat::test_that(desc = "macronutreints has expected error messeges", {

  testthat::expect_error(macronutrients(25, 50), "product must be string")
  testthat::expect_error(macronutrients("Kawior", "50"), "weight must be number")
})



testthat::test_that(desc = "macronutrients_of_meal works!", {

  list_of_products <- list("Kawior", "Kawior")
  weight_of_products <- list(50, 100)
  result <- macronutrients_of_meal(list_of_products, weight_of_products)

  testthat::expect_equal(nrow(result), 3)
  testthat::expect_equal(object = result$macro_name, expected = c("protein", "fat", "carbohydrates"))
  testthat::expect_equal(object = result$sum, expected = c(36, 27, 6))
  testthat::expect_equal(object = result$percent, expected = c(52.17, 39.13, 8.70))
})

testthat::test_that(desc = "macronutreints_of_meal has expected error messeges", {

  list_of_products <- c("Kawior")
  weight_of_products <- list(50, 100)
  testthat::expect_error(
    macronutrients_of_meal(list_of_products, weight_of_products),
    "list_of_products must be list")

  list_of_products <- list("Kawior", "Kawior")
  weight_of_products <- c(50, 100)
  testthat::expect_error(
    macronutrients_of_meal(list_of_products, weight_of_products),
    "weight_of_products must be list")

  list_of_products <- list("Kawior", 50)
  weight_of_products <- list(50, 100)
  testthat::expect_error(
    macronutrients_of_meal(list_of_products, weight_of_products),
    "elements of list_of_products must be character")

  list_of_products <- list("Kawior", "Kawior")
  weight_of_products <- list(50, "100")
  testthat::expect_error(
    macronutrients_of_meal(list_of_products, weight_of_products),
    "elements of weight_of_products must be numeric")

  list_of_products <- list("Kawior", "Kawior")
  weight_of_products <- list(50, 100, 100)
  testthat::expect_error(
    macronutrients_of_meal(list_of_products, weight_of_products),
    "list_of_products and weight_of_products must be the same length")
})
