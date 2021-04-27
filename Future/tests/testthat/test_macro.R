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
  testthat::expect_error(macronutrients("Kawior", "50"), "weight must be numeric")
})



testthat::test_that(desc = "macronutrients_of_meal works!", {

  list_of_products <- list("Kawior", "Kawior")
  weight_of_products <- list(50, 100)
  result <- macronutrients_of_meal(list_of_products, weight_of_products)

  testthat::expect_identical(length(list_of_products), length(weight_of_products))
  testthat::expect_equal(length(result), 5)
  testthat::expect_equal(object = result$product_name, expected = c("Kawior", "Kawior"))
  testthat::expect_equal(object = result$weight_of_product, expected = c(50, 100))
  testthat::expect_equal(object = result$protein, expected = c(12, 24))
  testthat::expect_equal(object = result$fat, expected = c(9, 18))
  testthat::expect_equal(object = result$carbohydrates, expected = c(2, 4))
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
