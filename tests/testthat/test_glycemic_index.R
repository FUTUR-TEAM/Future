testthat::test_that(desc = "glycemic_index works!", {

  result <- glycemic_index("Banan", 100)
  testthat::expect_equal(length(result), 5)
  testthat::expect_equal(object = result$name, expected = "Banan")
  testthat::expect_equal(object = result$weight, expected = 100)
  testthat::expect_equal(object = result$carbo, expected = 20)
  testthat::expect_equal(object = result$digestible_carbo, expected = 18.3)
  testthat::expect_equal(object = result$IG, expected = 60)
})

testthat::test_that(desc = "glycemic_index has expected error messeges", {

  testthat::expect_error(glycemic_index(25, 50), "product must be string")
  testthat::expect_error(glycemic_index("Banan", "100"), "weight must be number")
})


testthat::test_that(desc = "glycemic_index_of_meal works!", {

  list_of_products <- list("Banan", "Truskawki")
  weight_of_products <- list(100, 100)
  result <- glycemic_index_of_meal(list_of_products, weight_of_products)

  testthat::expect_equal(result, 52.03)
})

testthat::test_that(desc = "glycemic_index_of_meal has expected error messeges", {

  list_of_products <- c("Banan")
  weight_of_products <- list(100, 100)
  testthat::expect_error(
    glycemic_index_of_meal(list_of_products, weight_of_products),
    "list_of_products must be list")

  list_of_products <- list("Banan", "Truskawki")
  weight_of_products <- c(50, 100)
  testthat::expect_error(
    glycemic_index_of_meal(list_of_products, weight_of_products),
    "weight_of_products must be list")

  list_of_products <- list("Banan", 50)
  weight_of_products <- list(50, 100)
  testthat::expect_error(
    glycemic_index_of_meal(list_of_products, weight_of_products),
    "elements of list_of_products must be character")

  list_of_products <- list("Banan", "Truskawki")
  weight_of_products <- list(50, "100")
  testthat::expect_error(
    glycemic_index_of_meal(list_of_products, weight_of_products),
    "elements of weight_of_products must be numeric")

  list_of_products <- list("Banan", "Truskawki")
  weight_of_products <- list(50, 100, 100)
  testthat::expect_error(
    glycemic_index_of_meal(list_of_products, weight_of_products),
    "list_of_products and weight_of_products must be the same length")
})
