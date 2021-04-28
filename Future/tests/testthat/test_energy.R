testthat::test_that(desc = "energy_value_of_the_product_per_100 works!", {

  result <- energy_value_of_the_product_per_100(2.7, 1.5, 16)
  expect <- 88.3
  testthat::expect_equal(result, expect)

  result <- energy_value_of_the_product_per_100(2.7, 1.5, 16, 2)
  testthat::expect_equal(result, 84.3)
})


testthat::test_that(desc = "energy_value_of_the_product_per_100 has expected error messeges", {

  testthat::expect_error(energy_value_of_the_product_per_100("2.7", 1.5, 16), "protein must be number")
  testthat::expect_error(energy_value_of_the_product_per_100(2.7, "1.5", 16), "fat must be number")
  testthat::expect_error(energy_value_of_the_product_per_100(2.7, 1.5, "16"), "carbohydrates must be number")
  testthat::expect_error(energy_value_of_the_product_per_100("2.7", 1.5, 16, 2), "protein must be number")
  testthat::expect_error(energy_value_of_the_product_per_100(2.7, "1.5", 16, 2), "fat must be number")
  testthat::expect_error(energy_value_of_the_product_per_100(2.7, 1.5, "16", 2), "carbohydrates must be number")
  testthat::expect_error(energy_value_of_the_product_per_100(2.7, 1.5, 16, "2"), "fiber must be number")
})

testthat::test_that(desc = "energy_total works!", {

  result <- energy_total(50, 1.2, 0.9, 11.8, 3)
  testthat::expect_equal(result, 27.05)

  result <- energy_total(50, 1.2, 0.9, 11.8)
  testthat::expect_equal(result, 30.05)

})

testthat::test_that(desc = "energy_total has expected error messeges", {

  testthat::expect_error(energy_total("50", 1.2, 0.9, 11.8), "weight_of_product must be number")
  testthat::expect_error(energy_total(50, "1.2", 0.9, 11.8), "protein must be number")
  testthat::expect_error(energy_total(50, 1.2, "0.9", 11.8), "fat must be number")
  testthat::expect_error(energy_total(50, 1.2, 0.9, "11.8"), "carbohydrates must be number")
  testthat::expect_error(energy_total("50", 1.2, 0.9, 11.8, 3), "weight_of_product must be number")
  testthat::expect_error(energy_total(50, "1.2", 0.9, 11.8, 3), "protein must be number")
  testthat::expect_error(energy_total(50, 1.2, "0.9", 11.8, 3), "fat must be number")
  testthat::expect_error(energy_total(50, 1.2, 0.9, "11.8", 3), "carbohydrates must be number")
  testthat::expect_error(energy_total(50, 1.2, 0.9, 11.8, "3"), "fiber must be number")
})

testthat::test_that(desc = "energy_of_product works!", {

  result <- energy_of_product("Kawior", 50)
  testthat::expect_equal(result, 137)
})

testthat::test_that(desc = "energy_of_product has expected error messeges", {

  testthat::expect_error(energy_of_product(25, 50), "product must be string")
  testthat::expect_error(energy_of_product("Kawior", "50"), "weight must be number")
})

testthat::test_that(desc = "energy_of_meal works!", {

  list_of_products <- list("Kawior", "Kawior")
  weight_of_products <- list(50, 100)
  result <- energy_of_meal(list_of_products, weight_of_products)

  testthat::expect_identical(length(list_of_products), length(weight_of_products))
  testthat::expect_equal(length(result), 1)
  testthat::expect_equal(result, 411)
})

testthat::test_that(desc = "energy_of_meal has expected error messeges", {

  list_of_products <- c("Kawior")
  weight_of_products <- list(50, 100)
  testthat::expect_error(
    energy_of_meal(list_of_products, weight_of_products),
    "list_of_products must be list")

  list_of_products <- list("Kawior", 50)
  weight_of_products <- list(50, 100)
  testthat::expect_error(
    energy_of_meal(list_of_products, weight_of_products),
    "elements of list_of_products must be character")

  list_of_products <- list("Kawior", "Kawior")
  weight_of_products <- c(50, 100)
  testthat::expect_error(
    energy_of_meal(list_of_products, weight_of_products),
    "weight_of_products must be list")

  list_of_products <- list("Kawior", "Kawior")
  weight_of_products <- list(50, "100")
  testthat::expect_error(
    energy_of_meal(list_of_products, weight_of_products),
    "elements of weight_of_products must be numeric")

  list_of_products <- list("Kawior", "Kawior")
  weight_of_products <- list(50, 100, 100)
  testthat::expect_error(
    energy_of_meal(list_of_products, weight_of_products),
    "list_of_products and weight_of_products must be the same length")
})
