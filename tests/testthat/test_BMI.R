
testthat::test_that(desc = "BMI works", {

  result <- BMI(100,2)
  expect <- 100/2^2
  testthat::expect_equal(result$BMI, expect)

  result <- BMI(50,2)
  testthat::expect_equal(result$figure, "underweight")

  result <- BMI(50,1.5)
  testthat::expect_equal(result$figure, "normal")

  result <- BMI(60,1.5)
  testthat::expect_equal(result$figure, "overweight")

  result <- BMI(70,1.5)
  testthat::expect_equal(result$figure, "obese")

})

testthat::test_that(desc = "BMI has expected error messeges", {

  testthat::expect_error(BMI(100,"2"), "growth must be numeric")
  testthat::expect_error(BMI("100", 2), "weight must be numeric")
  testthat::expect_error(BMI(100, 2, 3), "'arg' must be NULL or a character vector")

})

testthat::test_that(desc = "check sex", {

  result <- BMI(50, 2, "female")
  testthat::expect_equal(result$figure, "underweight")

})
