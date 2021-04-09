
testthat::test_that(desc = "BMI dziaLa", {

  result <- BMI(100,2)
  expect <- 100/2^2
  testthat::expect_equal(result$BMI, expect)

  result <- BMI(50,2)
  testthat::expect_equal(result$sylwetka, "niedowaga")

  result <- BMI(50,1.5)
  testthat::expect_equal(result$sylwetka, "optimum")

  result <- BMI(60,1.5)
  testthat::expect_equal(result$sylwetka, "nadwaga")

  result <- BMI(70,1.5)
  testthat::expect_equal(result$sylwetka, "otyłosc")

})

testthat::test_that(desc = "BMI siE Ladnie wzszpuje", {

  testthat::expect_error(BMI(100,"2"), "wzrost musi byc wartosciaa liczbowa")
  testthat::expect_error(BMI("100", 2), "waga musi byc wartosciaa liczbowa")
  testthat::expect_error(BMI(100, 2, 3), "'arg' must be NULL or a character vector")

})

testthat::test_that(desc = "sprawdza plec", {

  result <- BMI(50, 2, "kobieta")
  testthat::expect_equal(result$sylwetka, "niedowaga")

})
