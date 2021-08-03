#' @title BMI
#'
#' @description Function for calculating BMI.
#'
#' @param weight numeric, weight of the person for whom we calculate the BMI
#' expressed in kg
#' @param growth numeric, growth of the person for whom we calculate the BMI
#' expressed in meters
#' @param sex string, sex of the person for whom we calculate BMI. Possible
#' values for the parameter: \code{female}, \code{male}
#'
#' @examples
#' BMI(100, 1.7)
#' BMI(85, 1.9, "female")
#'
#' @export
BMI <- function(weight, growth, sex = c("female", "male")) {
  assertthat::assert_that(is.numeric(weight), msg = "weight must be numeric")
  assertthat::assert_that(is.numeric(growth), msg = "growth must be numeric")

  sex <- match.arg(sex)

  BMI <- weight / growth^2

  figure <- if (BMI < 18.5) {
    "underweight"
  } else if (BMI < 25) {
    "normal"
  } else if (BMI < 30){
    "overweight"
  } else if (BMI >= 30){
    "obese"
  }

  return(list(BMI = BMI, figure = figure))
}

