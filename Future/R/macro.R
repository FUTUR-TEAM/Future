#' @title Amount of macronutrients in product
#'
#' @description Function for calculating amount of macronutrients in product
#'
#' @param product string, name of used product
#' @param weight numeric, weight of used product
#'
#' @examples
#' macronutrients("Kawior", 50)
#'
#' @export
macronutrients <- function(product, weight){

  assertthat::assert_that(assertthat::is.string(product),
                          msg = "product must be string")
  assertthat::assert_that(assertthat::is.number(weight),
                          msg = "weight must be numeric")

  product_info <- read.table(system.file("caloric_table.txt", package = "Future"), sep = ";", header = T) %>%
    rename("Protein" = "Bialko", "Fat" = "Tluszcz", "Carbohydrates" = "Weglowodany") %>%
    filter(Nazwa %in% product)
  macro_info <- list(product_name = product,
                     weight_of_product = weight,
                     protein = as.numeric(product_info$Protein) * weight / 100,
                     fat = as.numeric(product_info$Fat) * weight / 100,
                     carbohydrates = as.numeric(product_info$Carbohydrates) * weight / 100
  )
  return(macro_info)
}

#' @title Amount of macronutrients in the prepared meal
#'
#' @description Function for calculating amount of macronutrients in prepard meal
#'
#' @param list_of_products list, list of the names of the products used to prepare the meal
#' @param weight_of_products list, list of weights of the products used to prepare the meal
#'
#' @examples
#' list_of_products <- list("Kawior", "Kawior")
#' weight_of_products <- list(50, 100)
#' macronutrients_of_meal(list_of_products, weight_of_products)
#'
#' @export
macronutrients_of_meal <- function(list_of_products, weight_of_products){

  assertthat::assert_that(is.list(list_of_products),
                          msg = "list_of_products must be list")
  assertthat::assert_that(all(sapply(list_of_products, class) == "character"),
                          msg = "elements of list_of_products must be character")
  assertthat::assert_that(is.list(weight_of_products),
                          msg = "weight_of_products must be list")
  assertthat::assert_that(all(sapply(weight_of_products, class) == "numeric"),
                          msg = "elements of weight_of_products must be numeric")
  assertthat::assert_that(length(list_of_products) == length(weight_of_products),
                          msg = "list_of_products and weight_of_products must be the same length")

  df_macro <- data.frame()
  for (i in 1:length(list_of_products)) {
    macro <- macronutrients(list_of_products[[i]], weight_of_products[[i]])
    df_macro <- rbind(df_macro, macro)
  }
  return(df_macro)
}
