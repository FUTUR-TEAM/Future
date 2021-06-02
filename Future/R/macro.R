#' @title Amount of macronutrients in product
#'
#' @description Function for calculating amount of macronutrients in product
#'
#' @param product string, name of used product
#' @param weight numeric, weight of used product
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' macronutrients("Kawior", 50)
#'
#' @export
macronutrients <- function(product, weight){

  assertthat::assert_that(assertthat::is.string(product),
                          msg = "product must be string")
  assertthat::assert_that(assertthat::is.number(weight),
                          msg = "weight must be number")

  product_info <- utils::read.table(system.file("caloric_table.txt", package = "Future"), sep = ";", header = T) %>%
    dplyr::rename("Protein" = "Bialko", "Fat" = "Tluszcz", "Carbohydrates" = "Weglowodany") %>%
    dplyr::filter(Nazwa %in% product)
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
#' @description Function for calculating amount of macronutrients in prepared meal
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
  sum_protein <- colSums(df_macro[3])
  sum_fat <- colSums(df_macro[4])
  sum_carbohydrates <- colSums(df_macro[5])
  macro_name <- data.frame(macro_name = c("protein", "fat", "carbohydrates"),
                          sum = c(sum_protein, sum_fat, sum_carbohydrates))

  percent_protein <- round(sum_protein/colSums(macro_name[2])*100, digits = 2)
  percent_fat <- round(sum_fat/colSums(macro_name[2])*100, digits = 2)
  percent_carbohydrates <- round(sum_carbohydrates/colSums(macro_name[2])*100, digits = 2)

  total_macro_df <-cbind(macro_name, percent = c(percent_protein, percent_fat, percent_carbohydrates))
  rownames(total_macro_df) <- NULL

  total_macro_df
}

