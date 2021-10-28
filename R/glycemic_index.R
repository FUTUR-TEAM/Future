#' @title Glycemic index of product
#'
#' @description Function for calculating glycemic index of product in prepared meal
#'
#' @param product string, name of used product
#' @param weight numeric, weight of used product
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' glycemic_index("Banan", 100)
#'
#' @export
glycemic_index <- function(product, weight){

  assertthat::assert_that(assertthat::is.string(product),
                          msg = "product must be string")
  assertthat::assert_that(assertthat::is.number(weight),
                          msg = "weight must be number")

  glycemic_info <- utils::read.table(system.file("digestible_carbo.txt", package = "Future"), sep = ";", header = T) %>%
    dplyr::rename("name" = "nazwa", "carbohydrates"	= "weglowodany", "fiber"	= "blonnik", "digestible_carbo" =	"przyswajalne_weglo") %>%
    dplyr::filter(.data$name %in% product)

  if (NROW(glycemic_info) == 0) {
    glycemic_info[1,] <- 0
  }

  carbo_info <- list(name = product,
                     weight_of_product = weight,
                     carbohydrates = glycemic_info$carbohydrates * weight / 100,
                     digestible_carbohydrates = glycemic_info$digestible_carbo * weight / 100
  )

  glycemic_index_of_product <- utils::read.table(system.file("IG_new.txt", package = "Future"), sep = ";", header = T) %>%
    dplyr::filter(.data$nazwa %in% product)

  if (NROW(glycemic_index_of_product) == 0) {
    glycemic_index_of_product[1,] <- 0
  }

  total_carbo_info <- list(name = carbo_info$name,
                           weight = carbo_info$weight_of_product,
                           carbo = as.numeric(carbo_info$carbohydrates),
                           digestible_carbo = as.numeric(carbo_info$digestible_carbohydrates),
                           IG = as.numeric(glycemic_index_of_product$IG))

  total_carbo_info
}

#' @title Glycemic index of prepared meal
#'
#' @description Function for calculating glycemic index of prepared meal
#'
#' @param list_of_products list, list of the names of the products used to prepare the meal
#' @param weight_of_products list, list of weights of the products used to prepare the meal
#'
#' @examples
#' list_of_products <- list("Banan", "Truskawki")
#' weight_of_products <- list(100, 100)
#' glycemic_index_of_meal(list_of_products, weight_of_products)
#'
#' @export
glycemic_index_of_meal <- function(list_of_products, weight_of_products){

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
  dataframe_IG <- data.frame()
  for (i in seq_len(length(list_of_products))) {
   IG <- glycemic_index(list_of_products[[i]], weight_of_products[[i]])
     dataframe_IG <- base::rbind(dataframe_IG, IG)
  }
  # browser()

  sum_digestible_carbo <- colSums(dataframe_IG[4])
  percent_digestible_carbo <- round(dataframe_IG$digestible_carbo / sum_digestible_carbo, 4)
  dataframe_IG <- cbind(dataframe_IG, percent_digestible_carbo)

  IG_of_product <- round(dataframe_IG$percent_digestible_carbo * dataframe_IG$IG, 2)
  dataframe_IG <- cbind(dataframe_IG, IG_of_product)

  sum_IG_of_meal <- sum(dataframe_IG$IG_of_product)

  sum_IG_of_meal

  if (is.nan(sum_IG_of_meal)){
    print(0)
  }
}
