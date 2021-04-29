#' @title Energy value of the product per 100 grams
#'
#' @description Function for calculating the energy value of a product per 100
#' grams
#'
#' @param protein numeric, protein content in the product for which we calculate
#' the energy value expressed in grams
#' @param fat numeric, fat content in the product for which we calculate the
#' energy value expressed in grams
#' @param carbohydrates numeric, carbohydrates content in the product for which
#' we calculate the energy value expressed in grams
#' @param fiber numeric, fiber content in the product for which we calculate
#' the energy value expressed in grams, 0 by default
#'
#' @examples
#' energy_value_of_the_product_per_100(2.7, 1.5, 16)
#' energy_value_of_the_product_per_100(2.7, 1.5, 16, 2)
#'
#' @export
energy_value_of_the_product_per_100 <- function(protein, fat, carbohydrates, fiber = 0) {

   assertthat::assert_that(assertthat::is.number(protein),
                           msg ="protein must be number")
   assertthat::assert_that(assertthat::is.number(fat),
                           msg ="fat must be number")
   assertthat::assert_that(assertthat::is.number(carbohydrates),
                           msg ="carbohydrates must be number")
   assertthat::assert_that(assertthat::is.number(fiber),
                           msg ="fiber must be number")

   protein_energy <- protein * 4
   fat_energy <- fat * 9
   carbohydrates_energy <- carbohydrates * 4
   fiber_energy <- fiber * -2
   sum(protein_energy, fat_energy, carbohydrates_energy, fiber_energy)
}

#' @title Total energy value of the product
#'
#' @description Function for calculating the total energy value of a product
#'
#' @param weight_of_product numeric, weight of used product
#' @param protein numeric, protein content in the product for which we calculate
#' the energy value expressed in grams
#' @param fat numeric, fat content in the product for which we calculate the
#' energy value expressed in grams
#' @param carbohydrates numeric, carbohydrates content in the product for which
#' we calculate the energy value expressed in grams
#' @param fiber numeric, fiber content in the product for which we calculate
#' the energy value expressed in grams, 0 by default
#'
#' @examples
#' energy_total(50, 1.2, 0.9, 11.8, 3)
#' energy_total(35, 8, 32, 52)
#'
#'
#' @export
energy_total <- function(weight_of_product, protein, fat, carbohydrates, fiber = 0) {

   assertthat::assert_that(assertthat::is.number(weight_of_product),
                           msg ="weight_of_product must be number")
   assertthat::assert_that(assertthat::is.number(protein),
                           msg ="protein must be number")
   assertthat::assert_that(assertthat::is.number(fat),
                           msg ="fat must be number")
   assertthat::assert_that(assertthat::is.number(carbohydrates),
                           msg ="carbohydrates must be number")
   assertthat::assert_that(assertthat::is.number(fiber),
                           msg ="fiber must be number")

   energy_per_100 <- energy_value_of_the_product_per_100(protein, fat, carbohydrates, fiber)
   energy_per_100 * weight_of_product / 100
}

#' @title Energy value of the product for the given weight
#'
#' @description Function for calculating the energy value of a product for given weight
#'
#' @param product character, name of used product
#' @param weight numeric, weight of the used product expressed in grams
#'
#' @examples
#' energy_of_product("Kawior", 50)
#'
#' @export
energy_of_product <- function(product, weight){

   assertthat::assert_that(assertthat::is.string(product),
                           msg = "product must be string")
   assertthat::assert_that(assertthat::is.number(weight),
                           msg ="weight must be number")

   total_product_info <- read.table(system.file("caloric_table.txt", package = "Future"), sep = ";", header = T) %>%
      dplyr::rename("Protein" = "Bialko", "Fat" = "Tluszcz", "Carbohydrates" = "Weglowodany") %>%
      dplyr::filter(Nazwa %in% product)
   energy <- energy_total(weight_of_product = weight,
                          protein = as.numeric(total_product_info$Protein),
                          fat = as.numeric(total_product_info$Fat),
                          carbohydrates = as.numeric(total_product_info$Carbohydrates),
                          fiber = 0)

   return(energy)
}

#' @title Total energy value of the meal
#'
#' @description Function for calculating the total energy value of a meal
#'
#' @param list_of_products list, list of the names of the products added to the
#' preparation of the meal
#' @param weight_of_products list, list of weights of the products added to the
#' preparation of the meal
#'
#' @examples
#' list_of_products <- list("Kawior", "Kawior")
#' weight_of_products <- list(50, 100)
#' energy_of_meal(list_of_products, weight_of_products)
#'
#' @export
energy_of_meal <- function(list_of_products, weight_of_products){

   assertthat::assert_that(is.list(list_of_products),
                           msg = "list_of_products must be list")
   assertthat::assert_that(all(sapply(list_of_products, class) == "character"),
                           msg = "elements of list_of_products must be character")
   assertthat::assert_that(is.list(weight_of_products),
                           msg = "weight_of_products must be list")
   assertthat::assert_that(all(sapply(weight_of_products, class) == "numeric"),
                           msg ="elements of weight_of_products must be numeric")
   assertthat::assert_that(length(list_of_products) == length(weight_of_products),
                           msg = "list_of_products and weight_of_products must be the same length")

   whole_energy <- 0
   for (i in 1:length(list_of_products)) {
      energy_of_i <- energy_of_product(list_of_products[[i]], weight_of_products[[i]])
      whole_energy <- sum(whole_energy, energy_of_i)
   }
   return(whole_energy)
}
