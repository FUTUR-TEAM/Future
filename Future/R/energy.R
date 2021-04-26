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
   if (!is.numeric(protein)){
      stop("protein must be numeric")
   }
   if (!is.numeric(fat)){
      stop("fat must be numeric")
   }
   if (!is.numeric(carbohydrates)){
      stop("carbohydrates must be numeric")
   }
   if (!is.numeric(fiber)){
      stop("fiber must be numeric")
   }
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
   if (!is.numeric(weight_of_product)){
      stop("weight_of_product must be numeric")
   }
   if (!is.numeric(protein)){
      stop("protein must be numeric")
   }
   if (!is.numeric(fat)){
      stop("fat must be numeric")
   }
   if (!is.numeric(carbohydrates)){
      stop("carbohydrates must be numeric")
   }
   if (!is.numeric(fiber)){
      stop("fiber must be numeric")
   }
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
   if (!is.character(product)){
      stop("product must be string")
   }
   if (!is.numeric(weight)){
      stop("weight must be numeric")
   }

   total_product_info <- read.table(system.file("caloric_table.txt", package = "Future"), sep = ";", header = T) %>%
      rename("Protein" = "Bialko", "Fat" = "Tluszcz", "Carbohydrates" = "Weglowodany") %>%
      filter(Nazwa %in% product)
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

   if (!is.list(list_of_products)){
      stop("list_of_products must be list")
   }
   if (!all(sapply(list_of_products, class) == "character")){
      stop("elements of list_of_products must be character")
   }
   if (!is.list(weight_of_products)){
      stop("weight_of_products must be list")
   }
   if (!all(sapply(weight_of_products, class) == "numeric")){
      stop("elements of weight_of_products must be numeric")
   }
   if (length(list_of_products) != length(weight_of_products)){
      stop("list_of_products and weight_of_products must be the same length")
   }

   whole_energy <- 0
   for (i in 1:length(list_of_products)) {
      energy_of_i <- energy_of_product(list_of_products[[i]], weight_of_products[[i]])
      whole_energy <- sum(whole_energy, energy_of_i)
   }
   return(whole_energy)
}

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

   checkmate::assert_list(list_of_products, min.len = 1)
   checkmate::assert_true(all(sapply(list_of_products, class) == "character"))

   checkmate::assert_list(weight_of_products, min.len = 1)
   checkmate::assert_true(all(sapply(weight_of_products, class) == "numeric"))

   checkmate::assert_set_equal(length(list_of_products), length(weight_of_products))

   df_macro <- data.frame()
   for (i in 1:length(list_of_products)) {
      macro <- macronutrients(list_of_products[[i]], weight_of_products[[i]])
      df_macro <- rbind(df_macro, macro)
   }
   return(df_macro)
}
