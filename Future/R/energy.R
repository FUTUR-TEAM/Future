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

energy_of_meal <- function(list_of_products, weight_of_products){
   whole_energy <- 0
   for (i in 1:length(list_of_products)) {
      energy_of_i <- energy_of_product(list_of_products[[i]], weight_of_products[[i]])
      whole_energy <- sum(whole_energy, energy_of_i)
   }
   return(whole_energy)
}


makronutrients <- function(product, weight){
   if (!is.character(product)){
      stop("product must be string")
   }
   if (!is.numeric(weight)){
      stop("weight must be numeric")
   }

   product_info <- read.table(system.file("caloric_table.txt", package = "Future"), sep = ";", header = T) %>%
      rename("Protein" = "Bialko", "Fat" = "Tluszcz", "Carbohydrates" = "Weglowodany") %>%
      filter(Nazwa %in% product)
   makro_info <- list(weight_of_product = weight,
                      protein = as.numeric(product_info$Protein) * weight / 100,
                      fat = as.numeric(product_info$Fat) * weight / 100,
                      carbohydrates = as.numeric(product_info$Carbohydrates) * weight / 100
   )
   return(makro_info)
}

makronutrients_of_meal <- function(list_of_products, weight_of_products){
   df_makro <- data.frame()
   for (i in 1:length(list_of_products)) {
      makro <- makronutrients(list_of_products[[i]], weight_of_products[[i]])
      df_makro <- rbind(df_makro, makro)
   }
   return(list(
      protein = sum(df_makro$protein),
      fat = sum(df_makro$fat),
      carbohydrates = sum(df_makro$carbohydrates)
      ))
}
