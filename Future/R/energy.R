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
   pr <- protein * 4
   f <- fat * 9
   c <- carbohydrates * 4
   fi <- fiber * -2
   sum(pr, f, c, fi)
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
   energy_table <- read.table(system.file("Zeszyt1.tsv", package = "Future"), sep = "\t", header = T)
   total_product_info <- energy_table %>% filter(Nazwa %in% product)
   energy <- energy_total(weight_of_product = weight,
                          protein = as.numeric(total_product_info$Białko..g.),
                          fat = as.numeric(total_product_info$Tłuszcz..g.),
                          carbohydrates = as.numeric(total_product_info$Węglowodany..g.),
                          fiber = 0)

   return(energy)
}
