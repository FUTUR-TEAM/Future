% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/macro.R
\name{macronutrients_of_meal}
\alias{macronutrients_of_meal}
\title{Amount of macronutrients in the prepared meal}
\usage{
macronutrients_of_meal(list_of_products, weight_of_products)
}
\arguments{
\item{list_of_products}{list, list of the names of the products used to prepare the meal}

\item{weight_of_products}{list, list of weights of the products used to prepare the meal}
}
\description{
Function for calculating amount of macronutrients in prepared meal
}
\examples{
list_of_products <- list("Kawior", "Kawior")
weight_of_products <- list(50, 100)
macronutrients_of_meal(list_of_products, weight_of_products)

}
