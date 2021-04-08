#' @title BMI
#'
#' @description Funkcja pomocnicza do liczenia wskaźnika BMI.
#'
#' @param waga numeric, waga osoby, dla której liczymy BMI wyrażona w kg
#' @param wzrost numeric, wzrost osoby, dla której liczymy BMI wyrażony w metrach
#' @param plec string, plec osoby, dla której liczymy BMI. Możliwe wartości dla parametru: \code{kobieta}, \code{mezczyzna}
#'
#' @examples
#' BMI(100, 1.7)
#' BMI(85, 1.9, "kobieta")
#'
#' @export
BMI <- function(waga, wzrost, plec = c("kobieta", "mezczyzna")) {
  if (!is.numeric(waga)) {
    stop("waga musi byc wartosciaa liczbowa")
  }
  if (!is.numeric(wzrost)) {
    stop("wzrost musi byc wartosciaa liczbowa")
  }

  plec <- match.arg(plec)

  # if (!(is.character(plec) || is.null(plec))) {
  #   stop("wpisz kobieta lub mezczyzna")
  # }

  BMI <- waga / wzrost^2

  sylwetka <- if (BMI < 18.5) {
    "niedowaga"
  } else if (BMI < 25) {
    "optimum"
  } else if (BMI < 30){
    "nadwaga"
  } else if (BMI >= 30){
    "otyłosc"
  }

  return(list(BMI = BMI, sylwetka = sylwetka))
}

