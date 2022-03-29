#' @title Load table
#'
#' @description Function for loading table from file or database
#'
#' @param table_name string, name of table
#' @param source string, file or db
#'
#' @import RSQLite
#' @import DBI
#'
#' @examples
#' load_data("caloric_table", "db")
#'
#' @export
load_table <- function(table_name, source){

  assertthat::assert_that(assertthat::is.string(table_name),
                          msg = "table_name must be string")
  assertthat::assert_that(assertthat::is.string(source),
                          msg = "source must be string")

  match.arg(source, c("file", "db"))

  table <- if (source == "file") {
    utils::read.table(system.file(paste0(table_name, ".txt"), package = "Future"), sep = ";", header = T)

  } else if (source == "db") {
    con <- DBI::dbConnect(RSQLite::SQLite(), system.file("meta_info.db", package = "Future"))

    out <- DBI::dbReadTable(con, table_name)
    DBI::dbDisconnect(con)
    out
  }

   table
}

#' @title Load data
#'
#' @description Function for loading data from file or database
#'
#' @param table_name string, name of table
#' @param source string, file or db
#' @param selected_columns character, names of selected columns
#' @param product character, name of selected product
#'
#' @import DBI
#' @import dplyr
#' @import RSQLite
#'
#' @examples
#' load_data("caloric_table", "db", c("protein", "fat", "carbohydrates"), "Banan")
#'
#' @export
load_data <- function(table_name, source, selected_columns, product){

  assertthat::assert_that(assertthat::is.string(table_name),
                          msg = "table_name must be string")
  assertthat::assert_that(assertthat::is.string(source),
                          msg = "source must be string")
  assertthat::assert_that(is.character(selected_columns),
                          msg = "selected_columns must be character")
  assertthat::assert_that(is.character(product),
                          msg = "product must be character")

  match.arg(source, c("file", "db"))

  table <- if (source == "file") {
    utils::read.table(system.file(paste0(table_name, ".txt"), package = "Future"), sep = ";", header = T) %>%
      dplyr::filter(.data$name %in% product) %>%
      dplyr::select(selected_columns)

  } else if (source == "db") {
    con <- DBI::dbConnect(RSQLite::SQLite(), system.file("meta_info.db", package = "Future"))

    energy <- DBI::dbGetQuery(con,
                              sprintf(
                                'SELECT %s
                                    FROM %s
                                    WHERE "name" = "%s"
                                    ',
                                paste(selected_columns, collapse = ", "),
                                paste(table_name),
                                product
                              ))
    DBI::dbDisconnect(con)

    energy
  }

  table
}
