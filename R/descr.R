#' @title Basic descriptive statistics
#' @name descr
#'
#' @description This function wraps the \code{\link[psych]{describe}}-function
#'              and prints a basic descriptive statistic, including variable labels.
#'
#' @param .data A vector or a data frame.
#' @inheritParams flat_table
#'
#' @return A data frame with basic descriptive statistics, derived from the
#'         \code{\link[psych]{describe}}-function.
#'
#' @examples
#' data(efc)
#' descr(efc, e17age, c160age)
#'
#' library(dplyr)
#' efc %>% select(e42dep, e15relat, c172code) %>% descr()
#'
#' @importFrom tibble as_tibble rownames_to_column
#' @importFrom dplyr select mutate
#' @importFrom psych describe
#' @export
descr <- function(.data, ...) {

  # get dot data
  dd <- get_dot_data(.data, match.call(expand.dots = FALSE)$`...`)

  # call psych::describe and convert to tibble, remove some unnecessary
  # columns and and a variable label column
  x <- tibble::as_tibble(psych::describe(dd)) %>%
    tibble::rownames_to_column(var = "variable") %>%
    dplyr::select_("-vars", "-mad") %>%
    dplyr::mutate(label = unname(get_label(dd, def.value = colnames(dd)))) %>%
    var_rename(median = "md")

  # sort columns a bit
  x <- x[, c(1, 13, 2, 3, 4, 12, 5, 6, 7, 8, 9, 10, 11)]

  # add own class for print-method
  class(x) <- c("sjmisc.descr", "data.frame")
  x
}