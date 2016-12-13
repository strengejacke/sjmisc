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
#' library(dplyr)
#' data(efc)
#' efc %>% select(e42dep, e15relat, c172code) %>% descr()
#'
#' @importFrom tibble as_tibble rownames_to_column
#' @importFrom dplyr select mutate
#' @importFrom psych describe
#' @export
descr <- function(.data, ...) {

  # get dot data
  .data <- get_dot_data(.data, match.call(expand.dots = FALSE)$`...`)

  x <- tibble::as_tibble(psych::describe(.data)) %>%
    tibble::rownames_to_column(var = "variable") %>%
    dplyr::select_("-vars", "-mad") %>%
    dplyr::mutate(label = unname(get_label(.data, def.value = colnames(.data)))) %>%
    var_rename(median = "md")

  x <- x[, c(1, 13, 2, 3, 4, 12, 5, 6, 7, 8, 9, 10, 11)]
  class(x) <- c("sjmisc.descr", "data.frame")
  x
}