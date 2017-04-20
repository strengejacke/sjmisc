#' @title Count row indices
#' @name row_count
#'
#' @description \code{row_count()} mimics base R's \code{rowSums()}, with sums
#'              for a specific value indicated by \code{count}. Hence, it is equivalent
#'              to \code{rowSums(x == count, na.rm = TRUE)}. However, this function
#'              is designed to work nicely within a pipe-workflow and allows select-helpers
#'              for selecting variables and the return value is always a tibble
#'              (with one variable).
#'
#' @param count The value for which the row sum should be computed. May be a numeric
#'          value, a character string (for factors or character vectors), \code{NA},
#'          \code{Inf} or \code{NULL} to count missing or infinite values, or
#'          null-values.
#' @param var Name of new the variable with the row or column counts.
#'
#' @inheritParams to_factor
#' @inheritParams rec
#'
#' @return A tibble with one variable: the sum of \code{count} appearing in each
#'         row of \code{x}. If \code{append = TRUE}, \code{x} including this
#'         variable will be returned.
#'
#' @examples
#' library(dplyr)
#' library(tibble)
#' dat <- tribble(
#'   ~c1, ~c2, ~c3, ~c4,
#'     1,   3,   1,   1,
#'     2,   2,   1,   1,
#'     3,   1,   2,   3,
#'     1,   2,   1,   2,
#'     3,  NA,   3,   1,
#'    NA,   3,  NA,   2
#' )
#'
#' row_count(dat, count = 1)
#' row_count(dat, count = NA)
#' row_count(dat, c1:c3, count = 2, append = TRUE)
#'
#' @importFrom tibble as_tibble
#' @export
row_count <- function(x, ..., count, var = "rowcount", append = FALSE) {
  # evaluate arguments, generate data
  .dots <- match.call(expand.dots = FALSE)$`...`
  .dat <- get_dot_data(x, .dots)


  # remember original data, if user wants to bind columns
  orix <- tibble::as_tibble(x)

  if (is.data.frame(x)) {
    if (is.na(count))
      rc <- apply(.dat, 1, function(x) sum(is.na(x), na.rm = TRUE))
    else if (is.infinite(count))
      rc <- apply(.dat, 1, function(x) sum(is.infinite(x), na.rm = TRUE))
    else if (is.null(count))
      rc <- apply(.dat, 1, function(x) sum(is.null(x), na.rm = TRUE))
    else
      rc <- apply(.dat, 1, function(x) sum(x == count, na.rm = TRUE))
  } else {
    stop("`x` must be a data frame.", call. = F)
  }


  # to tibble, and rename variable
  rc <- tibble::as_tibble(rc)
  colnames(rc) <- var

  # combine data
  if (append) rc <- dplyr::bind_cols(orix, rc)

  rc
}
