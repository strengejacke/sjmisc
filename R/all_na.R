#' @title Check if vector only has NA values
#' @name all_na
#'
#' @description Check if all values in a vector are \code{NA}.
#'
#' @param x A vector or data frame.
#'
#' @return Logical, \code{TRUE} if \code{x} has only NA values, \code{FALSE} if
#'         \code{x} has at least one non-missing value.
#'
#' @examples
#' x <- c(NA, NA, NA)
#' y <- c(1, NA, NA)
#'
#' all_na(x)
#' all_na(y)
#' all_na(data.frame(x, y))
#'
#'
#' @importFrom tibble as_tibble
#' @export
all_na <- function(x) {
  UseMethod("all_na")
}

#' @export
all_na.default <- function(x) {
  sum(!is.na(x)) == 0
}

#' @export
all_na.data.frame <- function(x) {
  tibble::as_tibble(lapply(x, function(v) sum(!is.na(v)) == 0))
}
