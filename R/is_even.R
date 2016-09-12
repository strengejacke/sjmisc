#' @title Check whether value is even
#' @name is_even
#'
#' @description Checks whether \code{x} is an even number or not. Only
#'                accepts numeric vectors.
#'
#' @param x Numeric vector or single numeric value, or a data frame or list with
#'          such vectors.
#'
#' @return \code{TRUE} for each even value of \code{x}, \code{FALSE} for
#'           odd values.
#'
#' @seealso \code{\link{is_odd}}
#'
#' @examples
#' is_even(4)
#' is_even(5)
#' is_even(1:4)
#'
#' @export
is_even <- function(x) {
  UseMethod("is_even")
}

#' @export
is_even.data.frame <- function(x) {
  lapply(x, FUN = is_even_helper)
}

#' @export
is_even.list <- function(x) {
  lapply(x, FUN = is_even_helper)
}

#' @export
is_even.default <- function(x) {
  is_even_helper(x)
}

is_even_helper <- function(x) (x %% 2) == 0
