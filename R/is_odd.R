#' @title Check whether value is odd
#' @name is_odd
#'
#' @description Checks whether \code{x} is an odd number or not. Only
#'                accepts numeric vectors.
#'
#' @inheritParams is_even
#'
#' @return \code{TRUE} for each odd value of \code{x}, \code{FALSE} for
#'           even values.
#'
#' @seealso \code{\link{is_even}}
#'
#' @examples
#' is_odd(4)
#' is_odd(5)
#' is_odd(1:4)
#'
#' @export
is_odd <- function(x) {
  UseMethod("is_odd")
}

#' @export
is_odd.data.frame <- function(x) {
  lapply(x, FUN = is_odd_helper)
}

#' @export
is_odd.list <- function(x) {
  lapply(x, FUN = is_odd_helper)
}

#' @export
is_odd.default <- function(x) {
  is_odd_helper(x)
}

is_odd_helper <- function(x) (x %% 2) == 1
