#' @title Check whether value is even or odd
#' @name is_even
#'
#' @description Checks whether \code{x} is an even or odd number. Only
#'                accepts numeric vectors.
#'
#' @param x Numeric vector or single numeric value, or a data frame or list with
#'          such vectors.
#'
#' @return \code{is_even()} returns \code{TRUE} for each even value of \code{x}, \code{FALSE} for
#'           odd values. \code{is_odd()} returns \code{TRUE} for each odd value of \code{x}
#'           and \code{FALSE} for even values.
#'
#' @examples
#' is_even(4)
#' is_even(5)
#' is_even(1:4)
#'
#' is_odd(4)
#' is_odd(5)
#' is_odd(1:4)
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



#' @rdname is_even
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
