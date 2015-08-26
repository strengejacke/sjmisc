#' @title Check whether value is odd
#' @name is_odd
#'
#' @param x Numeric vector or single numeric value.
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
is_odd <- function(x) (x %% 2) == 1
