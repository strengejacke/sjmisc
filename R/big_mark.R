#' @title Formats large numbers with big marks
#' @name big_mark
#'
#' @description Formats large numbers with big marks
#'
#' @param x A vector or data frame. All numeric inputs (including numeric character)
#'          vectors) will be prettified.
#' @param big.mark Character, used as mark between every 3 decimals before the decimal point.
#' @param ... Other arguments passed down to the \code{\link{prettyNum}}-function.
#'
#' @return A prettified \code{x} as character, with big marks.
#'
#' @examples
#' # simple big mark
#' big_mark(1234567)
#'
#' # big marks for several values at once, mixed numeric and character
#' big_mark(c(1234567, "55443322"))
#'
#' # pre-defined width of character output
#' big_mark(c(1234567, 55443322), width = 15)
#'
#' @export
big_mark <- function(x, big.mark = ",", ...) {
  UseMethod("big_mark")
}

#' @export
big_mark.data.frame <- function(x, big.mark = ",", ...) {
  tibble::as_tibble(lapply(x, FUN = big_mark_helper, big.mark, ...))
}

#' @export
big_mark.list <- function(x, big.mark = ",", ...) {
  lapply(x, FUN = big_mark_helper, big.mark, ...)
}

#' @export
big_mark.default <- function(x, big.mark = ",", ...) {
  big_mark_helper(x, big.mark, ...)
}

big_mark_helper <- function(x, big.mark, ...) {
  prettyNum(x, big.mark = big.mark, ...)
}