#' @title Format numbers
#' @name big_mark
#'
#' @description \code{big_mark()} formats large numbers with big marks, while
#'    \code{prcn()} converts a numeric scalar between 0 and 1 into a character
#'    vector, representing the percentage-value.
#'
#' @param x A vector or data frame. All numeric inputs (including numeric character)
#'          vectors) will be prettified. For \code{prcn()}, a number between
#'          0 and 1, or a vector or data frame with such numbers.
#' @param big.mark Character, used as mark between every 3 decimals before the decimal point.
#' @param ... Other arguments passed down to the \code{\link{prettyNum}}-function.
#'
#' @return For \code{big_mark()}, a prettified \code{x} as character, with big marks.
#'    For \code{prcn}, a character vector with a percentage number.
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
#' # convert numbers into percentage, as character
#' prcn(0.2389)
#' prcn(c(0.2143887, 0.55443, 0.12345))
#'
#' dat <- data.frame(
#'   a = c(.321, .121, .64543),
#'   b = c("a", "b", "c"),
#'   c = c(.435, .54352, .234432)
#' )
#' prcn(dat)
#'
#' @export
big_mark <- function(x, big.mark = ",", ...) {
  UseMethod("big_mark")
}

#' @export
big_mark.data.frame <- function(x, big.mark = ",", ...) {
  as.data.frame(lapply(x, FUN = big_mark_helper, big.mark, ...))
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


#' @export
#' @rdname big_mark
prcn <- function(x) {
  UseMethod("prcn")
}

#' @export
prcn.default <- function(x) sprintf("%.2f%%", round(x * 100, 2))

#' @export
prcn.data.frame <- function(x) {
  as.data.frame(
    purrr::map_if(x, is.numeric, ~ sprintf("%.2f%%", round(.x * 100, 2))),
    stringsAsFactors = FALSE
  )
}
