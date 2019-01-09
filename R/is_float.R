#' @title Check if a variable is of (non-integer) double type or a whole number
#' @name is_float
#'
#' @description \code{is_float()} checks whether an input vector or value is a
#'    numeric non-integer (double), depending on fractional parts of the value(s).
#'    \code{is_whole()} does the opposite and checks whether an input vector
#'    is a whole number (without fractional parts).
#'
#' @param x A value, vector or data frame.
#'
#' @return For \code{is_float()}, \code{TRUE} if \code{x} is a floating value
#'   (non-integer double), \code{FALSE} otherwise (also returns \code{FALSE}
#'   for character vectors and factors). For \code{is_whole()}, \code{TRUE}
#'   if \code{x} is a vector with whole numbers only, \code{FALSE} otherwise
#'   (returns \code{TRUE} for character vectors and factors).
#'
#' @examples
#' data(mtcars)
#' data(iris)
#'
#' is.double(4)
#' is_float(4)
#' is_float(4.2)
#' is_float(iris)
#'
#' is_whole(4)
#' is_whole(4.2)
#' is_whole(mtcars)
#'
#'
#' @export
is_float <- function(x) {
  UseMethod("is_float")
}


#' @export
is_float.default <- function(x) {
  is.numeric(x) && !all(x %% 1 == 0, na.rm = T)
}

#' @importFrom purrr map_lgl
#' @export
is_float.data.frame <- function(x) {
  purrr::map_lgl(x, ~ is.numeric(.x) && !all(.x %% 1 == 0, na.rm = T))
}

#' @export
is_float.list <- function(x) {
  purrr::map_lgl(x, ~ is.numeric(.x) && !all(.x %% 1 == 0, na.rm = T))
}


#' @rdname is_float
#' @export
is_whole <- function(x) {
  UseMethod("is_whole")
}

iwh <- function(x) {
  (is.numeric(x) && all(floor(x) == x, na.rm = T)) || is.character(x) || is.factor(x)
}

#' @export
is_whole.default <- function(x) {
  iwh(x)
}

#' @importFrom purrr map_lgl
#' @export
is_whole.data.frame <- function(x) {
  purrr::map_lgl(x, iwh)
}

#' @export
is_whole.list <- function(x) {
  purrr::map_lgl(x, iwh)
}
