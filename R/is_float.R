#' @title Check if a variable is of (non-integer) double type
#' @name is_float
#'
#' @description This function checks whether an input vector or value is a
#'    numeric non-integer (double), depending on fractional parts of the value(s).
#'
#' @param x A value, vector or data frame.
#'
#' @return \code{TRUE} if \code{x} is a floating value (non-integer double),
#'    \code{FALSE} otherwise.
#'
#' @examples
#' is.double(4)
#' is_float(4)
#'
#' is_float(4.2)
#'
#' is_float(iris)
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
