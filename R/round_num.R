#' @title Round numeric variables in a data frame
#' @name round_num
#'
#' @description \code{round_num()} rounds numeric variables in a data frame
#'    that also contains non-numeric variables. Non-numeric variables are
#'    ignored.
#'
#' @param digits Numeric, number of decimals to round to.
#' @inheritParams to_factor
#'
#' @return \code{x} with all numeric variables rounded.
#'
#' @examples
#' data(iris)
#' round_num(iris)
#'
#' @export
round_num <- function(x, digits = 0) {
  UseMethod("round_num")
}


#' @importFrom purrr map_if
#' @export
round_num.data.frame <- function(x, digits = 0) {
  xa <- attributes(x)

  x <- x %>%
    purrr::map_if(is.numeric, ~ round(.x, digits = digits)) %>%
    as.data.frame(stringsAsFactors = FALSE)

  attributes(x) <- xa
  x
}


#' @importFrom purrr map_if
#' @export
round_num.list <- function(x, digits = 0) {
  purrr::map_if(x, is.numeric, ~ round(.x, digits = digits))
}


#' @export
round_num.default <- function(x, digits = 0) {
  round(x, digits = digits)
}
