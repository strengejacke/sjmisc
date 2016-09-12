#' @title Trim leading and trailing whitespaces from strings
#' @name trim
#'
#' @description Trims leading and trailing whitespaces from strings or
#'                character vectors.
#'
#' @param x Character vector or string, or a list or data frame with such vectors.
#'          Function is vectorized, i.e. vector may have a length greater than
#'          1. See 'Examples'.
#'
#' @return Trimmed \code{x}, i.e. with leading and trailing spaces removed.
#'
#' @examples
#' trim("white space at end ")
#' trim(" white space at start and end ")
#' trim(c(" string1 ", "   string2", "string 3   "))
#'
#' tmp <- data.frame(a = c(" string1 ", "   string2", "string 3   "),
#'                   b = c(" strong one  ", "    string two", "  third string "),
#'                   c = c(" str1 ", "   str2", "str3   "))
#' tmp
#' trim(tmp)
#'
#' @export
trim <- function(x) {
  UseMethod("trim")
}

#' @export
trim.data.frame <- function(x) {
  tibble::as_tibble(lapply(x, FUN = trim_helper))
}

#' @export
trim.list <- function(x) {
  lapply(x, FUN = trim_helper)
}

#' @export
trim.default <- function(x) {
  trim_helper(x)
}

trim_helper <- function(x) gsub("^\\s+|\\s+$", "", x)