#' @title Trim leading and trailing whitespaces from strings
#' @name trim
#'
#' @description Trims leading and trailing whitespaces from strings or
#'                character vectors.
#'
#' @param x Character vector or string. Function is vectorized, i.e. vector
#'          may have a length greater than 1. See 'Examples'.
#'
#' @return Trimmed \code{x}, i.e. with leading and trailing spaces removed.
#'
#' @examples
#' trim("white space at end ")
#' trim(" white space at start and end ")
#' trim(c(" string1 ", "   string2", "string 3   "))
#'
#' @export
trim <- function(x) gsub("^\\s+|\\s+$", "", x)