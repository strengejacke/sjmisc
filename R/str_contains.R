#' @title Check if string contains pattern
#' @name str_contains
#' @description This functions checks whether a string \code{x} contains
#'                the string \code{pattern}. By default, this function is
#'                case sensitive.
#'
#' @param x Character string where matches are sought.
#' @param pattern Character string to be matched in \code{x}.
#' @param ignore.case Logical, whether matching should be case sensitive or not.
#'
#' @return \code{TRUE} if \code{x} contains \code{pattern}.
#'
#' @examples
#' str_contains("hello", "hel")
#' str_contains("hello", "hal")
#'
#' str_contains("hello", "Hel")
#' str_contains("hello", "Hel", ignore.case = TRUE)
#'
#'
#' @export
str_contains <- function(x, pattern, ignore.case = FALSE) {
  if (ignore.case) {
    x <- tolower(x)
    pattern <- tolower(pattern)
  }
  return(!is_empty(grep(pattern, x, fixed = T)))
}
