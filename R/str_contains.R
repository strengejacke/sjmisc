#' @title Check if string contains pattern
#' @name str_contains
#' @description This functions checks whether a string \code{x} contains
#'                the string \code{pattern}. By default, this function is
#'                case sensitive.
#'
#' @param x Character string where matches are sought.
#' @param pattern Character string to be matched in \code{x}. May also be a
#'          character vector of length > 1 (see 'Examples').
#' @param ignore.case Logical, whether matching should be case sensitive or not.
#' @param logic Indicates whether a logical combination of multiple search pattern
#'          should be made.
#'          \itemize{
#'            \item Use \code{"or"}, \code{"OR"} or \code{"|"} for a logical or-combination, i.e. at least one element of \code{pattern} is in \code{x}.
#'            \item Use \code{"and"}, \code{"AND"} or \code{"&"} for a logical AND-combination, i.e. all elements of \code{pattern} are in \code{x}.
#'            \item Use \code{"not"}, \code{"NOT"} or \code{"!"} for a logical NOT-combination, i.e. no element of \code{pattern} is in \code{x}.
#'            \item By default, \code{logic = NULL}, which means that \code{TRUE} or \code{FALSE} is returned for each element of \code{pattern} separately.
#'          }
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
#' # which patterns are in "abc"?
#' str_contains("abc", c("a", "b", "e"))
#'
#' # any pattern in "abc"?
#' str_contains("abc", c("a", "b", "e"), logic = "or")
#'
#' # all patterns in "abc"?
#' str_contains("abc", c("a", "b", "e"), logic = "and")
#' str_contains("abc", c("a", "b"), logic = "and")
#'
#' # no patterns in "abc"?
#' str_contains("abc", c("a", "b", "e"), logic = "not")
#' str_contains("abc", c("d", "e", "f"), logic = "not")
#'
#' @export
str_contains <- function(x, pattern, ignore.case = FALSE, logic = NULL) {
  # ignore case in search term
  if (ignore.case) x <- tolower(x)
  # counter for matches
  cnt <- c()
  # iterate patterns
  for (k in pattern) {
    # ignore case for
    if (ignore.case) k <- tolower(k)
    # append result
    cnt <- c(cnt, !is_empty(grep(k, x, fixed = T)))
  }
  # which logical combination?
  if (is.null(logic))
    return(cnt)
  else if (logic %in% c("or", "OR", "|"))
    return(isTRUE(any(cnt)))
  else if (logic %in% c("and", "AND", "&"))
    return(isTRUE(all(cnt)))
  else if (logic %in% c("not", "NOT", "!"))
    return(!isTRUE(any(cnt)))
  return(cnt)
}
