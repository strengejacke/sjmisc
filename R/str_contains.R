#' @title Check if string contains pattern
#' @name str_contains
#' @description This functions checks whether a string or character vector
#'                \code{x} contains the string \code{pattern}. By default,
#'                this function is case sensitive.
#'
#' @param x Character string where matches are sought. May also be a
#'          character vector of length > 1 (see 'Examples').
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
#' @param switch Logical, if \code{TRUE}, \code{x} will be sought in each element
#'          of \code{pattern}. If \code{switch = TRUE}, \code{x} needs to be of
#'          length 1.
#'
#' @return \code{TRUE} if \code{x} contains \code{pattern}.
#'
#' @details This function iterates all elements in \code{pattern} and
#'            looks for each of these elements if it is found in
#'            \emph{any} element of \code{x}, i.e. which elements
#'            of \code{pattern} are found in the vector \code{x}.
#'            \cr \cr
#'            Technically, it iterates \code{pattern} and calls
#'            \code{grep(x, pattern(i), fixed = TRUE)} for each element
#'            of \code{pattern}. If \code{switch = TRUE}, it iterates
#'            \code{pattern} and calls \code{grep(pattern(i), x, fixed = TRUE)}
#'            for each element of \code{pattern}. Hence, in the latter case
#'            (if \code{switch = TRUE}), \code{x} must be of length 1.
#'
#'
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
#' # is pattern in any element of 'x'?
#' str_contains(c("def", "abc", "xyz"), "abc")
#' # is "abcde" in any element of 'x'?
#' str_contains(c("def", "abc", "xyz"), "abcde") # no...
#' # is "abc" in any of pattern?
#' str_contains("abc", c("defg", "abcde", "xyz12"), switch = TRUE)
#'
#' str_contains(c("def", "abcde", "xyz"), c("abc", "123"))
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
str_contains <- function(x, pattern, ignore.case = FALSE, logic = NULL, switch = FALSE) {
  # check if correct length when switching
  if (switch && length(x) > 1) {
    warning("`x` must be of length 1 when `switch = TRUE`. First element will be used.", call. = F)
    x <- x[1]
  }
  # counter for matches
  cnt <- c()
  # ignore case for x and pattern
  if (ignore.case) {
    x <- tolower(x)
    pattern <- tolower(pattern)
  }
  # iterate patterns
  for (k in pattern) {
    # append result
    if (switch)
      cnt <- c(cnt, !is_empty(grep(x, k, fixed = T)))
    else
      cnt <- c(cnt, !is_empty(grep(k, x, fixed = T)))
  }
  # which logical combination?
  if (is.null(logic))
    return(cnt)
  else if (logic %in% c("or", "OR", "|"))
    return(any(cnt))
  else if (logic %in% c("and", "AND", "&"))
    return(all(cnt))
  else if (logic %in% c("not", "NOT", "!"))
    return(!any(cnt))
  return(cnt)
}
