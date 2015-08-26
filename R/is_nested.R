#' @title Check whether two factors are nested
#' @name is_nested
#' @description This function checks whether two factors are nested,
#'                i.e. if each category of the first factor co-occurs
#'                with only one category of the other.
#'
#' @param f1 Numeric vector or \code{\link{factor}}.
#' @param f2 Numeric vector or \code{\link{factor}}.
#' @return Logical, \code{TRUE} if factors are nested, \code{FALSE} otherwise.
#'
#' @note If factors are nested, a message is displayed to tell whether \code{f1}
#'         is nested within \code{f2} or vice versa.
#'
#' @seealso \code{\link{is_crossed}}
#'
#' @references Grace, K. The Difference Between Crossed and Nested Factors. \href{http://www.theanalysisfactor.com/the-difference-between-crossed-and-nested-factors/}{(web)}
#'
#' @examples
#' # nested factors, each category of
#' # x appears in one category of y
#' x <- c(1,2,3,4,5,6,7,8,9)
#' y <- c(1,1,1,2,2,2,3,3,3)
#' # show distribution
#' table(x, y)
#' # check if nested
#' is_nested(x, y)
#' is_nested(y, x)
#'
#' # not nested factors
#' x <- c(1,2,3,4,5,6,7,8,9,1,2)
#' y <- c(1,1,1,2,2,2,3,3,3,2,3)
#' # show distribution
#' table(x, y)
#' # check if nested
#' is_nested(x, y)
#' is_nested(y, x)
#'
#' @export
is_nested <- function(f1, f2) {
  tab <- table(f1, f2)
  # cross tabulation of nested factors should have only 1 value per row
  # (or column) that is not zero. If we found more, factors are not nested
  # or rows and columns have to be swapped.
  # check if f1 is nested within f2
  nested <- !any(apply(tab, 1, function(x) sum(x != 0) > 1))
  if (nested) message("'f1' is nested within 'f2'")
  # swap rows and columns to check whether factors are nested
  # check whether f2 is nested within f1
  if (!nested) {
    nested <- !any(apply(tab, 2, function(x) sum(x != 0) > 1))
    if (nested) message("'f2' is nested within 'f1'")
  }
  return(nested)
}
