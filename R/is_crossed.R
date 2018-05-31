#' @title Check whether two factors are crossed or nested
#' @name is_crossed
#' @description These functions checks whether two factors are (fully) crossed
#'   or nested, i.e. if each level of one factor occurs in combination with
#'   each level of the other factor (\code{is_crossed()}) resp. if each
#'   category of the first factor co-occurs with only one category of the
#'   other (\code{is_nested()}). \code{is_cross_classified()} checks if one
#'   factor level occurs in some, but not all levels of another factor.
#'
#' @param f1 Numeric vector or \code{\link{factor}}.
#' @param f2 Numeric vector or \code{\link{factor}}.
#'
#' @return Logical. For \code{is_crossed()}, \code{TRUE} if factors are (fully)
#'   crossed, \code{FALSE} otherwise. For \code{is_nested()}, \code{TRUE} if
#'   factors are nested, \code{FALSE} otherwise. For \code{is_cross_classified()},
#'   \code{TRUE}, if one factor level occurs in some, but not all levels of
#'   another factor.
#'
#' @note If factors are nested, a message is displayed to tell whether \code{f1}
#'   is nested within \code{f2} or vice versa.
#'
#' @references Grace, K. The Difference Between Crossed and Nested Factors. \href{http://www.theanalysisfactor.com/the-difference-between-crossed-and-nested-factors/}{(web)}
#'
#' @examples
#' # crossed factors, each category of
#' # x appears in each category of y
#' x <- c(1,4,3,2,3,2,1,4)
#' y <- c(1,1,1,2,2,1,2,2)
#' # show distribution
#' table(x, y)
#' # check if crossed
#' is_crossed(x, y)
#'
#' # not crossed factors
#' x <- c(1,4,3,2,3,2,1,4)
#' y <- c(1,1,1,2,1,1,2,2)
#' # show distribution
#' table(x, y)
#' # check if crossed
#' is_crossed(x, y)
#'
#'
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
#' # also not fully crossed
#' is_crossed(x, y)
#'
#' # but partially crossed
#' is_cross_classified(x, y)
#'
#' @export
is_crossed <- function(f1, f2) {
  tab <- table(f1, f2)
  # for crossed factors, we should have no zeros in any rows
  # (i.e. each level of f1 also contains any level of f2)
  !any(apply(tab, 1, function(x) any(x == 0)) == TRUE)
}

#' @rdname is_crossed
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

  nested
}


#' @rdname is_crossed
#' @export
is_cross_classified <- function(f1, f2) {
  suppressMessages(!is_nested(f1, f2) && !is_crossed(f1, f2))
}
