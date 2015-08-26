#' @title Check whether two factors are crossed
#' @name is_crossed
#' @description This function checks whether two factors are crossed,
#'                i.e. if each level of one factor occurs in combination
#'                with each level of the other factor.
#'
#' @param f1 Numeric vector or \code{\link{factor}}.
#' @param f2 Numeric vector or \code{\link{factor}}.
#' @return Logical, \code{TRUE} if factors are crossed, \code{FALSE} otherwise.
#'
#' @seealso \code{\link{is_nested}}
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
#' @export
is_crossed <- function(f1, f2) {
  tab <- table(f1, f2)
  # for crossed factors, we should have no zeros in any rows
  # (i.e. each level of f1 also contains any level of f2)
  return(!any(apply(tab, 1, function(x) any(x == 0)) == TRUE))
}
