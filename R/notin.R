#' @title Value matching
#' @name %nin%
#'
#' @description \%nin\% is the complement to \%in\%. It looks which values
#'              in \code{x} do \emph{not} match (hence, are \emph{not in})
#'              values in \code{y}.
#'
#' @param x	Vector with values to be matched.
#' @param y	Vector with values to be matched against.
#'
#' @details See 'Details' in \code{\link{match}}.
#'
#' @return A logical vector, indicating if a match was \emph{not} located for each element
#'         of \code{x}, thus the values are \code{TRUE} or \code{FALSE} and
#'         never \code{NA}.
#'
#' @examples
#' c("a", "B", "c") %in% letters
#' c("a", "B", "c") %nin% letters
#'
#' c(1, 2, 3, 4) %in% c(3, 4, 5, 6)
#' c(1, 2, 3, 4) %nin% c(3, 4, 5, 6)
#'
#' @export
"%nin%" <- function(x, y) {
  !(x %in% y)
}
