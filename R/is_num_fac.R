#' @title Check whether a factor has numeric levels only
#' @name is_num_fac
#' @description This function checks whether a factor has only numeric or
#'                any non-numeric factor levels.
#'
#' @param x A \code{\link{factor}}.
#' @return Logical, \code{TRUE} if factor has numeric factor levels only,
#'           \code{FALSE} otherwise.
#'
#' @examples
#' # numeric factor levels
#' f1 <- factor(c(NA, 1, 3, NA, 2, 4))
#' is_num_fac(f1)
#'
#' # not completeley numeric factor levels
#' f2 <- factor(c(NA, "C", 1, 3, "A", NA, 2, 4))
#' is_num_fac(f2)
#'
#' # not completeley numeric factor levels
#' f3 <- factor(c("Justus", "Bob", "Peter"))
#' is_num_fac(f3)
#'
#' @export
is_num_fac <- function(x) {
  # check if we have numeric levels
  return(!anyNA(suppressWarnings(as.numeric(levels(x)))))
}
