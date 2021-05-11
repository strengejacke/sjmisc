#' @title Check whether a factor has numeric levels only
#' @name is_num_fac
#' @description \code{is_num_fac()} checks whether a factor has only numeric or
#'    any non-numeric factor levels, while \code{is_num_chr()} checks whether
#'    a character vector has only numeric strings.
#'
#' @param x A factor for \code{is_num_fac()} and a character vector for
#'    \code{is_num_chr()}
#'
#' @return Logical, \code{TRUE} if factor has numeric factor levels only, or
#'    if character vector has numeric strings only, \code{FALSE} otherwise.
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
#' is_num_chr(c("a", "1"))
#' is_num_chr(c("2", "1"))
#'
#' @export
is_num_fac <- function(x) {
  # check if we have numeric levels
  is.factor(x) && !anyNA(suppressWarnings(as.numeric(levels(x))))
}

#' @rdname is_num_fac
#' @export
is_num_chr <- function(x) {
  # check if we have numeric bvalues
  is.character(x) && !anyNA(suppressWarnings(as.numeric(stats::na.omit(x))))
}
