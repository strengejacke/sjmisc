#' @title Check whether string is empty
#' @name is_empty
#' @description This function checks whether a string or character vector (of
#'                length 1) is empty or not.
#'
#' @param x String or character vector of length 1.
#' @return Logical, \code{TRUE} if \code{x} is empty, \code{FALSE} otherwise.
#'
#' @note \code{NULL}- or \code{NA}-values are also considered as "empty" (see
#'         'Examples') and will return \code{TRUE}.
#'
#' @examples
#' x <- "test"
#' is_empty(x)
#'
#' x <- ""
#' is_empty(x)
#'
#' x <- NA
#' is_empty(x)
#'
#' x <- NULL
#' is_empty(x)
#'
#' # string is not empty
#' is_empty(" ")
#'
#' # however, this trimmed string is
#' is_empty(trim(" "))
#'
#' @export
is_empty <- function(x) {
  if (!is.null(x) && length(x) > 1) warning("'x' must be of length 1. Evaluating first element only.", call. = F)
  return(is.null(x) || nchar(x) == 0 || is.na(x))
}
