#' @title Check whether object is of class "labelled"
#' @name is_labelled
#' @description This function checks whether \code{x} is of class \code{labelled}.
#'
#' @param x An object.
#' @return Logical, \code{TRUE} if \code{x} inherits from class \code{labelled},
#'           \code{FALSE} otherwise.
#'
#' @export
is_labelled <- function(x) inherits(x, "labelled")
