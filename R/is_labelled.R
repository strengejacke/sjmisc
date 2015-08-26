#' @title Check whether object is of class "labelled"
#' @name is_labelled
#' @description This function checks whether \code{x} is of class \code{labelled}.
#'
#' @param x An object.
#' @return Logical, \code{TRUE} if \code{any(class(x))} is \code{labelled},
#'           \code{FALSE} otherwise.
#'
#' @export
is_labelled <- function(x) {
  # check if object has multiple class attributes
  if (length(class(x)) > 1) return(any(class(x) == "labelled"))
  # return if labelled
  return(class(x) == "labelled")
}
