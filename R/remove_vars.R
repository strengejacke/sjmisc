#' @title Remove variables from a data frame
#' @name remove_var
#'
#' @description This function removes variables from a data frame, and is
#'   intended to use within a pipe-workflow. \code{remove_cols()} is an
#'   alias for \code{remove_var()}.
#'
#' @param ... Character vector with variable names, or unquoted names
#'   of variables that should be removed from the data frame.
#'   You may also use functions like \code{:} or tidyselect's
#'   select-helpers.
#'
#' @inheritParams to_dummy
#'
#' @return \code{x}, with variables specified in \code{...} removed.
#'
#' @examples
#' mtcars %>% remove_var("disp", "cyl")
#' mtcars %>% remove_var(c("wt", "vs"))
#' mtcars %>% remove_var(drat:am)
#' @export
remove_var <- function(x, ...) {
  vars_to_remove <- tidyselect::vars_select(colnames(x), ...)
  x[colnames(x) %nin% vars_to_remove]
}


#' @rdname remove_var
#' @export
remove_cols <- function(x, ...) {
  remove_var(x, ...)
}
