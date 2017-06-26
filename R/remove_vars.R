#' @title Remove variables from a data frame
#' @name remove_var
#'
#' @description This function removes variables from a data frame, and is
#'              intended to use within a pipe-workflow.
#'
#' @param ... Character vector with variable names, or unquoted names
#'          of variables that should be removed from the data frame.
#'          You may also use functions like \code{:} or dplyr's \code{\link[dplyr]{select_helpers}}.
#'
#' @inheritParams to_factor
#'
#' @return \code{x}, with variables specified in \code{...} removed.
#'
#' @examples
#' mtcars %>% remove_var("disp", "cyl")
#' mtcars %>% remove_var(c("wt", "vs"))
#' mtcars %>% remove_var(drat:am)
#'
#' @importFrom dplyr select_vars
#' @importFrom tibble as_tibble
#' @export
remove_var <- function(x, ...) {
  vars_to_remove <- dplyr::select_vars(colnames(x), ...)
  tibble::as_tibble(x[colnames(x) %nin% vars_to_remove])
}
