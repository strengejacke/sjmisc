#' @title Determine variable type
#' @name var_type
#'
#' @description This function returns the type of a variable as character. It
#'              is similar to \code{\link[pillar]{type_sum}}, however, the
#'              return value is not truncated, and \code{var_type()} works
#'              on data frames and within pipe-chains.
#'
#' @param abbr Logical, if \code{TRUE}, returns a shortened, abbreviated value
#'        for the variable type (as returned by \code{pillar::type_sum()}).
#'        If \code{FALSE} (default), a longer "description" is returned.
#'
#' @inheritParams to_dummy
#'
#' @return The variable type of \code{x}, as character.
#'
#'
#' @examples
#' data(efc)
#'
#' var_type(1)
#' var_type(1L)
#' var_type("a")
#'
#' var_type(efc$e42dep)
#' var_type(to_factor(efc$e42dep))
#'
#' library(dplyr)
#' var_type(efc, contains("cop"))
#' @export
var_type <- function(x, ..., abbr = FALSE) {

  # get dot data
  x <- get_dot_data(x, dplyr::quos(...))

  if (is.data.frame(x))
    purrr::map_chr(x, ~ get_vt(.x, abbr = abbr))
  else
    get_vt(x, abbr = abbr)
}


get_vt <- function(x, abbr) {

  if (is.ordered(x))
    vt <- "ord"
  else if (is.factor(x))
    vt <- "fct"
  else if (methods::is(x, "Date"))
    vt <- "date"
  else {
    vt <- switch(
      typeof(x),
      logical = "lgl",
      integer = "int",
      double = "dbl",
      character = "chr",
      complex = "cpl",
      closure = "fn",
      environment = "env",
      typeof(x)
    )
  }


  if (!abbr) {
    vt <- dplyr::case_when(
      vt == "ord" ~ "ordinal",
      vt == "fct" ~ "categorical",
      vt == "dbl" ~ "numeric",
      vt == "int" ~ "integer",
      vt == "chr" ~ "character",
      vt == "lbl" ~ "labelled",
      vt == "cpl" ~ "complex",
      TRUE ~ vt
    )
  }

  vt
}
