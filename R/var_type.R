#' @title Determine variable type
#' @name var_type
#'
#' @description This function returns the type of a variable as character. It
#'              is similar to \code{\link[tibble]{type_sum}}, however, the
#'              return value is not truncated, and \code{var_type()} works
#'              on data frames and within pipe-chains.
#'
#' @seealso \code{\link[tibble]{type_sum}}
#'
#' @inheritParams to_factor
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
#'
#' @importFrom tibble type_sum
#' @importFrom dplyr case_when
#' @importFrom purrr map_chr
#' @export
var_type <- function(x, ...) {

  # get dot data
  x <- get_dot_data(x, dplyr::quos(...))

  if (is.data.frame(x))
    purrr::map_chr(x, ~ get_vt(.x))
  else
    get_vt(x)
}


get_vt <- function(x) {
  vt <- tibble::type_sum(x)

  dplyr::case_when(
    vt == "ord" ~ "ordinal",
    vt == "fctr" ~ "categorical",
    vt == "dbl" ~ "numeric",
    vt == "int" ~ "integer",
    vt == "chr" ~ "character",
    TRUE ~ vt
  )
}
