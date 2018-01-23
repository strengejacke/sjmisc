#' @title Determine variable type
#' @name var_type
#'
#' @description This function returns the type of a variable as character. It
#'              is similar to \code{\link[pillar]{type_sum}}, however, the
#'              return value is not truncated, and \code{var_type()} works
#'              on data frames and within pipe-chains.
#'
#' @param abbr Logical, if \code{TRUE}, returns a shortened, abbreviated value
#'        for the variable type (as returned by \code{\link[pillar]{type_sum}}).
#'        If \code{FALSE} (default), a longer "description" is returned.
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
#' @importFrom purrr map_chr
#' @export
var_type <- function(x, ..., abbr = FALSE) {

  # get dot data
  x <- get_dot_data(x, dplyr::quos(...))

  if (is.data.frame(x))
    purrr::map_chr(x, ~ get_vt(.x, abbr = abbr))
  else
    get_vt(x, abbr = abbr)
}


#' @importFrom purrr flatten_chr
#' @importFrom pillar type_sum
#' @importFrom dplyr case_when
get_vt <- function(x, abbr) {
  # get type of object. might be multiple types, e.g. for labelled vectors
  vt <- purrr::flatten_chr(strsplit(pillar::type_sum(x), "+", fixed = TRUE))

  # only keep "main" type of object
  if (length(vt) > 1) vt <- vt[1]

  if (!abbr) {
    vt <- dplyr::case_when(
      vt == "ord" ~ "ordinal",
      vt == "fct" ~ "categorical",
      vt == "dbl" ~ "numeric",
      vt == "int" ~ "integer",
      vt == "chr" ~ "character",
      vt == "lbl" ~ "labelled",
      TRUE ~ vt
    )
  }

  vt
}
