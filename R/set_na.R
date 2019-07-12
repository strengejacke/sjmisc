#' @title Replace specific values in vector with NA
#' @name set_na_if
#'
#' @description \code{set_na_if()} is a scoped variant of
#'    \code{\link[sjlabelled]{set_na}}, where values will be replaced only
#'    with NA's for those variables that match the logical condition of
#'    \code{predicate}.
#'
#' @seealso \code{\link{replace_na}} to replace \code{\link{NA}}'s with specific
#'   values, \code{\link{rec}} for general recoding of variables and
#'   \code{\link{recode_to}} for re-shifting value ranges. See
#'   \code{\link[sjlabelled]{get_na}} to get values of missing values in
#'   labelled vectors.
#'
#' @param na Numeric vector with values that should be replaced with NA values,
#'   or a character vector if values of factors or character vectors should be
#'   replaced. For labelled vectors, may also be the name of a value label. In
#'   this case, the associated values for the value labels in each vector
#'   will be replaced with \code{NA}. \code{na} can also be a named vector.
#'   If \code{as.tag = FALSE}, values will be replaced only in those variables
#'   that are indicated by the value names (see 'Examples').
#' @param drop.levels Logical, if \code{TRUE}, factor levels of values that have
#'   been replaced with \code{NA} are dropped. See 'Examples'.
#' @param as.tag Logical, if \code{TRUE}, values in \code{x} will be replaced
#'   by \code{tagged_na}, else by usual \code{NA} values. Use a named
#'   vector to assign the value label to the tagged NA value (see 'Examples').
#'
#' @inheritParams to_factor
#' @inheritParams rec
#'
#' @return \code{x}, with all values in \code{na} being replaced by \code{NA}.
#'   If \code{x} is a data frame, the complete data frame \code{x} will
#'   be returned, with NA's set for variables specified in \code{...};
#'   if \code{...} is not specified, applies to all variables in the
#'   data frame.
#'
#' @examples
#' dummy <- data.frame(var1 = sample(1:8, 100, replace = TRUE),
#'                     var2 = sample(1:10, 100, replace = TRUE),
#'                     var3 = sample(1:6, 100, replace = TRUE))
#'
#' p <- function(x) max(x, na.rm = TRUE) > 7
#' tmp <- set_na_if(dummy, predicate = p, na = 8:9)
#' head(tmp)
#'
#' @importFrom dplyr select_if
#' @export
set_na_if <- function(x, predicate, na, drop.levels = TRUE, as.tag = FALSE) {

  # select variables that match logical conditions
  .dat <- dplyr::select_if(x, .predicate = predicate)


  # if no variable matches the condition specified
  # in predicate, return original data

  if (sjmisc::is_empty(.dat)) {
    return(x)
  }


  if (is.data.frame(x)) {
    # iterate variables of data frame
    for (i in colnames(.dat)) {
      x[[i]] <- sjlabelled::set_na(
        x = .dat[[i]],
        na = na,
        drop.levels = drop.levels,
        as.tag = as.tag
      )
    }
  } else {
    x <- sjlabelled::set_na(
      x = x,
      na = na,
      drop.levels = drop.levels,
      as.tag = as.tag
    )
  }

  x
}
