#' @title Convert variable into factor with associated value labels
#' @name to_label
#'
#' @description This function converts (replaces) values of a variable (also of factors
#'                or character vectors) with their associated value labels. Might
#'                be helpful for factor variables.
#'                For instance, if you have a Gender variable with 0/1 value, and associated
#'                labels are male/female, this function would convert all 0 to male and
#'                all 1 to female and returns the new variable as factor.
#'
#' @param add.non.labelled Logical, if \code{TRUE}, values without associated
#'          value label will also be converted to labels (as is). See 'Examples'.
#' @param prefix Logical, if \code{TRUE}, the value labels used as factor levels
#'          or character values will be prefixed with their associated values. See 'Examples'.
#' @param drop.na Logical, if \code{TRUE}, tagged \code{NA} values with value labels
#'          will be converted to regular NA's. Else, tagged \code{NA} values will be replaced
#'          with their value labels. See 'Examples' and \code{\link{get_na}}.
#' @param drop.levels Logical, if \code{TRUE}, unused factor levels will be
#'          dropped (i.e. \code{\link{droplevels}} will be applied before returning
#'          the result).
#'
#' @inheritParams to_factor
#' @inheritParams rec
#'
#' @return A factor with the associated value labels as factor levels. If \code{x}
#'           is a data frame, the complete data frame \code{x} will be returned,
#'           where variables specified in \code{...} are coerced to factors;
#'           if \code{...} is not specified, applies to all variables in the
#'           data frame.
#'
#' @note Value label attributes will be removed when converting variables to factors.
#'       \cr \cr
#'       This function is kept for backwards-compatibility. It is preferred to
#'       use \code{\link[sjlabelled]{as_label}}.
#'
#' @examples
#' library(sjlabelled)
#' data(efc)
#' print(get_labels(efc)['c161sex'])
#' head(efc$c161sex)
#' head(to_label(efc$c161sex))
#'
#' # Find more examples at '?sjlabelled::as_label'
#'
#' @importFrom sjlabelled as_label
#' @export
to_label <- function(x, ..., add.non.labelled = FALSE, prefix = FALSE, var.label = NULL, drop.na = TRUE, drop.levels = FALSE) {
  sjlabelled::as_label(
    x = x,
    ...,
    add.non.labelled = add.non.labelled,
    prefix = prefix,
    var.label = var.label,
    drop.na = drop.na,
    drop.levels = drop.levels
  )
}



#' @title Convert variable into character vector and replace values with associated value labels
#' @name to_character
#'
#' @description This function converts (replaces) variable values (also of factors
#'                or character vectors) with their associated value labels and returns
#'                them as character vector. This is just a convenient wrapper for
#'                \code{as.character(to_label(x))}.
#'
#' @inheritParams to_label
#'
#' @note Value labels will be removed when converting variables to factors,
#'       variable labels, however, are preserved.
#'       \cr \cr
#'       This function is kept for backwards-compatibility. It is preferred to
#'       use \code{\link[sjlabelled:as_label]{sjlabelled::as_character()}}.
#'
#' @return A character vector with the associated value labels as values. If \code{x}
#'           is a data frame, the complete data frame \code{x} will be returned,
#'           where variables specified in \code{...} are coerced
#'           to character variables;
#'           if \code{...} is not specified, applies to all variables in the
#'           data frame.
#'
#' @examples
#' library(sjlabelled)
#' data(efc)
#' print(get_labels(efc)['c161sex'])
#' head(efc$c161sex)
#' head(to_character(efc$c161sex))
#'
#' # Find more examples at '?sjlabelled::as_label'
#'
#' @importFrom sjlabelled as_character
#' @export
to_character <- function(x, ..., add.non.labelled = FALSE, prefix = FALSE, var.label = NULL, drop.na = TRUE, drop.levels = FALSE) {
  sjlabelled::as_character(
    x = x,
    ...,
    add.non.labelled = add.non.labelled,
    prefix = prefix,
    var.label = var.label,
    drop.na = drop.na,
    drop.levels = drop.levels
  )
}
