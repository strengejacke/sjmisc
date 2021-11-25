#' @title Convert factors to numeric variables
#' @name to_value
#'
#' @description This function converts (replaces) factor levels with the
#' related factor level index number, thus the factor is converted to
#' a numeric variable. \code{to_value()} and \code{to_numeric()} are aliases.
#'
#' @param start.at Starting index, i.e. the lowest numeric value of the variable's
#'          value range. By default, this argument is \code{NULL}, hence the lowest
#'          value of the returned numeric variable corresponds to the lowest factor
#'          level (if factor levels are numeric) or to \code{1} (if factor levels
#'          are not numeric).
#' @param keep.labels Logical, if \code{TRUE}, former factor levels will be added as
#'          value labels. For numeric factor levels, values labels will be used,
#'          if present. See 'Examples' and \code{\link{set_labels}} for more details.
#' @param use.labels Logical, if \code{TRUE} and \code{x} has numeric value labels,
#'          these value labels will be set as numeric values.
#'
#' @return A numeric variable with values ranging either from \code{start.at} to
#'           \code{start.at} + length of factor levels, or to the corresponding
#'           factor levels (if these were numeric). If \code{x} is a data frame,
#'           the complete data frame \code{x} will be returned, where variables
#'           specified in \code{...} are coerced to numeric; if \code{...} is
#'           not specified, applies to all variables in the data frame.
#'
#' @inheritParams to_dummy
#'
#' @note This function is kept for backwards-compatibility. It is preferred to
#'       use \code{\link[sjlabelled]{as_numeric}}.
#'
#' @examples
#' library(sjlabelled)
#' data(efc)
#' test <- as_label(efc$e42dep)
#' table(test)
#' table(to_value(test))
#'
#' # Find more examples at '?sjlabelled::as_numeric'
#' @export
to_value <- function(x, ..., start.at = NULL, keep.labels = TRUE, use.labels = FALSE) {
  sjlabelled::as_numeric(x = x, ..., start.at = start.at, keep.labels = keep.labels, use.labels = use.labels)
}
