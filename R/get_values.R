#' @title Retrieve values of labelled variables
#' @name get_values
#'
#' @description This function retrieves the values associated with value labels
#'                of an imported SPSS, SAS or STATA data set (via \code{\link{read_spss}},
#'                \code{\link{read_sas}} or \code{\link{read_stata}}),
#'                or of a \code{\link[haven]{labelled}} vector.
#'
#' @seealso \code{\link{get_labels}} for getting value labels and \code{\link{get_na}}
#'            to get values for missing values.
#'
#' @param x Variable (vector) with value label attributes.
#' @param sort.val Logical, if \code{TRUE} (default), values of associated value labels
#'          are sorted.
#' @param drop.na Logical, if \code{TRUE}, missing code values are excluded from
#'          the return value. See 'Examples' and \code{\link{get_na}}.
#' @return The values associated with value labels from \code{x},
#'           or \code{NULL} if \code{x} has no label attributes.
#'
#' @details \code{\link[haven]{labelled}} vectors are numeric by default (when imported with read-functions
#'            like \code{\link{read_spss}}) and have variable and value labels attributes.
#'            The value labels are associated with those values from the labelled vector.
#'            This function returns the values associated with the vector's value labels,
#'            which may differ from actual values in the vector (e.g. due to missings)
#'            or are not represented in sorted order.
#'
#' @examples
#' data(efc)
#' str(efc$e42dep)
#' get_values(efc$e42dep)
#' get_labels(efc$e42dep)
#'
#' # create labelled integer, with missing flag
#' x <- labelled(c(1, 2, 1, 3, 4, 1),
#'               c(Male = 1, Female = 2, Refused = 3, "N/A" = 4),
#'               c(FALSE, FALSE, TRUE, TRUE))
#' # get all values
#' get_values(x)
#' # drop NA
#' get_values(x, , TRUE)
#'
#'
#' @export
get_values <- function(x, sort.val = FALSE, drop.na = FALSE) {
  # haven or sjPlot?
  attr.string <- getValLabelAttribute(x)
  # nothing found? then leave...
  if (is.null(attr.string)) return(NULL)
  # get values
  if (is.character(x))
    values <- unname(attr(x, attr.string, exact = T))
  else
    values <- as.numeric(unname(attr(x, attr.string, exact = T)))
  # sort values
  if (sort.val) values <- sort(values)
  # remove missing value codes?
  if (drop.na) {
    # get NA logicals
    na.flag <- get_na_flags(x)
    # do we have missing flag? if yes, remove missing code value
    if (!is.null(na.flag)) values <- values[!na.flag]
  }
  # foreign? then reverse order
  if (is_foreign(attr.string)) values <- rev(values)
  # return sorted
  return(values)
}
