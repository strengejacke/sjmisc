#' @title Convert variable into factor and keep value labels
#' @name to_factor
#'
#' @description This function converts a variable into a factor, but preserves
#'                variable and value label attributes. See 'Examples'.
#'
#' @seealso \code{\link{to_value}} to convert a factor into a numeric value and
#'            \code{\link{to_label}} to convert a value into a factor with labelled
#'            factor levels.
#'
#' @param x numeric or atomic variable or a data frame with
#'          numeric or atomic variables.
#' @param add.non.labelled logical, if \code{TRUE}, non-labelled values also
#'          get value labels.
#' @param drop.na logical, if \code{TRUE}, all types of missing value codes are
#'          converted into NA before \code{x} is converted as factor. If
#'          \code{FALSE}, missing values will be left as their original codes.
#'          See 'Examples' and \code{\link{get_na}}.
#' @return A factor variable, including variable and value labels, respectively
#'           a data frame with factor variables (including variable and value labels)
#'           if \code{x} was a data frame.
#'
#' @note This function is intended for use with vectors that have value and variable
#'        label attributes. Unlike \code{\link{as.factor}}, \code{to_factor} converts
#'        a variable into a factor and preserves the value and variable label attributes.
#'        \cr \cr
#'        Adding label attributes is automatically done by importing data sets
#'        with one of the \code{read_*}-functions, like \code{\link{read_spss}}.
#'        Else, value and variable labels can be manually added to vectors
#'        with \code{\link{set_labels}} and \code{\link{set_label}}.
#'
#' @details See 'Details' in \code{\link{get_na}}.
#'
#' @examples
#' \dontrun{
#' data(efc)
#' library(sjPlot)
#' # normal factor conversion, loses value attributes
#' efc$e42dep <- as.factor(efc$e42dep)
#' sjt.frq(efc$e42dep)
#'
#' # factor conversion, which keeps value attributes
#' efc$e42dep <- to_factor(efc$e42dep)
#' sjt.frq(efc$e42dep)}
#'
#' data(efc)
#' # create parially labelled vector
#' x <- set_labels(efc$e42dep,
#'                 c(`1` = "independent",
#'                   `4` = "severe dependency",
#'                   `9` = "missing value"))
#'
#' # only copy existing value labels
#' to_factor(x)
#' get_labels(to_factor(x), include.values = "p")
#'
#' # also add labels to non-labelled values
#' to_factor(x, add.non.labelled = TRUE)
#' get_labels(to_factor(x, add.non.labelled = TRUE), include.values = "p")
#'
#' # create labelled integer, with missing flag
#' x <- labelled(c(1, 2, 1, 3, 4, 1),
#'               c(Male = 1, Female = 2, Refused = 3, "N/A" = 4),
#'               c(FALSE, FALSE, TRUE, TRUE))
#' # to factor, with missing labels
#' to_factor(x, drop.na = FALSE)
#' # to factor, missings removed
#' to_factor(x, drop.na = TRUE)
#'
#' @export
to_factor <- function(x, add.non.labelled = FALSE, drop.na = TRUE) {
  if (is.matrix(x) || is.data.frame(x)) {
    for (i in 1:ncol(x)) x[[i]] <- to_fac_helper(x[[i]],
                                                 add.non.labelled,
                                                 drop.na)
    return(x)
  } else {
    return(to_fac_helper(x,
                         add.non.labelled,
                         drop.na))
  }
}


#' @name to_fac
#' @rdname to_factor
#' @export
to_fac <- function(x, add.non.labelled = FALSE, drop.na = TRUE) {
  .Deprecated("to_factor")
  return(to_factor(x, add.non.labelled, drop.na))
}


to_fac_helper <- function(x, add.non.labelled, drop.na) {
  # is already factor?
  if (is.factor(x)) return(x)
  # remove missings?
  if (drop.na) x <- to_na(x)
  # retrieve value labels
  lab <- get_labels(x,
                    attr.only = TRUE,
                    include.values = "n",
                    include.non.labelled = add.non.labelled)
  # retrieve variable labels
  varlab <- get_label(x)
  # retrieve missing codes
  nas <- suppressMessages(get_na(x))
  # convert variable to factor
  x <- as.factor(x)
  # set back value labels
  x <- suppressMessages(set_labels(x,
                                   lab,
                                   force.labels = TRUE,
                                   force.values = FALSE))
  # set back variable labels
  x <- set_label(x, varlab)
  # set back missing codes
  x <- set_na(x, nas, as.attr = TRUE)
  return(x)
}
