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
#' @param x Numeric, atomic or character vector or a data frame with
#'          such vectors.
#' @param add.non.labelled Logical, if \code{TRUE}, non-labelled values also
#'          get value labels.
#' @param ref.lvl Numeric, specifies the reference level for the new factor. Use
#'          this parameter if a different factor level than the lowest value
#'          should be used as reference level. If \code{NULL}, lowest value
#'          will become the reference level. See \code{\link{ref_lvl}} for
#'          details.
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
#' @details \code{to_factor} converts numeric values into a factor with numeric
#'            levels. \code{\link{to_label}}, however, converts a vector into
#'            a factor and uses value labels as factor levels.
#'            Furthermore, see 'Details' in \code{\link{get_na}}.
#'
#' @examples
#' data(efc)
#' # normal factor conversion, loses value attributes
#' x <- as.factor(efc$e42dep)
#' frq(x)
#'
#' # factor conversion, which keeps value attributes
#' x <- to_factor(efc$e42dep)
#' frq(x)
#'
#' # create parially labelled vector
#' x <- set_labels(efc$e42dep, c(`1` = "independent", `4` = "severe dependency",
#'                               `9` = "missing value"))
#'
#' # only copy existing value labels
#' to_factor(x)
#' get_labels(to_factor(x), include.values = "p")
#'
#' # also add labels to non-labelled values
#' to_factor(x, add.non.labelled = TRUE)
#' get_labels(to_factor(x, add.non.labelled = TRUE), include.values = "p")
#'
#' # Convert to factor, using different reference level
#' x <- to_factor(efc$e42dep)
#' str(x)
#' table(x)
#'
#' x <- to_factor(efc$e42dep, ref.lvl = 3)
#' str(x)
#' table(x)
#'
#' @export
to_factor <- function(x, add.non.labelled = FALSE, ref.lvl = NULL) {
  UseMethod("to_factor")
}

#' @export
to_factor.data.frame <- function(x, add.non.labelled = FALSE, ref.lvl = NULL) {
  tibble::as_tibble(lapply(x, FUN = to_fac_helper, add.non.labelled, ref.lvl))
}

#' @export
to_factor.list <- function(x, add.non.labelled = FALSE, ref.lvl = NULL) {
  lapply(x, FUN = to_fac_helper, add.non.labelled, ref.lvl)
}

#' @export
to_factor.default <- function(x, add.non.labelled = FALSE, ref.lvl = NULL) {
  to_fac_helper(x, add.non.labelled, ref.lvl)
}


to_fac_helper <- function(x, add.non.labelled, ref.lvl) {
  # is already factor?
  if (is.factor(x)) return(x)

  # retrieve value labels
  lab <- get_labels(x, attr.only = TRUE, include.values = "n", include.non.labelled = add.non.labelled)
  # retrieve variable labels
  varlab <- get_label(x)

  # switch value and names attribute, since get_labels
  # returns the values as names, and the value labels
  # as "vector content"
  if (!is.null(lab)) {
    if (is.character(x) || (is.factor(x) && !is_num_fac(x)))
      lab.switch <- names(lab)
    else
      lab.switch <- as.numeric(names(lab))

    names(lab.switch) <- as.character(lab)
  } else {
    lab.switch <- NULL
  }

  # convert variable to factor
  x <- as.factor(x)

  # set back value labels
  x <- suppressMessages(set_labels(x, lab.switch, force.labels = TRUE, force.values = FALSE))
  # set back variable labels
  x <- set_label(x, varlab)
  # change reference level?
  if (!is.null(ref.lvl)) ref_lvl(x) <- ref.lvl
  return(x)
}
