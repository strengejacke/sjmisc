#' @title Dichotomize variables
#' @name dicho
#'
#' @description Dichotomizes variables into dummy variables (0/1). Dichotomization is
#'                either done by median, mean or a specific value (see \code{dich.by}).
#'                Either single vectors, a complete data frame or a list of
#'                variables can be dichotomized.
#'
#' @param x Variable (vector), \code{data.frame} or \code{list} of variables
#'          that should be dichotomized
#' @param dich.by Indicates the split criterion where a variable is dichotomized
#'          \describe{
#'            \item{\code{"median"}}{by default, \code{x} is split into two groups at the median. May be abbreviated as \code{"md"}.}
#'            \item{\code{"mean"}}{splits \code{x} into two groups at the mean of \code{x}. May be abbreviated as \code{"m"}.}
#'            \item{\code{"value"}}{splits \code{x} into two groups at a specific value (see \code{dich.val}). May be abbreviated as \code{"v"}.}
#'            }
#' @param dich.val Numeric, indicates a value where \code{x} is dichotomized when \code{dich.by = "value"}.
#'          \strong{Note that \code{dich.val} is inclusive}, i.e. \code{dich.val = 10} will split \code{x}
#'          into one group with values from lowest to 10 and another group with values greater
#'          than 10.
#' @param as.num Logical, if \code{TRUE}, return value will be numeric, not a factor.
#' @param var.label Optional string, to set variable label attribute for the
#'          dichotomized variable (see \code{\link{set_label}}). If \code{NULL}
#'          (default), variable label attribute of \code{x} will be used (if present).
#' @param val.labels Optional character vector (of length two), to set value label
#'          attributes of dichotomized variable (see \code{\link{set_labels}}).
#'          If \code{NULL} (default), no value labels will be set.
#' @return A dichotomized factor (or numeric, if \code{as.num = TRUE}) variable (0/1-coded),
#'           respectively a data frame or list of dichotomized factor (or numeric) variables.
#'
#' @note Variable label attributes (see, for instance, \code{\link{set_label}}) are preserved
#'         (unless changes via \code{var.label}-argument).
#'
#' @examples
#' data(efc)
#' summary(efc$c12hour)
#' table(dicho(efc$c12hour))
#' table(dicho(efc$c12hour, "mean"))
#' table(dicho(efc$c12hour, "value", 30))
#'
#' # sample data frame, values from 1-4
#' head(efc[, 6:10])
#' # dichtomized values (1 to 2 = 0, 3 to 4 = 1)
#' head(dicho(efc[, 6:10], "v", 2))
#'
#' # dichtomize several variables in a list
#' dummy <- list(efc$c12hour, efc$e17age, efc$c160age)
#' dicho(dummy)
#'
#' # dichotomize and set labels. requires package
#' # sjPlot to test
#' \dontrun{
#' library(sjPlot)
#' sjp.frq(dicho(efc$e42dep,
#'               var.label = "Dependency (dichotomized)",
#'               val.labels = c("lower", "higher")))}
#'
#' @export
dicho <- function(x,
                  dich.by = "median",
                  dich.val = -1,
                  as.num = FALSE,
                  var.label = NULL,
                  val.labels = NULL) {
  # --------------------------------
  # check abbreviations
  # --------------------------------
  if (dich.by == "md") dich.by <- "median"
  if (dich.by == "m") dich.by <- "mean"
  if (dich.by == "v") dich.by <- "value"
  # --------------------------------
  # check for correct dichotome types
  # --------------------------------
  if (dich.by != "median" && dich.by != "mean" && dich.by != "value") {
    stop("argument \"dich.by\" must either be \"median\", \"mean\" or \"value\"." , call. = FALSE)
  }
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # --------------------------------
    # get length of data frame or list, i.e.
    # determine number of variables
    # --------------------------------
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # --------------------------------
    # dichotomize all
    # --------------------------------
    for (i in 1:nvars) x[[i]] <- dicho_helper(x[[i]], dich.by, dich.val, as.num, var.label, val.labels)
    return(x)
  } else {
    return(dicho_helper(x, dich.by, dich.val, as.num, var.label, val.labels))
  }
}


#' @importFrom stats median
dicho_helper <- function(x, dich.by, dich.val, as.num, var.label, val.labels) {
  # --------------------------------
  # do we have labels? if not, try to
  # automatically get variable labels
  # --------------------------------
  if (is.null(var.label))
    varlab <- get_label(x)
  else
    varlab <- var.label
  # --------------------------------
  # check if factor. factors need conversion
  # to numeric before dichtomizing
  # --------------------------------
  if (is.factor(x)) {
    # --------------------------------
    # non-numeric-factor cannot be converted
    # --------------------------------
    if (is_num_fac(x)) {
      # try to convert to numeric
      x <- as.numeric(as.character(x))
    } else {
      # --------------------------------
      # convert non-numeric factor to numeric
      # factor levels are replaced by numeric values
      # --------------------------------
      x <- to_value(x, keep.labels = FALSE)
      message("Trying to dichotomize non-numeric factor.")
    }
  }
  # split at median
  if (dich.by == "median") {
    x <- ifelse(x <= stats::median(x, na.rm = T), 0, 1)
    # split at mean
  } else if (dich.by == "mean") {
    x <- ifelse(x <= mean(x, na.rm = T), 0, 1)
    # split at specific value
  } else {
    x <- ifelse(x <= dich.val, 0, 1)
  }
  if (!as.num) x <- as.factor(x)
  # --------------------------------
  # add back labels
  # --------------------------------
  # set back variable labels
  if (!is.null(varlab)) x <- set_label(x, varlab)
  # set value labels
  if (!is.null(val.labels)) x <- set_labels(x, val.labels)
  return(x)
}
