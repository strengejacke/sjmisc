#' @title Convert factors to numeric variables
#' @name to_value
#'
#' @description This function converts (replaces) factor values with the
#' related factor level index number, thus the factor is converted to
#' a numeric variable.
#'
#' @seealso \code{\link{to_label}} to convert a labelled vector into a factor with labelled
#'            factor levels and \code{\link{to_factor}} to convert a numeric variable
#'            into a factor (and preserve labels)
#'
#' @param x \code{\link{factor}} or a data frame with \code{factor}s. May also be
#'          a character vector.
#' @param start.at starting index, i.e. the lowest numeric value of the variable's
#'          value range. By default, this argument is \code{NULL}, hence the lowest
#'          value of the returned numeric variable corresponds to the lowest factor
#'          level (if factor is \code{\link{numeric}}) or to \code{1} (if factor levels
#'          are not numeric).
#' @param keep.labels logical, if \code{TRUE}, former factor levels will be added as
#'          value labels. For numeric factor levels, values labels will be used,
#'          if present. See 'Examples' and \code{\link{set_labels}} for more details.
#' @return A numeric variable with values ranging either from \code{start.at} to
#'           \code{start.at} + length of factor levels, or to the corresponding
#'           factor levels (if these were numeric). Or a data frame with numeric
#'           variables, if \code{x} was a data frame.
#'
#' @examples
#' data(efc)
#' test <- to_label(efc$e42dep)
#' table(test)
#'
#' table(to_value(test))
#' hist(to_value(test, 0))
#'
#' # set lowest value of new variable to "5".
#' table(to_value(test, 5))
#'
#' # numeric factor keeps values
#' dummy <- factor(c("3", "4", "6"))
#' table(to_value(dummy))
#'
#' # do not drop unused factor levels
#' dummy <- ordered(c(rep("No", 5), rep("Maybe", 3)),
#'                  levels = c("Yes", "No", "Maybe"))
#' to_value(dummy)
#'
#' # non-numeric factor is converted to numeric
#' # starting at 1
#' dummy <- factor(c("D", "F", "H"))
#' table(to_value(dummy))
#'
#' # for numeric factor levels, value labels will be used, if present
#' dummy1 <- factor(c("3", "4", "6"))
#' set_labels(dummy1) <- c("first", "2nd", "3rd")
#' dummy1
#' to_value(dummy1)
#'
#' # for non-numeric factor levels, these will be used.
#' # value labels will be ignored
#' dummy2 <- factor(c("D", "F", "H"))
#' set_labels(dummy2) <- c("first", "2nd", "3rd")
#' dummy2
#' to_value(dummy2)
#'
#'
#' @export
to_value <- function(x, start.at = NULL, keep.labels = TRUE) {
  UseMethod("to_value")
}

#' @export
to_value.data.frame <- function(x, start.at = NULL, keep.labels = TRUE) {
  tibble::as_tibble(lapply(x, FUN = to_value_helper, start.at, keep.labels))
}

#' @export
to_value.list <- function(x, start.at = NULL, keep.labels = TRUE) {
  lapply(x, FUN = to_value_helper, start.at, keep.labels)
}

#' @export
to_value.default <- function(x, start.at = NULL, keep.labels = TRUE) {
  to_value_helper(x, start.at, keep.labels)
}

to_value_helper <- function(x, start.at, keep.labels) {
  labels <- NULL
  # is already numeric?
  if (is.numeric(x)) return(x)
  # get labels
  labels <- get_labels(x, attr.only = T, include.values = "n")
  # is character?
  if (is.character(x)) {
    # has labels?
    if (!is.null(labels)) {
      # sort labels correctly
      lvls <- levels(as.factor(x))
      labels <- unname(labels[order(names(labels), lvls)])
    }
    # convert to factor
    x <- as.factor(x)
  }
  # check if we have numeric factor levels
  if (is_num_fac(x)) {
    # retrieve "value labels"
    if (is.null(labels)) labels <- levels(x)
    # convert to numeric via as.vector
    new_value <- as.numeric(as.vector((x)))
    # new minimum value?
    if (!is.null(start.at) && is.numeric(start.at)) {
      # check if lowest value of variable differs from
      # requested minimum conversion value
      val_diff <- start.at - min(new_value, na.rm = T)
      # adjust new_value
      new_value <- new_value + val_diff
    }
  } else {
    # use non-numeric factor levels as new labels
    labels <- levels(x)
    # check start.at value
    if (is.null(start.at)) start.at <- 1
    # get amount of categories
    l <- length(levels(x))
    # determine highest category value
    end <- start.at + l - 1
    # replace labels with numeric values
    levels(x) <- c(start.at:end)
    # convert to numeric
    new_value <- as.numeric(as.character(x))
  }
  # check if we should attach former labels as value labels
  if (keep.labels) new_value <- set_labels(new_value, labels, force.labels = T)
  return(new_value)
}
