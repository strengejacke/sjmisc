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
#'          value labels. See \code{\link{set_labels}} for more details.
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
#' # set lowest value of new variable
#' # to "5".
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
#' @export
to_value <- function(x,
                     start.at = NULL,
                     keep.labels = TRUE) {
  if (is.matrix(x) || is.data.frame(x)) {
    for (i in 1:ncol(x)) x[[i]] <- to_value_helper(x[[i]], start.at, keep.labels)
    return(x)
  } else {
    return(to_value_helper(x, start.at, keep.labels))
  }
}


to_value_helper <- function(x, start.at, keep.labels) {
  # is already numeric?
  if (is.numeric(x)) return(x)
  # is character?
  if (is.character(x)) return(as.numeric(as.factor(x)))
  # retrieve "value labels"
  labels <- levels(x)
  # check if we have numeric factor levels
  if (is_num_fac(x)) {
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
  if (keep.labels) new_value <- set_labels(new_value, labels, T)
  return(new_value)
}
