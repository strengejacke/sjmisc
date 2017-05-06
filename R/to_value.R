#' @title Convert factors to numeric variables
#' @name to_value
#'
#' @description This function converts (replaces) factor levels with the
#' related factor level index number, thus the factor is converted to
#' a numeric variable.
#'
#' @param start.at Starting index, i.e. the lowest numeric value of the variable's
#'          value range. By default, this argument is \code{NULL}, hence the lowest
#'          value of the returned numeric variable corresponds to the lowest factor
#'          level (if factor levels are numeric) or to \code{1} (if factor levels
#'          are not numeric).
#' @param keep.labels Logical, if \code{TRUE}, former factor levels will be added as
#'          value labels. For numeric factor levels, values labels will be used,
#'          if present. See 'Examples' and \code{\link{set_labels}} for more details.
#'
#' @return A numeric variable with values ranging either from \code{start.at} to
#'           \code{start.at} + length of factor levels, or to the corresponding
#'           factor levels (if these were numeric). If \code{x} is a data frame,
#'           the complete data frame \code{x} will be returned, where variables
#'           specified in \code{...} are coerced to numeric; if \code{...} is
#'           not specified, applies to all variables in the data frame.
#'
#' @inheritParams to_factor
#'
#' @examples
#' data(efc)
#' test <- to_label(efc$e42dep)
#' table(test)
#'
#' table(to_value(test))
#' hist(to_value(test, start.at = 0))
#'
#' # set lowest value of new variable to "5".
#' table(to_value(test, start.at = 5))
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
#' dummy1 <- set_labels(dummy1, labels = c("first", "2nd", "3rd"))
#' dummy1
#' to_value(dummy1)
#'
#' # for non-numeric factor levels, these will be used.
#' # value labels will be ignored
#' dummy2 <- factor(c("D", "F", "H"))
#' dummy2 <- set_labels(dummy2, labels = c("first", "2nd", "3rd"))
#' dummy2
#' to_value(dummy2)
#'
#'
#' # easily coerce specific variables in a data frame to numeric
#' # and keep other variables, with their class preserved
#' data(efc)
#' efc$e42dep <- as.factor(efc$e42dep)
#' efc$e16sex <- as.factor(efc$e16sex)
#' efc$e17age <- as.factor(efc$e17age)
#'
#' # convert back "sex" and "age" into numeric
#' to_value(efc, e16sex, e17age)
#'
#' @export
to_value <- function(x, ..., start.at = NULL, keep.labels = TRUE) {
  # evaluate arguments, generate data
  .dat <- get_dot_data(x, dplyr::quos(...))

  if (is.data.frame(x)) {
    # iterate variables of data frame
    for (i in colnames(.dat)) {
      x[[i]] <- to_value_helper(.dat[[i]], start.at, keep.labels)
    }
    # coerce to tibble
    x <- tibble::as_tibble(x)
  } else {
    x <- to_value_helper(.dat, start.at, keep.labels)
  }

  x
}


to_value_helper <- function(x, start.at, keep.labels) {
  labels <- NULL

  # is already numeric?
  if (is.numeric(x)) return(x)

  # save variable label
  varlab <- get_label(x)

  # get labels
  labels <- get_labels(x, attr.only = T, include.values = "n")

  # is character?
  if (is.character(x)) {
    # has labels?
    if (!is.null(labels)) {
      # sort labels correctly, therefor get "levels"
      lvls <- levels(as.factor(x))
      # do we have more labels than values? If yes, drop unused labels
      if (length(labels) > length(lvls)) labels <- labels[names(labels) %in% lvls]
      # it might be that we have more levels than labels, in this case
      # drop unused levels - else, ordering won't work
      if (length(lvls) > length(labels)) lvls <- lvls[lvls %in% names(labels)]
      # sort labels correctly
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

  # check if we should set back former variable and value labels
  if (keep.labels) {
    new_value <- set_labels(new_value, labels = labels, force.labels = T)
    new_value <- set_label(new_value, label = varlab)
  }

  new_value
}
