# evaluates arguments
#' @importFrom dplyr select_
#' @importFrom stats as.formula
get_dot_data <- function(data, dots) {
  # check if data is a data frame
  if (is.data.frame(data) && length(dots) > 0) {
    # find functions
    funs <- lapply(dots, function(x) as.character(x)[1] == "~")

    # extract only variables of dot-argument
    vars <- as.character(dots[!unlist(funs)])

    # extract only functions of dot-argument
    if (!all(funs == FALSE))
      funs <- stats::as.formula(dots[[which(unlist(funs))]])
    else
      funs <- NULL

    # we need to evaluate functions and variables separately
    x <- dplyr::select_(data, .dots = vars)

    # now check if we also have functions in dot-argument, and
    # select those variables as well
    if (!is.null(funs))
      x <- dplyr::bind_cols(x, dplyr::select_(data, .dots = funs))
  } else {
    x <- data
  }

  x
}

# return names of objects passed as ellipses argument
dot_names <- function(dots) unname(unlist(lapply(dots, as.character)))

is_float <- function(x) is.numeric(x) && !all(x %% 1 == 0, na.rm = T)

is_foreign <- function(x) return(!is.null(x) && x == "value.labels")

is_completely_labelled <- function(x) {
  # get label attribute, which may differ depending on the package
  # used for reading the data
  attr.string <- getValLabelAttribute(x)
  # if variable has no label attribute, use factor levels as labels
  if (is.null(attr.string)) return(TRUE)
  # retrieve named labels
  lab <- attr(x, attr.string, exact = T)
  lab <- lab[!haven::is_tagged_na(lab)]
  if (!is.null(lab) && length(lab) > 0) {
    # get values of variable
    valid.vals <- sort(unique(stats::na.omit(as.vector(x))))
    # retrieve values associated with labels. for character vectors
    # or factors with character levels, these values are character values,
    # else, they are numeric values
    if (is.character(x) || (is.factor(x) && !is_num_fac(x)))
      values <- unname(lab)
    else
      values <- as.numeric(unname(lab))
    # check if we have different amount values than labels
    # or, if we have same amount of values and labels, whether
    # values and labels match or not
    return(length(valid.vals) == length(lab) && !anyNA(match(values, valid.vals)))
  }
  return(TRUE)
}

# auto-detect attribute style for variable labels.
# either haven style ("label") or foreign style
# ("variable.label")
getVarLabelAttribute <- function(x) {
  attr.string <- NULL
  # check if x is data frame. if yes, retrieve one "example" variable
  if (is.data.frame(x) || is.list(x)) {
    # define length for loop
    if (is.data.frame(x))
      counter <- ncol(x)
    else
      counter <- length(x)
    # we need to check all variables until first variable
    # that has any attributes at all - SPSS variables without
    # labels would return NULL, so if -e.g.- first variable
    # of data set has no label attribute, but second had, this
    # function would stop after first attribute and return NULL
    for (i in seq_len(counter)) {
      # retrieve attribute names
      an <- names(attributes(x[[i]]))
      # check for label attributes
      if (any(an == "label") || any(an == "variable.label")) {
        x <- x[[i]]
        break
      }
    }
  }
  # check if vector has label attribute
  if (!is.null(attr(x, "label", exact = T))) attr.string <- "label"
  # check if vector has variable label attribute
  if (!is.null(attr(x, "variable.label", exact = T))) attr.string <- "variable.label"
  # not found any label yet?
  if (is.null(attr.string)) {
    # check value_labels option
    opt <- getOption("value_labels")
    if (!is.null(opt)) attr.string <- ifelse(opt == "haven", "label", "variable.label")
  }
  return(attr.string)
}


# auto-detect attribute style for value labels.
# either haven style ("labels") or foreign style
# ("value.labels")
getValLabelAttribute <- function(x) {
  attr.string <- NULL
  # check if x is data frame. if yes, just retrieve one "example" variable
  if (is.data.frame(x)) {
    # find first variable with labels or value.labels attribute
    for (i in seq_len(ncol(x))) {
      # has any attribute?
      if (!is.null(attr(x[[i]], "labels", exact = T))) {
        attr.string <- "labels"
        break
      } else if (!is.null(attr(x[[i]], "value.labels", exact = T))) {
        attr.string <- "value.labels"
        break
      }
    }
  } else {
    # check if vector has labels attribute
    if (!is.null(attr(x, "labels", exact = T))) attr.string <- "labels"
    # check if vector has value.labels attribute
    if (!is.null(attr(x, "value.labels", exact = T))) attr.string <- "value.labels"
  }
  # not found any label yet?
  if (is.null(attr.string)) {
    # check value_labels option
    opt <- getOption("value_labels")
    if (!is.null(opt)) attr.string <- ifelse(opt == "haven", "label", "variable.label")
  }
  return(attr.string)
}
