# Help-functions


is_foreign <- function(x) return(!is.null(x) && x == "value.labels")


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
    for (i in 1:counter) {
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
    # ----------------------------
    # check value_labels option
    # ----------------------------
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
    for (i in 1:ncol(x)) {
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
    # ----------------------------
    # check value_labels option
    # ----------------------------
    opt <- getOption("value_labels")
    if (!is.null(opt)) attr.string <- ifelse(opt == "haven", "label", "variable.label")
  }
  return(attr.string)
}
