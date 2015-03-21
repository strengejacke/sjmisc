# Help-functions


# automatically set labels of values,
# if attributes are present
autoSetValueLabels <- function(x) {
  # do we have global options?
  opt <- getOption("autoSetValueLabels")
  if (is.null(opt) || opt == TRUE) {
    # check if we have value label attribut
    vl <- sji.getValueLabel(x)
    lv <- levels(x)
    label <- NULL
    # check  if we have value labels
    if (!is.null(vl) && length(vl)>0) {
      label <- vl
    # check  if we have factor levels
    } else if (!is.null(lv)) {
      label <- lv
    # if we have string values, copy them as labels
    } else if (is.character(x)) {
      label <- unique(x)
    }
    return(label)
  }
  return (NULL)
}


# auto-detect attribute style for variable labels.
# either haven style ("label") or foreign style
# ("variable.label")
getVarLabelAttribute <- function(x) {
  attr.string <- NULL
  # check if x is data frame. if yes, just retrieve one "example" variable
  if (is.data.frame(x)) x <- x[[1]]
  # check if vector has label attribute
  if (!is.null(attr(x, "label"))) attr.string <- "label"
  # check if vector has variable label attribute
  if (!is.null(attr(x, "variable.label"))) attr.string <- "variable.label"
  # not found any label yet?
  if (is.null(attr.string)) {
    # ----------------------------
    # check value_labels option
    # ----------------------------
    opt <- getOption("value_labels")
    if (!is.null(opt)) attr.string <- ifelse (opt == "haven", "label", "variable.label")
  }
  return (attr.string)
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
      if (!is.null(attr(x[[i]], "labels"))) {
        attr.string <- "labels"
        break
      } else if (!is.null(attr(x[[i]], "value.labels"))) {
        attr.string <- "value.labels"
        break
      }
    }
  } else {
    # check if vector has labels attribute
    if (!is.null(attr(x, "labels"))) attr.string <- "labels"
    # check if vector has value.labels attribute
    if (!is.null(attr(x, "value.labels"))) attr.string <- "value.labels"
  }
  # not found any label yet?
  if (is.null(attr.string)) {
    # ----------------------------
    # check value_labels option
    # ----------------------------
    opt <- getOption("value_labels")
    if (!is.null(opt)) attr.string <- ifelse (opt == "haven", "label", "variable.label")
  }
  return (attr.string)
}


# automatically set labels of variables,
# if attributes are present
autoSetVariableLabels <- function(x) {
  # do we have global options?
  opt <- getOption("autoSetVariableLabels")
  if (is.null(opt) || opt == TRUE) {
    # auto-detect variable label attribute
    attr.string <- getVarLabelAttribute(x)
    # nothing found? then leave...
    if (is.null(attr.string)) return (NULL)
    # check if we have variable label attribute
    vl <- as.vector(attr(x, attr.string))
    label <- NULL
    # check if we have variable labels
    if (!is.null(vl) && length(vl) > 0) label <- vl
    return(label)
  }
  return (NULL)
}
