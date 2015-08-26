#' @title Add value labels to variables
#' @name add_labels
#'
#' @description This function adds additional labels as attribute to a variable
#'                or vector \code{x}, resp. to a set of variables in a
#'                \code{data.frame} or \code{list}-object. Unlike \code{\link{set_labels}},
#'                \code{add_labels} does not replace existing value labels, but add
#'                \code{labels} to the existing value labels of \code{x}.
#'
#' @seealso \code{\link{set_label}} to manually set variable labels or
#'            \code{\link{get_label}} to get variable labels; \code{\link{set_labels}} to
#'            add value labels, replacing the existing ones.
#'
#' @param x Variable (vector), \code{list} of variables or a \code{data.frame}
#'          where value label attributes should be added. Does not replaces former
#'          value labels.
#' @param labels Named character vector of labels that will be added to \code{x} as
#'          \code{"labels"} or \code{"value.labels"} attribute. If \code{x} is
#'          a data frame, \code{labels} may also be a \code{\link{list}} of
#'          named character vectors. If \code{labels} is a list, it must have
#'          the same length as number of columns of \code{x}. If \code{labels}
#'          is a vector and \code{x} is a data frame, \code{labels} will be applied
#'          to each column of \code{x}.
#'
#' @return \code{x} with additional value labels.
#'
#' @note Existing labelled values will be replaced by new labelled values
#'         in \code{labels}. See 'Examples'.
#'
#' @examples
#' data(efc)
#' get_labels(efc$e42dep)
#'
#' x <- add_labels(efc$e42dep, c(`5` = "nothing"))
#' get_labels(x)
#'
#' x <- add_labels(efc$e42dep, c(`5` = "nothing", `0` = "zero value"))
#' get_labels(x, include.values = "p")
#'
#' # replace old values
#' x <- add_labels(efc$e42dep, c(`4` = "not so dependent", `5` = "lorem ipsum"))
#' get_labels(x, include.values = "p")
#'
#'
#' @export
add_labels <- function(x, labels) {
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # dichotomize all
    for (i in 1:nvars) x[[i]] <- add_labels_helper(x[[i]], labels)
    return(x)
  } else {
    return(add_labels_helper(x, labels))
  }
}


add_labels_helper <- function(x, labels) {
  # ----------------------------------------
  # get current labels of `x`
  # ----------------------------------------
  current.labels <- get_labels(x,
                               attr.only = T,
                               include.values = "n",
                               include.non.labelled = F)
  # ----------------------------------------
  # if we had already labels, append new ones
  # ----------------------------------------
  if (!is.null(current.labels)) {
    # -------------------------
    # remove double values labels
    # -------------------------
    doubles <- names(current.labels) %in% names(labels)
    # -------------------------
    # update all labels
    # -------------------------
    all.labels <- c(current.labels[!doubles], labels)
    # -------------------------
    # tell user
    # -------------------------
    warning(sprintf("label '%s' was replaced with new value label.\n", current.labels[doubles]), call. = F)
  } else {
    all.labels <- labels
  }
  # ----------------------------------------
  # sort labels by values
  # ----------------------------------------
  all.labels <- all.labels[order(names(all.labels))]
  # ----------------------------------------
  # set back labels
  # ----------------------------------------
  x <- set_labels(x, labels = all.labels)
  return(x)
}
