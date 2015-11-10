#' @title Add value labels to variables
#' @name add_labels
#'
#' @description This function adds additional labels as attribute to a variable
#'                or vector \code{x}, resp. to a set of variables in a
#'                \code{data.frame} or \code{list}-object. Unlike \code{\link{set_labels}},
#'                \code{add_labels} does not replace existing value labels, but adds
#'                \code{value} to the existing value labels of \code{x}.
#'
#' @seealso \code{\link{set_label}} to manually set variable labels or
#'            \code{\link{get_label}} to get variable labels; \code{\link{set_labels}} to
#'            add value labels, replacing the existing ones.
#'
#' @param x Variable (vector), \code{list} of variables or a \code{data.frame}
#'          where value label attributes should be added. Does not replaces former
#'          value labels.
#' @param value Named character vector of labels that will be added to \code{x} as
#'          \code{"labels"} or \code{"value.labels"} attribute. If \code{x} is
#'          a data frame, \code{value} may also be a \code{\link{list}} of
#'          named character vectors. If \code{value} is a list, it must have
#'          the same length as number of columns of \code{x}. If \code{value}
#'          is a vector and \code{x} is a data frame, \code{value} will be applied
#'          to each column of \code{x}.
#'
#' @return \code{x} with additional value labels.
#'
#' @note Existing labelled values will be replaced by new labelled values
#'         in \code{value}. See 'Examples'.
#'
#' @examples
#' data(efc)
#' get_labels(efc$e42dep)
#'
#' x <- add_labels(efc$e42dep, c(`nothing` = 5))
#' get_labels(x)
#'
#' x <- add_labels(efc$e42dep, c(`nothing` = 5, `zero value` = 0))
#' get_labels(x, include.values = "p")
#'
#' # replace old values
#' x <- add_labels(efc$e42dep, c(`not so dependent` = 4, `lorem ipsum` = 5))
#' get_labels(x, include.values = "p")
#'
#' # replace values, alternative function call
#' add_labels(x) <- c(`new second` = 2)
#'
#'
#' @export
add_labels <- function(x, value) {
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # dichotomize all
    for (i in 1:nvars) x[[i]] <- add_labels_helper(x[[i]], value)
    return(x)
  } else {
    return(add_labels_helper(x, value))
  }
}


add_labels_helper <- function(x, value) {
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
    doubles <- names(current.labels) %in% as.character(value)
    # -------------------------
    # switch value and names attribute, since get_labels
    # returns the values as names, and the value labels
    # as "vector content"
    # -------------------------
    val.switch <- as.numeric(names(current.labels))
    names(val.switch) <- as.character(current.labels)
    # -------------------------
    # update all labels
    # -------------------------
    all.labels <- c(val.switch[!doubles], value)
    # -------------------------
    # tell user
    # -------------------------
    if (any(doubles)) warning(sprintf("label '%s' was replaced with new value label.\n", current.labels[doubles]), call. = F)
  } else {
    all.labels <- value
  }
  # ----------------------------------------
  # sort labels by values
  # ----------------------------------------
  all.labels <- all.labels[order(as.numeric(all.labels))]
  # ----------------------------------------
  # set back labels
  # ----------------------------------------
  x <- set_labels(x, labels = all.labels)
  return(x)
}

#' @rdname add_labels
#' @export
`add_labels<-` <- function(x, value) {
  UseMethod("add_labels<-")
}

#' @export
`add_labels<-.default` <- function(x, value) {
  x <- add_labels(x, value)
  x
}
