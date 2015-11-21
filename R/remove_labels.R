#' @title Remove value labels from variables
#' @name remove_labels
#'
#' @description This function removes labels from a label attribute of a
#'                vector \code{x}, resp. from a set of vectors in a
#'                \code{data.frame} or \code{list}-object. The counterpart
#'                to this function is \code{\link{add_labels}}.
#'
#' @seealso \code{\link{set_labels}} to add value labels, replacing the existing ones;
#'            \code{\link{add_labels}} to add new labels to a vector.
#'
#' @param x Variable (vector), \code{list} of variables or a \code{data.frame}
#'          where value label attributes should be removed.
#' @param value Either a numeric vector, indicating one or more label attributes that
#'          should be removed (see \code{\link{get_labels}} to retrieve a vector's
#'          label attributes), or a character vector with names of label attributes
#'          that should be removed.
#'
#' @return \code{x} with removed value labels.
#'
#' @examples
#' data(efc)
#' get_labels(efc$e42dep)
#'
#' x <- remove_labels(efc$e42dep, 2)
#' get_labels(x, include.values = "p")
#'
#' x <- remove_labels(efc$e42dep, "independent")
#' get_labels(x, include.values = "p")
#'
#'
#' @export
remove_labels <- function(x, value) {
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # dichotomize all
    for (i in 1:nvars) x[[i]] <- remove_labels_helper(x[[i]], value)
    return(x)
  } else {
    return(remove_labels_helper(x, value))
  }
}


remove_labels_helper <- function(x, value) {
  # ----------------------------------------
  # get current labels of `x`
  # ----------------------------------------
  current.labels <- get_labels(x,
                               attr.only = T,
                               include.values = "n",
                               include.non.labelled = F)
  # ----------------------------------------
  # if we have no labels, return
  # ----------------------------------------
  if (is.null(current.labels)) {
    message("`x` has no value label attributes.")
    return(x)
  }
  # -------------------------
  # remove by index?
  # -------------------------
  if (is.numeric(value)) {
    current.labels <- current.labels[-value]
  } else if (is.character(value)) {
    # -------------------------
    # find value labels that should be removes
    # -------------------------
    removers <- as.vector(current.labels) %in% value
    # remove them
    current.labels <- current.labels[!removers]
  }
  # -------------------------
  # switch value and names attribute, since get_labels
  # returns the values as names, and the value labels
  # as "vector content"
  # -------------------------
  all.labels <- as.numeric(names(current.labels))
  names(all.labels) <- as.character(current.labels)
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

#' @rdname remove_labels
#' @export
`remove_labels<-` <- function(x, value) {
  UseMethod("remove_labels<-")
}

#' @export
`remove_labels<-.default` <- function(x, value) {
  x <- remove_labels(x, value)
  x
}
