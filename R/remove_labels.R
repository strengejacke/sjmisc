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
#' @param value Either a numeric vector, indicating the position of one or more label
#'          attributes that should be removed (see \code{\link{get_labels}} to
#'          retrieve a vector's label attributes); a character vector with names
#'          of label attributes that should be removed; or a \code{\link[haven]{tagged_na}}
#'          to remove the labels from specific NA values.
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
#' library(haven)
#' x <- labelled(c(1:3, tagged_na("a", "c", "z"), 4:1),
#'               c("Agreement" = 1, "Disagreement" = 4, "First" = tagged_na("c"),
#'                 "Refused" = tagged_na("a"), "Not home" = tagged_na("z")))
#' # get current NA values
#' get_na(x)
#' get_na(remove_labels(x, tagged_na("c")))
#'
#'
#' @export
remove_labels <- function(x, value) {
  UseMethod("remove_labels")
}

#' @export
remove_labels.data.frame <- function(x, value) {
  tibble::as_tibble(lapply(x, FUN = remove_labels_helper, value))
}

#' @export
remove_labels.list <- function(x, value) {
  lapply(x, FUN = remove_labels_helper, value)
}

#' @export
remove_labels.default <- function(x, value) {
  remove_labels_helper(x, value)
}


#' @importFrom haven is_tagged_na na_tag
remove_labels_helper <- function(x, value) {
  # value needs to be specified
  if (is.null(value)) stop("`value` must not be NULL.", call. = F)
  # if value is NA, it must be tagged
  if ((is.na(value) && !haven::is_tagged_na(value))) stop("`value` must be a tagged NA.", call. = F)

  # get current labels of `x`
  current.labels <- get_labels(x,
                               attr.only = T,
                               include.values = "n",
                               include.non.labelled = F)

  # get current NA values
  current.na <- get_na(x)

  # if we have no labels, return
  if (is.null(current.labels) && is.null(current.na)) {
    message("`x` has no value labels.")
    return(x)
  }

  # remove by index?
  if (haven::is_tagged_na(value)) {
    current.na <- current.na[haven::na_tag(current.na) != haven::na_tag(value)]
  } else if (is.numeric(value)) {
    current.labels <- current.labels[-value]
  } else if (is.character(value)) {
    # find value labels that should be removes
    removers <- as.vector(current.labels) %in% value
    # remove them
    current.labels <- current.labels[!removers]
  }

  # switch value and names attribute, since get_labels
  # returns the values as names, and the value labels
  # as "vector content"
  all.labels <- as.numeric(names(current.labels))
  names(all.labels) <- as.character(current.labels)

  # sort labels by values
  all.labels <- all.labels[order(as.numeric(all.labels))]

  # set back labels
  x <- set_labels(x, labels = c(all.labels, current.na))
  return(x)
}

#' @rdname remove_labels
#' @export
`remove_labels<-` <- function(x, value) {
  UseMethod("remove_labels<-")
}

#' @export
`remove_labels<-.default` <- function(x, value) {
  remove_labels(x, value)
}
