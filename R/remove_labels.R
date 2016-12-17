#' @rdname add_labels
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
  if (is.na(value) && !haven::is_tagged_na(value)) stop("`value` must be a tagged NA.", call. = F)

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
  if (haven::is_tagged_na(value[1])) {
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

  # complete labels, including NA labels
  compl.lab <- c(all.labels, current.na)

  # check if any labels left after removing
  if (is.null(compl.lab) || sjmisc::is_empty(compl.lab)) {
    # clear all labels
    x <- remove_all_labels(x)
  } else {
    # set back labels
    x <- set_labels(x, labels = compl.lab)
  }

  return(x)
}

#' @rdname add_labels
#' @export
`remove_labels<-` <- function(x, value) {
  UseMethod("remove_labels<-")
}

#' @export
`remove_labels<-.default` <- function(x, value) {
  remove_labels(x, value)
}
