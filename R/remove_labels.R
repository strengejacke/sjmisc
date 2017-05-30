#' @rdname add_labels
#' @export
remove_labels <- function(x, ..., labels, value) {
  # check deprecated arguments
  if (!missing(value)) {
    message("Argument `value` is deprecated. Please use `labels` instead.")
    labels <- value
  }

  # check for valid value. value must be a named vector
  if (is.null(labels)) stop("`labels` is NULL.", call. = F)
  # if value is NA, it must be tagged
  if (is.na(labels) && !haven::is_tagged_na(labels)) stop("`labels` must be a tagged NA.", call. = F)

  # evaluate arguments, generate data
  .dat <- get_dot_data(x, dplyr::quos(...))

  if (is.data.frame(x)) {
    # iterate variables of data frame
    for (i in colnames(.dat)) {
      x[[i]] <- remove_labels_helper(.dat[[i]], labels)
    }
    # coerce to tibble
    x <- tibble::as_tibble(x)
  } else {
    x <- remove_labels_helper(.dat, labels)
  }

  x
}


#' @importFrom haven is_tagged_na na_tag
remove_labels_helper <- function(x, labels) {
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
  if (haven::is_tagged_na(labels[1])) {
    current.na <- current.na[haven::na_tag(current.na) != haven::na_tag(labels)]
  } else if (is.numeric(labels)) {
    current.labels <- current.labels[-labels]
  } else if (is.character(labels)) {
    # find value labels that should be removes
    removers <- as.vector(current.labels) %in% labels
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

  x
}
