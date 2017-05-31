#' @rdname zap_labels
#' @export
drop_labels <- function(x, ..., drop.na = TRUE) {
  .Deprecated("drop_labels", package = "sjlabelled", msg = "This function will be removed in future versions of sjmisc and has been moved to package 'sjlabelled'. Please use sjlabelled::drop_labels() instead.")

  # evaluate arguments, generate data
  .dat <- get_dot_data(x, dplyr::quos(...))

  if (is.data.frame(x)) {
    # iterate variables of data frame
    for (i in colnames(.dat)) {
      x[[i]] <- drop_labels_helper(.dat[[i]], drop.na = drop.na)
    }
    # coerce to tibble
    x <- tibble::as_tibble(x)
  } else {
    x <- drop_labels_helper(.dat, drop.na = drop.na)
  }

  x
}

drop_labels_helper <- function(x, drop.na) {
  # get labels
  tidy.labels <-
    sjlabelled::get_labels(
      x,
      attr.only = T,
      include.values = "n",
      include.non.labelled = F,
      drop.na = T
    )

  # return x, if no attribute
  if (is.null(tidy.labels)) return(x)

  # all missing in variable?
  if (all(is.na(x))) return(x)

  # remove labels with no values in data
  tidy.labels <- tidy.labels[get_values(x, drop.na = drop.na) %in% names(table(x))]

  # check if tidy labels is empty - then remove everything
  if (sjmisc::is_empty(tidy.labels)) tidy.labels <- ""

  # set labels
  set_labels(x, labels = tidy.labels, drop.na = drop.na)
}
