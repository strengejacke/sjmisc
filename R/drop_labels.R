#' @rdname zap_labels
#' @export
drop_labels <- function(x, drop.na = TRUE) {
  UseMethod("drop_labels")
}

#' @export
drop_labels.data.frame <- function(x, drop.na = TRUE) {
  tibble::as_tibble(lapply(x, FUN = drop_labels_helper, drop.na))
}

#' @export
drop_labels.list <- function(x, drop.na = TRUE) {
  lapply(x, FUN = drop_labels_helper, drop.na)
}

#' @export
drop_labels.default <- function(x, drop.na = TRUE) {
  drop_labels_helper(x, drop.na)
}

drop_labels_helper <- function(x, drop.na) {
  # get labels
  tidy.labels <- get_labels(x, attr.only = T, include.values = "n", include.non.labelled = F, drop.na = T)
  # return x, if no attribute
  if (is.null(tidy.labels)) return(x)
  # all missing in variable?
  if (all(is.na(x))) return(x)
  # remove labels with no values in data
  tidy.labels <- tidy.labels[get_values(x, drop.na = drop.na) %in% names(table(x))]
  # set labels
  set_labels(x, drop.na = drop.na) <- tidy.labels
  return(x)
}
