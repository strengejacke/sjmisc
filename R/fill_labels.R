#' @title Add missing value labels to partially labelled vector
#' @name fill_labels
#'
#' @description This function adds value labels to a partially labelled vector,
#'                i.e. if not all values are labelled, non-labelled values
#'                get labels.
#'
#' @param x Variable (vector), \code{data.frame} or \code{list} of variables
#'          with partially added value labels (see \code{\link[haven]{labelled}}).
#'
#' @return \code{x}, where labels for non-labelled values are added.
#'
#' @seealso \code{\link{drop_labels}} is the counterpart to \code{fill_labels}
#'            and drops labels from zero-count (non-existing) values.
#'
#' @examples
#' # create labelled integer, with tagged missings
#' library(haven)
#' x <- labelled(c(1:3, tagged_na("a", "c", "z"), 4:1),
#'               c("Agreement" = 1, "Disagreement" = 4, "First" = tagged_na("c"),
#'                 "Refused" = tagged_na("a"), "Not home" = tagged_na("z")))
#' # get current values and labels
#' x
#' get_labels(x)
#'
#' fill_labels(x)
#' get_labels(fill_labels(x))
#' # same as
#' get_labels(x, include.non.labelled = TRUE)
#'
#' @export
fill_labels <- function(x) {
  UseMethod("fill_labels")
}

#' @export
fill_labels.data.frame <- function(x) {
  tibble::as_tibble(lapply(x, FUN = fill_labels_helper))
}

#' @export
fill_labels.list <- function(x) {
  lapply(x, FUN = fill_labels_helper)
}

#' @export
fill_labels.default <- function(x) {
  fill_labels_helper(x)
}

fill_labels_helper <- function(x) {
  # get current labels
  current.values <- get_labels(x, attr.only = T, include.non.labelled = F)
  # get all labels, including non-labelled values
  all.values <- get_labels(x,
                           attr.only = T,
                           include.values = "n",
                           include.non.labelled = T)
  # have any values?
  if (!is.null(all.values)) {
    # set back all labels, if amount of all labels differ
    # from the "current" values
    if (length(all.values) > length(current.values)) {
      # first, we need to switch name attribute and values
      all.val.switch <- as.numeric(names(all.values))
      names(all.val.switch) <- as.character(all.values)
      # get current NA values
      current.na <- get_na(x)
      # add NA
      if (!is.null(current.na)) all.val.switch <- c(all.val.switch, current.na)
      # then set labels
      x <- set_labels(x, all.val.switch, force.labels = T, force.values = T)
    }
  }
  return(x)
}
