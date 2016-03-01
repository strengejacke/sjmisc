#' @title Drop labels of zero-count values
#' @name drop_labels
#'
#' @description This function drops all value labels for those values that have
#'                no cases (frequencies) in a vector.
#'
#' @param x Variable (vector), \code{data.frame} or \code{list} of variables
#'          with partially added value labels (see \code{\link[haven]{labelled}}).
#' @return \code{x}, where labels for non-labelled values are added
#'
#' @seealso \code{\link{zap_labels}} and \code{\link{zap_unlabelled}} to convert
#'            (non-)labelled values into \code{NA}; \code{\link{fill_labels}} to
#'            add labels to existing, but not yet labelled values.
#'
#' @examples
#' rp <- rec_pattern(1, 100)
#' rp
#'
#' # sample data
#' data(efc)
#' # recode carers age into groups of width 5
#' x <- rec(efc$c160age, rp$pattern)
#' # add value labels to new vector
#' set_labels(x) <- rp$labels
#' # watch result. due to recode-pattern, we have age groups with
#' # no observations (zero-counts)
#' frq(as_labelled(x))
#'
#' # now, let's drop zero's
#' frq(as_labelled(drop_labels(x)))
#'
#' @export
drop_labels <- function(x) {
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # na all
    for (i in 1:nvars) x[[i]] <- drop_labels_helper(x[[i]])
    return(x)
  } else {
    return(drop_labels_helper(x))
  }
}

drop_labels_helper <- function(x) {
  # first, get frequency table
  mydat <- get_frq(x, coerce = TRUE)
  # get all valid values, that have counts
  valid.values <- !is.na(mydat$value) & mydat$count > 0
  # create labels
  value.labels <- as.character(mydat$label[valid.values])
  # get value names
  values <- mydat$value[valid.values]
  # name vector
  names(value.labels) <- values
  # set labels
  set_labels(x, labels = value.labels)
}
