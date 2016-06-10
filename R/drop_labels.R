#' @title Drop labels of zero-count values
#' @name drop_labels
#'
#' @description This function drops all value labels for those values that have
#'                no cases (frequencies) in a vector.
#'
#' @param x Variable (vector), \code{data.frame} or \code{list} of variables
#'          with partially added value labels (see \code{\link[haven]{labelled}}).
#'
#' @return \code{x}, where value labels for non-existing values are removed.
#'
#' @seealso \code{\link{zap_labels}} and \code{\link{zap_unlabelled}} to convert
#'            (non-)labelled values into \code{NA}; \code{\link{fill_labels}} to
#'            add labels to existing, but not yet labelled values. The latter
#'            function is the counterpart to \code{drop_labels}.
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
#'
#' # watch result. due to recode-pattern, we have age groups with
#' # no observations (zero-counts)
#' \dontrun{
#' library(sjPlot)
#' sjt.frq(x)
#' # now, let's drop zero's
#' sjt.frq(drop_labels(x))}
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
  # get labels
  tidy.labels <- get_labels(x, attr.only = T, include.values = "n", include.non.labelled = F, drop.na = T)
  # remove labels with no values in data
  tidy.labels <- tidy.labels[get_values(x) %in% names(table(x))]
  # get NA labels
  current.na <- get_na(x)
  # set labels
  set_labels(x, labels = c(tidy.labels, current.na))
}
