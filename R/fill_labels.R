#' @title Add missing value labels to partially labelled vector
#' @name fill_labels
#'
#' @description This function adds value labels to a partially labelled vector,
#'                i.e. if not all values are labelled, non-labelled values
#'                get labels.
#'
#' @param x Variable (vector), \code{data.frame} or \code{list} of variables
#'          with partially added value labels (see \code{\link[haven]{labelled}}).
#' @return \code{x}, where labels for non-labelled values are added
#'
#' @seealso \code{\link{drop_labels}} to drop labels from zero-count values.
#'
#' @examples
#' # create labelled integer, with missing flag
#' x <- labelled(c(1, 2, 1, 3, 4, 1, 5),
#'               c(Good = 1, Bad = 5))
#' get_labels(x)
#' get_labels(x, include.non.labelled = TRUE)
#'
#' fill_labels(x)
#' get_labels(fill_labels(x))
#'
#' # create partially labelled vector with missings
#' x <- labelled(c(1, 2, 1, 3, 4, 1, 5),
#'               c(Male = 1, Female = 2, Refused = 5),
#'               c(FALSE, FALSE, TRUE))
#' x
#' fill_labels(x)
#' get_labels(fill_labels(x))
#'
#' # get summary
#' x <- labelled(c(1, 2, 1, 3, 4, 1, NA, 5),
#'               c(Male = 1, Female = 2, Refused = 5),
#'               c(FALSE, FALSE, TRUE))
#' frq(x)
#'
#' @export
fill_labels <- function(x) {
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # na all
    for (i in 1:nvars) x[[i]] <- fill_labels_helper(x[[i]])
    return(x)
  } else {
    return(fill_labels_helper(x))
  }
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
    # get missing values
    missings <- get_na_flags(x)
    # create new missing vector with same length as
    # all present values
    all.missings <- rep(FALSE, length(all.values))
    # "insert" former missings into new missing vector
    if (!is.null(missings)) all.missings[match(current.values, all.values)] <- missings
    # set back all labels, if amount of all labels differ
    # from the "current" values
    if (length(all.values) > length(current.values)) {
      # first, we need to switch name attribute and values
      all.val.switch <- as.numeric(names(all.values))
      names(all.val.switch) <- as.character(all.values)
      # then set labels
      x <- set_labels(x, all.val.switch, force.labels = T, force.values = T)
    }
    # set back missing information
    x <- set_na(x, all.missings, as.attr = T)
  }
  return(x)
}
