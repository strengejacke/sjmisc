#' @title Convert vector to labelled class
#' @name as_labelled
#'
#' @param x Variable (vector), \code{data.frame} or \code{list} of variables
#'          that should be converted to \code{\link[haven]{labelled}}-class
#'          objects.
#' @param add.labels Logical, if \code{TRUE}, non-labelled values will be
#'          labelled with the corresponding value.
#' @param add.class Logical, if \code{TRUE}, \code{x} preserves its former
#'          \code{class}-attribute and \code{labelled} is added as additional
#'          attribute. If \code{FALSE} (default), all former \code{class}-attributes
#'          will be removed and the class-attribute  of \code{x} will only
#'          be \code{labelled}.
#' @return \code{x}, as \code{\link[haven]{labelled}}-class object, including
#'           missing-flags (\code{is_na}-attribute).
#'
#' @examples
#'
#' data(efc)
#' str(efc$e42dep)
#'
#' x <- as_labelled(efc$e42dep)
#' str(x)
#' summary(x)
#'
#' x <- as_labelled(efc$e42dep, add.class = TRUE)
#' str(x)
#' summary(x)
#'
#' a <- c(1, 2, 4)
#' x <- as_labelled(a, add.class = TRUE)
#' str(x)
#' summary(x)
#'
#' data(efc)
#' x <- set_labels(efc$e42dep, c(`1` = "independent",
#'                               `4` = "severe dependency"))
#' x1 <- as_labelled(x, add.labels = FALSE)
#' x2 <- as_labelled(x, add.labels = TRUE)
#'
#' str(x1)
#' str(x2)
#'
#' get_values(x1)
#' get_values(x2)
#'
#' @importFrom stats na.omit
#' @export
as_labelled <- function(x, add.labels = FALSE, add.class = FALSE) {
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # dichotomize all
    for (i in 1:nvars) x[[i]] <- as_labelled_helper(x[[i]],
                                                    add.labels,
                                                    add.class)
    return(x)
  } else {
    return(as_labelled_helper(x,
                              add.labels,
                              add.class))
  }
}


as_labelled_helper <- function(x, add.labels, add.class) {
  # check if we have any value label attributes
  vallabel <- get_labels(x, attr.only = T)
  # nothing?
  if (is.null(vallabel)) {
    # factor levels as labels?
    vallabel <- get_labels(x, attr.only = F)
    # still nothing?
    if (is.null(vallabel)) {
      # get unique values
      vallabel <- as.character(unique(stats::na.omit(x)))
    }
    # set value labels
    x <- set_labels(x, vallabel, force.labels = T, force.values = T)
  }
  # fill up missing attributes
  if (add.labels) x <- fill_labels(x)
  # reset missings
  x <- set_na(x, suppressMessages(get_na(x)), as.attr = T)
  # get former class attributes
  xc <- class(x)
  if (add.class)
    class(x) <- c(xc, "labelled")
  else
    class(x) <- "labelled"

  return(x)
}
