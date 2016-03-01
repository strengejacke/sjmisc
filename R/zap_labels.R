#' @title Convert labelled values into NA
#' @name zap_labels
#'
#' @description For (partially) labelled vectors, all values that have
#'                a value label attribute will be replaced by \code{NA}.
#'
#' @param x (partially) \code{\link[haven]{labelled}} vector, \code{data.frame} or \code{list}
#'            of (partially) labelled vectors
#' @return \code{x}, where all labelled values are converted to \code{NA}.
#'
#' @seealso \code{\link{get_values}} and \code{\link{zap_unlabelled}};
#'          \code{\link{drop_labels}} to drop labels from zero-count values.
#'
#' @examples
#'
#' data(efc)
#' str(efc$e42dep)
#'
#' x <- set_labels(efc$e42dep,
#'                 c(`1` = "independent",
#'                   `4` = "severe dependency"))
#' table(x)
#' get_values(x)
#' str(x)
#'
#' # zap all labelled values
#' x <- set_labels(efc$e42dep,
#'                 c(`1` = "independent",
#'                   `4` = "severe dependency"))
#' table(zap_labels(x))
#' get_values(zap_labels(x))
#' str(zap_labels(x))
#'
#' # zap all unlabelled values
#' x <- set_labels(efc$e42dep,
#'                 c(`1` = "independent",
#'                   `4` = "severe dependency"))
#' table(zap_unlabelled(x))
#' get_values(zap_unlabelled(x))
#' str(zap_unlabelled(x))
#'
#' @importFrom stats na.omit
#' @export
zap_labels <- function(x) {
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # na all
    for (i in 1:nvars) x[[i]] <- zap_labels_helper(x[[i]])
    return(x)
  } else {
    return(zap_labels_helper(x))
  }
}


#' @title Convert non-labelled values into NA
#' @name zap_unlabelled
#'
#' @description For (partially) labelled vectors, all values that don't have
#'                a value label attribute will be replaced by \code{NA}.
#'
#' @inheritParams zap_labels
#' @return \code{x}, where all non-labelled values are converted to \code{NA}.
#'
#' @seealso \code{\link{get_values}} and \code{\link{zap_labels}};
#'          \code{\link{drop_labels}} to drop labels from zero-count values.
#'
#' @examples
#'
#' data(efc)
#' str(efc$e42dep)
#'
#' x <- set_labels(efc$e42dep,
#'                 c(`1` = "independent",
#'                   `4` = "severe dependency"))
#' table(x)
#' get_values(x)
#' str(x)
#'
#' # zap all labelled values
#' x <- set_labels(efc$e42dep,
#'                 c(`1` = "independent",
#'                   `4` = "severe dependency"))
#' table(zap_labels(x))
#' get_values(zap_labels(x))
#' str(zap_labels(x))
#'
#' # zap all unlabelled values
#' x <- set_labels(efc$e42dep,
#'                 c(`1` = "independent",
#'                   `4` = "severe dependency"))
#' table(zap_unlabelled(x))
#' get_values(zap_unlabelled(x))
#' str(zap_unlabelled(x))
#'
#' @importFrom stats na.omit
#' @export
zap_unlabelled <- function(x) {
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # na all
    for (i in 1:nvars) x[[i]] <- zap_unlabelled_helper(x[[i]])
    return(x)
  } else {
    return(zap_unlabelled_helper(x))
  }
}

zap_labels_helper <- function(x) {
  x <- set_na(x, get_values(x), as.attr = F)
  if (is_labelled(x)) class(x) <- NULL
  return(x)
}

zap_unlabelled_helper <- function(x) {
  vals <- get_values(x)
  x <- set_na(x, stats::na.omit(unique(x)[!unique(x) %in% vals]), as.attr = F)
  if (is_labelled(x)) class(x) <- NULL
  return(x)
}