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
#' x <- set_labels(efc$e42dep, c(`1` = "independent", `4` = "severe dependency"))
#' table(x)
#' get_values(x)
#' str(x)
#'
#' # zap all labelled values
#' x <- set_labels(efc$e42dep, c(`1` = "independent", `4` = "severe dependency"))
#' table(zap_labels(x))
#' get_values(zap_labels(x))
#' str(zap_labels(x))
#'
#' # zap all unlabelled values
#' x <- set_labels(efc$e42dep, c(`1` = "independent", `4` = "severe dependency"))
#' table(zap_unlabelled(x))
#' get_values(zap_unlabelled(x))
#' str(zap_unlabelled(x))
#'
#' @importFrom stats na.omit
#' @export
zap_labels <- function(x) {
  UseMethod("zap_labels")
}

#' @export
zap_labels.data.frame <- function(x) {
  tibble::as_tibble(lapply(x, FUN = zap_labels_helper))
}

#' @export
zap_labels.list <- function(x) {
  lapply(x, FUN = zap_labels_helper)
}

#' @export
zap_labels.default <- function(x) {
  zap_labels_helper(x)
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
#' x <- set_labels(efc$e42dep, c(`1` = "independent", `4` = "severe dependency"))
#' table(x)
#' get_values(x)
#' str(x)
#'
#' # zap all labelled values
#' x <- set_labels(efc$e42dep, c(`1` = "independent", `4` = "severe dependency"))
#' table(zap_labels(x))
#' get_values(zap_labels(x))
#' str(zap_labels(x))
#'
#' # zap all unlabelled values
#' x <- set_labels(efc$e42dep, c(`1` = "independent", `4` = "severe dependency"))
#' table(zap_unlabelled(x))
#' get_values(zap_unlabelled(x))
#' str(zap_unlabelled(x))
#'
#' @importFrom stats na.omit
#' @export
zap_unlabelled <- function(x) {
  UseMethod("zap_unlabelled")
}

#' @export
zap_unlabelled.data.frame <- function(x) {
  tibble::as_tibble(lapply(x, FUN = zap_unlabelled_helper))
}

#' @export
zap_unlabelled.list <- function(x) {
  lapply(x, FUN = zap_unlabelled_helper)
}

#' @export
zap_unlabelled.default <- function(x) {
  zap_unlabelled_helper(x)
}


#' @title Convert tagged NA values into regular NA
#' @name zap_na_tags
#'
#' @description Replaces all \code{\link[haven]{tagged_na}} values into
#'                regular \code{NA}.
#'
#' @param x \code{\link[haven]{labelled}} vector, \code{data.frame} or \code{list}
#'            of labelled vectors with \code{\link[haven]{tagged_na}} values.
#' @return \code{x}, where all \code{\link[haven]{tagged_na}} values are converted to \code{NA}.
#'
#' @seealso \code{\link{set_na}} and \code{\link{get_na}};
#'          \code{\link{drop_labels}} to drop labels from zero-count values.
#'
#' @examples
#' library(haven)
#' x <- labelled(c(1:3, tagged_na("a", "c", "z"), 4:1),
#'               c("Agreement" = 1, "Disagreement" = 4, "First" = tagged_na("c"),
#'                 "Refused" = tagged_na("a"), "Not home" = tagged_na("z")))
#' # get current NA values
#' x
#' get_na(x)
#' zap_na_tags(x)
#' get_na(zap_na_tags(x))
#'
#' # also works with non-labelled vector that have tagged NA values
#' x <- c(1:5, tagged_na("a"), tagged_na("z"), NA)
#' haven::print_tagged_na(x)
#' haven::print_tagged_na(zap_na_tags(x))
#'
#' @importFrom stats na.omit
#' @export
#' @export
zap_na_tags <- function(x) {
  UseMethod("zap_na_tags")
}

#' @export
zap_na_tags.data.frame <- function(x) {
  tibble::as_tibble(lapply(x, FUN = zap_na_tags_helper))
}

#' @export
zap_na_tags.list <- function(x) {
  lapply(x, FUN = zap_na_tags_helper)
}

#' @export
zap_na_tags.default <- function(x) {
  zap_na_tags_helper(x)
}


zap_labels_helper <- function(x) {
  x <- set_na(x, get_values(x, drop.na = T))
  # auto-detect variable label attribute
  attr.string <- getVarLabelAttribute(x)
  # remove label attributes
  if (!is.null(attr.string)) attr(x, attr.string) <- NULL
  if (haven::is.labelled(x)) class(x) <- NULL
  return(x)
}

zap_unlabelled_helper <- function(x) {
  vals <- get_values(x)
  x <- set_na(x, stats::na.omit(unique(x)[!unique(x) %in% vals]))
  if (haven::is.labelled(x)) class(x) <- NULL
  return(x)
}

zap_na_tags_helper <- function(x) {
  # convert all NA, including tagged NA, into regular NA
  x[is.na(x)] <- NA
  # "remove" labels from tagged NA values
  set_labels(x) <- get_labels(x, attr.only = T, include.values = "n", drop.na = T)
  return(x)
}