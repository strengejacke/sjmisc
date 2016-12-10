#' @title Drop, add or convert (non-)labelled values to vectors
#' @name zap_labels
#'
#' @description For (partially) labelled vectors, \code{zap_labels()} will replace
#'                all values that have a value label attribute by \code{NA};
#'                \code{zap_unlabelled()}, as counterpart, will replace all values
#'                that \emph{don't} have a value label attribute by \code{NA}.
#'                \cr \cr
#'                \code{drop_labels()} drops all value labels for unused values that have
#'                no cases (counts) in a vector. \code{fill_labels()} is the
#'                counterpart to \code{drop_labels()} and adds value labels to
#'                a partially labelled vector, i.e. if not all values are
#'                labelled, non-labelled values get labels.
#'
#' @param x (partially) \code{\link[haven]{labelled}} vector or a data frame
#'          with such vectors.
#'
#' @inheritParams to_factor
#' @inheritParams set_labels
#'
#' @return \itemize{
#'           \item For \code{zap_labels()}, \code{x}, where all labelled values are converted to \code{NA}.
#'           \item For \code{zap_unlabelled()}, \code{x}, where all non-labelled values are converted to \code{NA}.
#'           \item For \code{drop_labels()}, \code{x}, where value labels for non-existing values are removed.
#'           \item For \code{fill_labels()}, \code{x}, where labels for non-labelled values are added.
#'         }
#'
#' @examples
#' # ------------------------
#' # zap_labels()
#' # ------------------------
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
#' # in a pipe-workflow
#' library(dplyr)
#' set_labels(efc$e42dep) <-  c(`1` = "independent", `4` = "severe dependency")
#' efc %>%
#'   select(c172code, e42dep) %>%
#'   zap_labels()
#'
#' # ------------------------
#' # drop_labels()
#' # ------------------------
#' rp <- rec_pattern(1, 100)
#' rp
#'
#' # sample data
#' data(efc)
#' # recode carers age into groups of width 5
#' x <- rec(efc$c160age, recodes = rp$pattern)
#' # add value labels to new vector
#' set_labels(x) <- rp$labels
#'
#' # watch result. due to recode-pattern, we have age groups with
#' # no observations (zero-counts)
#' frq(x)
#' # now, let's drop zero's
#' frq(drop_labels(x))
#'
#' # drop labels, also drop NA value labels, then also zap tagged NA
#' library(haven)
#' x <- labelled(c(1:3, tagged_na("z"), 4:1),
#'               c("Agreement" = 1, "Disagreement" = 4, "Unused" = 5,
#'                 "Not home" = tagged_na("z")))
#' x
#' drop_labels(x, drop.na = FALSE)
#' drop_labels(x)
#' zap_na_tags(drop_labels(x))
#'
#' # ------------------------
#' # fill_labels()
#' # ------------------------
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
#' @importFrom stats na.omit
#' @export
zap_labels <- function(x, ...) {
  # evaluate arguments, generate data
  .dots <- match.call(expand.dots = FALSE)$`...`
  .dat <- get_dot_data(x, .dots)

  # get variable names
  .vars <- dot_names(.dots)

  # if user only provided a data frame, get all variable names
  if (is.null(.vars) && is.data.frame(x)) .vars <- colnames(x)

  # if we have any dot names, we definitely have a data frame
  if (!is.null(.vars)) {
    # iterate variables of data frame
    for (i in .vars) {
      x[[i]] <- zap_labels_helper(.dat[[i]])
    }
    # coerce to tibble
    x <- tibble::as_tibble(x)
  } else {
    x <- zap_labels_helper(.dat)
  }

  x
}


#' @rdname zap_labels
#' @importFrom stats na.omit
#' @export
zap_unlabelled <- function(x, ...) {
  # evaluate arguments, generate data
  .dots <- match.call(expand.dots = FALSE)$`...`
  .dat <- get_dot_data(x, .dots)

  # get variable names
  .vars <- dot_names(.dots)

  # if user only provided a data frame, get all variable names
  if (is.null(.vars) && is.data.frame(x)) .vars <- colnames(x)

  # if we have any dot names, we definitely have a data frame
  if (!is.null(.vars)) {
    # iterate variables of data frame
    for (i in .vars) {
      x[[i]] <- zap_unlabelled_helper(.dat[[i]])
    }
    # coerce to tibble
    x <- tibble::as_tibble(x)
  } else {
    x <- zap_unlabelled_helper(.dat)
  }

  x
}


#' @title Convert tagged NA values into regular NA
#' @name zap_na_tags
#'
#' @description Replaces all \code{\link[haven]{tagged_na}} values with
#'                regular \code{NA}.
#'
#' @param x A \code{\link[haven]{labelled}} vector with \code{\link[haven]{tagged_na}}
#'            values, or a data frame with such vectors.
#'
#' @inheritParams to_factor
#'
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
zap_na_tags <- function(x, ...) {
  # evaluate arguments, generate data
  .dots <- match.call(expand.dots = FALSE)$`...`
  .dat <- get_dot_data(x, .dots)

  # get variable names
  .vars <- dot_names(.dots)

  # if user only provided a data frame, get all variable names
  if (is.null(.vars) && is.data.frame(x)) .vars <- colnames(x)

  # if we have any dot names, we definitely have a data frame
  if (!is.null(.vars)) {
    # iterate variables of data frame
    for (i in .vars) {
      x[[i]] <- zap_na_tags_helper(.dat[[i]])
    }
    # coerce to tibble
    x <- tibble::as_tibble(x)
  } else {
    x <- zap_na_tags_helper(.dat)
  }

  x
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
