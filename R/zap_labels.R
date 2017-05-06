#' @title Drop, add or convert (non-)labelled values
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
#'         If \code{x} is a data frame, the complete data frame \code{x} will be
#'         returned, with variables specified in \code{...} being converted;
#'         if \code{...} is not specified, applies to all variables in the
#'         data frame.
#'
#'
#' @examples
#' # ------------------------
#' # zap_labels()
#' # ------------------------
#' data(efc)
#' str(efc$e42dep)
#'
#' x <- set_labels(
#'   efc$e42dep,
#'   labels = c("independent" = 1, "severe dependency" = 4)
#' )
#' table(x)
#' get_values(x)
#' str(x)
#'
#' # zap all labelled values
#' table(zap_labels(x))
#' get_values(zap_labels(x))
#' str(zap_labels(x))
#'
#' # zap all unlabelled values
#' table(zap_unlabelled(x))
#' get_values(zap_unlabelled(x))
#' str(zap_unlabelled(x))
#'
#' # in a pipe-workflow
#' library(dplyr)
#' efc %>%
#'   select(c172code, e42dep) %>%
#'   set_labels(
#'     e42dep,
#'     labels = c("independent" = 1, "severe dependency" = 4)
#'   ) %>%
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
#' x <- rec(efc$c160age, rec = rp$pattern)
#' # add value labels to new vector
#' x <- set_labels(x, labels = rp$labels)
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
  .dat <- get_dot_data(x, dplyr::quos(...))

  if (is.data.frame(x)) {
    # iterate variables of data frame
    for (i in colnames(.dat)) {
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
  .dat <- get_dot_data(x, dplyr::quos(...))

  if (is.data.frame(x)) {
    # iterate variables of data frame
    for (i in colnames(.dat)) {
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
#' @param x A \code{\link[haven]{labelled}} vector with \code{tagged_na}
#'            values, or a data frame with such vectors.
#'
#' @inheritParams to_factor
#'
#' @return \code{x}, where all \code{tagged_na} values are converted to \code{NA}.
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
  .dat <- get_dot_data(x, dplyr::quos(...))

  if (is.data.frame(x)) {
    # iterate variables of data frame
    for (i in colnames(.dat)) {
      x[[i]] <- zap_na_tags_helper(.dat[[i]])
    }
    # coerce to tibble
    x <- tibble::as_tibble(x)
  } else {
    x <- zap_na_tags_helper(.dat)
  }

  x
}



#' @title Convert infiite or NaN values into regular NA
#' @name zap_inf
#'
#' @description Replaces all infinite (\code{Inf} and \code{-Inf}) or \code{NaN}
#'                values with regular \code{NA}.
#'
#' @param x A vector or a data frame.
#'
#' @inheritParams to_factor
#'
#' @return \code{x}, where all \code{Inf}, \code{-Inf} and \code{NaN} are converted to \code{NA}.
#'
#' @examples
#' x <- c(1, 2, NA, 3, NaN, 4, NA, 5, Inf, -Inf, 6, 7)
#' zap_inf(x)
#'
#' data(efc)
#' # produce some NA and NaN values
#' efc$e42dep[1] <- NaN
#' efc$e42dep[2] <- NA
#' efc$c12hour[1] <- NaN
#' efc$c12hour[2] <- NA
#' efc$e17age[2] <- NaN
#' efc$e17age[1] <- NA
#'
#' # only zap NaN for c12hour
#' zap_inf(efc$c12hour)
#'
#' # only zap NaN for c12hour and e17age, not for e42dep,
#' # but return complete data framee
#' zap_inf(efc, c12hour, e17age)
#'
#' # zap NaN for complete data frame
#' zap_inf(efc)
#'
#' @importFrom tibble as_tibble
#' @export
zap_inf <- function(x, ...) {
  # evaluate arguments, generate data
  .dat <- get_dot_data(x, dplyr::quos(...))

  if (is.data.frame(x)) {
    # iterate variables of data frame
    for (i in colnames(.dat)) {
      # convert NaN and Inf to missing
      x[[i]][is.nan(x[[i]])] <- NA
      x[[i]][is.infinite(x[[i]])] <- NA
    }
    # coerce to tibble
    x <- tibble::as_tibble(x)
  } else {
    x[is.nan(x)] <- NA
    x[is.infinite(x)] <- NA
  }

  x
}



zap_labels_helper <- function(x) {
  x <- set_na(x, na = get_values(x, drop.na = T))

  # auto-detect variable label attribute
  attr.string <- getVarLabelAttribute(x)

  # remove label attributes
  if (!is.null(attr.string)) attr(x, attr.string) <- NULL
  if (is_labelled(x)) class(x) <- NULL

  x
}

zap_unlabelled_helper <- function(x) {
  vals <- get_values(x)
  x <- set_na(x, na = stats::na.omit(unique(x)[!unique(x) %in% vals]))
  if (is_labelled(x)) class(x) <- NULL
  x
}

zap_na_tags_helper <- function(x) {
  # check if values has only NA's
  if (sum(is.na(x)) == length(x)) return(x)
  # convert all NA, including tagged NA, into regular NA
  x[is.na(x)] <- NA
  # "remove" labels from tagged NA values
  set_labels(x, labels = get_labels(x, attr.only = T, include.values = "n", drop.na = T))
}
