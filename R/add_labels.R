#' @title Add, replace or remove value labels of variables
#' @name add_labels
#'
#' @description These functions add, replace or remove value labels to or from variables.
#'
#' @seealso \code{\link{set_label}} to manually set variable labels or
#'            \code{\link{get_label}} to get variable labels; \code{\link{set_labels}} to
#'            add value labels, replacing the existing ones (and removing non-specified
#'            value labels).
#'
#' @param value \describe{
#'          \item{For \code{add_labels()}}{A named (numeric) vector of labels
#'          that will be added to \code{x} as label attribute.}
#'          \item{For \code{remove_labels()}}{Either a numeric vector, indicating
#'          the position of one or more label attributes that should be removed;
#'          a character vector with names of label attributes that should be
#'          removed; or a \code{\link[haven]{tagged_na}} to remove the labels
#'          from specific NA values.}
#'          }
#'
#' @inheritParams to_factor
#'
#' @return \code{x} with additional or removed value labels. If \code{x}
#'           is a data frame, the complete data frame \code{x} will be returned,
#'           with removed or added to variables specified in \code{...};
#'           if \code{...} is not specified, applies to all variables in the
#'           data frame.
#'
#' @details \code{add_labels()} adds \code{value} to the existing value
#'          labels of \code{x}, however, unlike \code{\link{set_labels}}, it
#'          does \emph{not} remove labels that were \emph{not} specified in
#'          \code{value}. \code{add_labels()} also replaces existing
#'          value labels, but preserves the remaining labels.
#'          \cr \cr
#'          \code{remove_labels()} is the counterpart to \code{add_labels()}.
#'          It removes labels from a label attribute of \code{x}.
#'          \cr \cr
#'          \code{replace_labels()} is an alias for \code{add_labels()}.
#'
#' @examples
#' # ----------------------
#' # add_labels()
#' # ----------------------
#' data(efc)
#' get_labels(efc$e42dep)
#'
#' x <- add_labels(efc$e42dep, value = c(`nothing` = 5))
#' get_labels(x)
#'
#' library(dplyr)
#' x <- efc %>%
#'   # select three variables
#'   dplyr::select(e42dep, c172code, c161sex) %>%
#'   # only add new label to two of those
#'   add_labels(e42dep, c172code, value = c(`nothing` = 5))
#' # see data frame, with selected variables having new labels
#' get_labels(x)
#'
#' x <- add_labels(efc$e42dep, value = c(`nothing` = 5, `zero value` = 0))
#' get_labels(x, include.values = "p")
#'
#' # replace old value labels
#' x <- add_labels(
#'   efc$e42dep,
#'   values = c(`not so dependent` = 4, `lorem ipsum` = 5)
#' )
#' get_labels(x, include.values = "p")
#'
#' # replace specific missing value (tagged NA)
#' library(haven)
#' x <- labelled(c(1:3, tagged_na("a", "c", "z"), 4:1),
#'               c("Agreement" = 1, "Disagreement" = 4, "First" = tagged_na("c"),
#'                 "Refused" = tagged_na("a"), "Not home" = tagged_na("z")))
#' # get current NA values
#' x
#' # tagged NA(c) has currently the value label "First", will be
#' # replaced by "Second" now.
#' replace_labels(x, value = c("Second" = tagged_na("c")))
#'
#'
#' # ----------------------
#' # remove_labels()
#' # ----------------------
#' x <- remove_labels(efc$e42dep, value = 2)
#' get_labels(x, include.values = "p")
#'
#' x <- remove_labels(efc$e42dep, value = "independent")
#' get_labels(x, include.values = "p")
#'
#' library(haven)
#' x <- labelled(c(1:3, tagged_na("a", "c", "z"), 4:1),
#'               c("Agreement" = 1, "Disagreement" = 4, "First" = tagged_na("c"),
#'                 "Refused" = tagged_na("a"), "Not home" = tagged_na("z")))
#' # get current NA values
#' get_na(x)
#' get_na(remove_labels(x, value = tagged_na("c")))
#'
#' @importFrom tibble as_tibble
#' @export
add_labels <- function(x, ..., value) {
  # check for valid value. value must be a named vector
  if (is.null(value)) stop("`value` is NULL.", call. = F)
  if (is.null(names(value))) stop("`value` must be a named vector.", call. = F)

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
      x[[i]] <- add_labels_helper(.dat[[i]], value)
    }
    # coerce to tibble
    x <- tibble::as_tibble(x)
  } else {
    x <- add_labels_helper(.dat, value)
  }

  x
}

#' @importFrom haven is_tagged_na na_tag
add_labels_helper <- function(x, value) {
  # get current labels of `x`
  current.labels <- get_labels(x,
                               attr.only = T,
                               include.values = "n",
                               include.non.labelled = F,
                               drop.na = TRUE)

  # get current NA values
  current.na <- get_na(x)

  # if we had already labels, append new ones
  if (!is.null(current.labels)) {
    # remove double values labels
    doubles <- names(current.labels) %in% as.character(value)

    # switch value and names attribute, since get_labels
    # returns the values as names, and the value labels
    # as "vector content"
    val.switch <- as.numeric(names(current.labels))
    names(val.switch) <- as.character(current.labels)

    # update all labels
    all.labels <- c(val.switch[!doubles], value)
    # tell user
    if (any(doubles)) {
      message(sprintf("label '%s' was replaced with new value label.\n",
                      current.labels[doubles]))
    }
  } else {
    all.labels <- value
  }

  # replace tagged NA
  if (any(haven::is_tagged_na(value))) {
    # get tagged NAs
    value_tag <- haven::na_tag(value)[haven::is_tagged_na(value)]
    cna_tag <- haven::na_tag(current.na)

    # find matches (replaced NA)
    doubles <- na.omit(match(value_tag, cna_tag))
    if (any(doubles)) {
      message(sprintf("tagged NA '%s' was replaced with new value label.\n",
                      names(current.na)[doubles]))
    }

    # remove multiple tagged NA
    current.na <- current.na[-doubles]
  }

  # sort labels by values
  all.labels <- all.labels[order(as.numeric(all.labels))]

  # add NA
  if (!is.null(current.na)) all.labels <- c(all.labels, current.na)

  # set back labels
  x <- set_labels(x, labels = all.labels)
  return(x)
}


#' @rdname add_labels
#' @export
replace_labels <- function(x, ..., value) {
  add_labels(x = x, ..., value = value)
}
