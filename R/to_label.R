#' @title Convert variable into factor with associated value labels
#' @name to_label
#'
#' @description This function converts (replaces) values of a variable (also of factors
#'                or character vectors) with their associated value labels. Might
#'                be helpful for factor variables.
#'                For instance, if you have a Gender variable with 0/1 value, and associated
#'                labels are male/female, this function would convert all 0 to male and
#'                all 1 to female and returns the new variable as factor.
#'
#' @param add.non.labelled Logical, if \code{TRUE}, values without associated
#'          value label will also be converted to labels (as is). See 'Examples'.
#' @param prefix Logical, if \code{TRUE}, the value labels used as factor levels
#'          or character values will be prefixed with their associated values. See 'Examples'.
#' @param drop.na Logical, if \code{TRUE}, tagged \code{NA} values with value labels
#'          will be converted to regular NA's. Else, tagged \code{NA} values will be replaced
#'          with their value labels. See 'Examples' and \code{\link{get_na}}.
#' @param drop.levels Logical, if \code{TRUE}, unused factor levels will be
#'          dropped (i.e. \code{\link{droplevels}} will be applied before returning
#'          the result).
#'
#' @inheritParams to_factor
#' @inheritParams rec
#'
#' @return A factor with the associated value labels as factor levels. If \code{x}
#'           is a data frame, the complete data frame \code{x} will be returned,
#'           where variables specified in \code{...} are coerced to factors;
#'           if \code{...} is not specified, applies to all variables in the
#'           data frame.
#'
#' @note Value label attributes will be removed when converting variables to factors.
#'       This function is kept for backwards-compatibility. It is preferred to
#'       use \code{\link[sjlabelled]{as_label}}.
#'
#' @examples
#' library(sjlabelled)
#' data(efc)
#' print(get_labels(efc)['c161sex'])
#' head(efc$c161sex)
#' head(to_label(efc$c161sex))
#'
#' print(get_labels(efc)['e42dep'])
#' table(efc$e42dep)
#' table(to_label(efc$e42dep))
#'
#' head(efc$e42dep)
#' head(to_label(efc$e42dep))
#'
#' # structure of numeric values won't be changed
#' # by this function, it only applies to labelled vectors
#' # (typically categorical or factor variables)
#' str(efc$e17age)
#' str(to_label(efc$e17age))
#'
#'
#' # factor with non-numeric levels
#' to_label(factor(c("a", "b", "c")))
#'
#' # factor with non-numeric levels, prefixed
#' x <- factor(c("a", "b", "c"))
#' x <- set_labels(x, labels = c("ape", "bear", "cat"))
#' to_label(x, prefix = TRUE)
#'
#'
#' # create vector
#' x <- c(1, 2, 3, 2, 4, NA)
#' # add less labels than values
#' x <- set_labels(x,
#'                 labels = c("yes", "maybe", "no"),
#'                 force.labels = FALSE,
#'                 force.values = FALSE)
#' # convert to label w/o non-labelled values
#' to_label(x)
#' # convert to label, including non-labelled values
#' to_label(x, add.non.labelled = TRUE)
#'
#'
#' # create labelled integer, with missing flag
#' library(haven)
#' x <- labelled(c(1:3, tagged_na("a", "c", "z"), 4:1, 2:3),
#'               c("Agreement" = 1, "Disagreement" = 4, "First" = tagged_na("c"),
#'                 "Refused" = tagged_na("a"), "Not home" = tagged_na("z")))
#' # to labelled factor, with missing labels
#' to_label(x, drop.na = FALSE)
#' # to labelled factor, missings removed
#' to_label(x, drop.na = TRUE)
#' # keep missings, and use non-labelled values as well
#' to_label(x, add.non.labelled = TRUE, drop.na = FALSE)
#'
#'
#' # convert labelled character to factor
#' dummy <- c("M", "F", "F", "X")
#' dummy <- set_labels(
#'   dummy,
#'   labels = c(`M` = "Male", `F` = "Female", `X` = "Refused")
#' )
#' get_labels(dummy,, "p")
#' to_label(dummy)
#'
#' # drop unused factor levels, but preserve variable label
#' x <- factor(c("a", "b", "c"), levels = c("a", "b", "c", "d"))
#' x <- set_labels(x, labels = c("ape", "bear", "cat"))
#' set_label(x) <- "A factor!"
#' x
#' to_label(x, drop.levels = TRUE)
#'
#' # change variable label
#' to_label(x, var.label = "New variable label!", drop.levels = TRUE)
#'
#'
#' # easily coerce specific variables in a data frame to factor
#' # and keep other variables, with their class preserved
#' to_label(efc, e42dep, e16sex, c172code)
#'
#' @export
to_label <- function(x, ..., add.non.labelled = FALSE, prefix = FALSE, var.label = NULL, drop.na = TRUE, drop.levels = FALSE) {
  # evaluate arguments, generate data
  .dat <- get_dot_data(x, dplyr::quos(...))

  if (is.data.frame(x)) {
    # iterate variables of data frame
    for (i in colnames(.dat)) {
      x[[i]] <- to_label_helper(.dat[[i]], add.non.labelled, prefix, var.label, drop.na, drop.levels)
    }
    # coerce to tibble
    x <- tibble::as_tibble(x)
  } else {
    x <- to_label_helper(.dat, add.non.labelled, prefix, var.label, drop.na, drop.levels)
  }

  x
}

#' @importFrom haven na_tag
to_label_helper <- function(x, add.non.labelled, prefix, var.label, drop.na, drop.levels) {
  # prefix labels?
  if (prefix)
    iv <- "p"
  else
    iv <- 0

  # retrieve variable label
  if (is.null(var.label))
    var_lab <- sjlabelled::get_label(x)
  else
    var_lab <- var.label

  # keep missings?
  if (!drop.na) {
    # get NA
    current.na <- sjlabelled::get_na(x)
    # any NA?
    if (!is.null(current.na)) {
      # we have to set all NA labels at once, else NA loses tag
      # so we prepare a dummy label-vector, where we copy all different
      # NA labels to `x` afterwards
      dummy_na <- rep("", times = length(x))
      # iterare NA
      for (i in seq_len(length(current.na))) {
        dummy_na[haven::na_tag(x) == haven::na_tag(current.na[i])] <- names(current.na)[i]
      }
      x[haven::is_tagged_na(x)] <- dummy_na[haven::is_tagged_na(x)]
    }
  } else {
    # in case x has tagged NA's we need to be sure to convert
    # those into regular NA's, because else saving would not work
    x[is.na(x)] <- NA
  }

  # get value labels
  vl <-
    sjlabelled::get_labels(
      x,
      attr.only = TRUE,
      include.values = iv,
      include.non.labelled = add.non.labelled,
      drop.na = drop.na
    )

  # check if we have any labels, else
  # return variable "as is"
  if (!is.null(vl)) {
    # get associated values for value labels
    vnn <-
      sjlabelled::get_labels(
        x,
        attr.only = TRUE,
        include.values = "n",
        include.non.labelled = add.non.labelled,
        drop.na = drop.na
      )

    # convert to numeric
    vn <- suppressWarnings(as.numeric(names(vnn)))
    # where some values non-numeric? if yes,
    # use value names as character values
    if (anyNA(vn)) vn <- names(vnn)

    # replace values with labels
    if (is.factor(x)) {
      # more levels than labels?
      remain_labels <- levels(x)[!levels(x) %in% vn]
      # set new levels
      levels(x) <- c(vl, remain_labels)
      # remove attributes
      x <- sjlabelled::remove_all_labels(x)
    } else {
      for (i in seq_len(length(vl))) x[x == vn[i]] <- vl[i]
      # to factor
      x <- factor(x, levels = unique(vl))
    }
  }
  # drop unused levels?
  if (drop.levels) x <- droplevels(x)

  # set back variable labels
  if (!is.null(var_lab)) x <- suppressWarnings(sjlabelled::set_label(x, label = var_lab))

  # return as factor
  x
}


#' @title Convert variable into character vector and replace values with associated value labels
#' @name to_character
#'
#' @description This function converts (replaces) variable values (also of factors
#'                or character vectors) with their associated value labels and returns
#'                them as character vector. This is just a convenient wrapper for
#'                \code{as.character(to_label(x))}.
#'
#' @inheritParams to_label
#'
#' @note Value and variable label attributes (see, for instance, \code{\link{get_labels}}
#'         or \code{\link{set_labels}}) will be removed  when converting variables to factors.
#'
#' @return A character vector with the associated value labels as values. If \code{x}
#'           is a data frame, the complete data frame \code{x} will be returned,
#'           where variables specified in \code{...} are coerced
#'           to character variables;
#'           if \code{...} is not specified, applies to all variables in the
#'           data frame.
#'
#' @examples
#' library(sjlabelled)
#' data(efc)
#' print(get_labels(efc)['c161sex'])
#' head(efc$c161sex)
#' head(to_character(efc$c161sex))
#'
#' print(get_labels(efc)['e42dep'])
#' table(efc$e42dep)
#' table(to_character(efc$e42dep))
#'
#' head(efc$e42dep)
#' head(to_character(efc$e42dep))
#'
#' # numeric values w/o value labels will also be converted into character
#' str(efc$e17age)
#' str(to_character(efc$e17age))
#'
#'
#' # factor with non-numeric levels, non-prefixed and prefixed
#' x <- factor(c("a", "b", "c"))
#' x <- set_labels(x, labels = c("ape", "bear", "cat"))
#'
#' to_character(x, prefix = FALSE)
#' to_character(x, prefix = TRUE)
#'
#'
#' # create vector
#' x <- c(1, 2, 3, 2, 4, NA)
#' # add less labels than values
#' x <- set_labels(x,
#'                 labels = c("yes", "maybe", "no"),
#'                 force.labels = FALSE,
#'                 force.values = FALSE)
#' # convert to character w/o non-labelled values
#' to_character(x)
#' # convert to character, including non-labelled values
#' to_character(x, add.non.labelled = TRUE)
#'
#'
#' # create labelled integer, with missing flag
#' library(haven)
#' x <- labelled(c(1:3, tagged_na("a", "c", "z"), 4:1, 2:3),
#'               c("Agreement" = 1, "Disagreement" = 4, "First" = tagged_na("c"),
#'                 "Refused" = tagged_na("a"), "Not home" = tagged_na("z")))
#' # to character, with missing labels
#' to_character(x, drop.na = FALSE)
#' # to character, missings removed
#' to_character(x, drop.na = TRUE)
#' # keep missings, and use non-labelled values as well
#' to_character(x, add.non.labelled = TRUE, drop.na = FALSE)
#'
#'
#' # easily coerce specific variables in a data frame to character
#' # and keep other variables, with their class preserved
#' to_character(efc, e42dep, e16sex, c172code)
#'
#' @export
to_character <- function(x, ..., add.non.labelled = FALSE, prefix = FALSE, var.label = NULL, drop.na = TRUE, drop.levels = FALSE) {
  # evaluate arguments, generate data
  .dat <- get_dot_data(x, dplyr::quos(...))

  if (is.data.frame(x)) {

    # iterate variables of data frame
    for (i in colnames(.dat)) {
      x[[i]] <- as.character(to_label_helper(.dat[[i]], add.non.labelled, prefix, var.label, drop.na, drop.levels))
    }
    # coerce to tibble
    x <- tibble::as_tibble(x)
  } else {
    x <- as.character(to_label_helper(.dat, add.non.labelled, prefix, var.label, drop.na, drop.levels))
  }

  x
}
