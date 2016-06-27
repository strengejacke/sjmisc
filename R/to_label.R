#' @title Convert variable into factor and replace values with associated value labels
#' @name to_label
#'
#' @description This function converts (replaces) variable values (also of factors
#'                or character vectors) with their associated value labels. Might
#'                be helpful for factor variables.
#'                For instance, if you have a Gender variable with 0/1 value, and associated
#'                labels are male/female, this function would convert all 0 to male and
#'                all 1 to female and returns the new variable as \code{\link{factor}}.
#'
#' @seealso \code{\link{to_factor}} to convert a numeric variable into a factor (and
#'            preserve labels); \code{\link{to_value}} to convert a factor into
#'            a numeric variable and \code{\link{to_character}} to convert a
#'            labelled vector into a character vector (using label attributes as
#'            values).
#'
#' @param x A labelled vector (see \code{\link{set_labels}}),
#'          respectively a data frame with such variables.
#' @param add.non.labelled logical, if \code{TRUE}, values without associated
#'          value label will also be converted to labels (as is). See 'Examples'.
#' @param prefix Logical, if \code{TRUE}, the value labels used as factor levels
#'          or character values will be prefixed with their associated values. See 'Examples'.
#' @param drop.na Logical, if \code{TRUE}, tagged \code{NA} values with value labels
#'          will be converted to normal NA's. Else, tagged \code{NA} values will be replaced
#'          with their value labels. See 'Examples' and \code{\link{get_na}}.
#' @return A factor variable with the associated value labels as factor levels, or a
#'           data frame with such factor variables (if \code{x} was a data frame).
#'
#' @note Value and variable label attributes (see, for instance, \code{\link{get_labels}}
#'         or \code{\link{set_labels}}) will be removed  when converting variables to factors.
#'
#' @details See 'Details' in \code{\link{get_na}}.
#'
#' @examples
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
#' set_labels(x) <- c("ape", "bear", "cat")
#' to_label(x, prefix = TRUE)
#'
#'
#' # create vector
#' x <- c(1, 2, 3, 2, 4, NA)
#' # add less labels than values
#' x <- set_labels(x, c("yes", "maybe", "no"),
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
#' set_labels(dummy) <- c(`M` = "Male", `F` = "Female", `X` = "Refused")
#' get_labels(dummy,, "p")
#' to_label(dummy)
#'
#' @export
to_label <- function(x, add.non.labelled = FALSE, prefix = FALSE, drop.na = TRUE) {
  if (is.matrix(x) || is.data.frame(x)) {
    for (i in 1:ncol(x)) {
      x[[i]] <- to_label_helper(x[[i]], add.non.labelled, prefix, drop.na)
    }
    return(x)
  } else {
    return(to_label_helper(x, add.non.labelled, prefix, drop.na))
  }
}

#' @importFrom haven na_tag
to_label_helper <- function(x, add.non.labelled, prefix, drop.na) {
  # prefix labels?
  if (prefix)
    iv <- "p"
  else
    iv <- 0
  # keep missings?
  if (!drop.na) {
    # get NA
    current.na <- get_na(x)
    # any NA?
    if (!is.null(current.na)) {
      # we have to set all NA labels at once, else NA loses tag
      # so we prepare a dummy label-vector, where we copy all different
      # NA labels to `x` afterwards
      dummy_na <- rep("", times = length(x))
      # iterare NA
      for (i in 1:length(current.na)) {
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
  vl <- get_labels(x, attr.only = TRUE, include.values = iv,
                   include.non.labelled = add.non.labelled,
                   drop.na = drop.na)
  # check if we have any labels, else
  # return variable "as is"
  if (!is.null(vl)) {
    # get associated values for value labels
    vnn <- get_labels(x, attr.only = TRUE, include.values = "n",
                      include.non.labelled = add.non.labelled,
                      drop.na = drop.na)

    # convert to numeric
    vn <- suppressWarnings(as.numeric(names(vnn)))
    # where some values non-numeric? if yes,
    # use value names as character values
    if (anyNA(vn)) vn <- names(vnn)

    # replace values with labels
    if (is.factor(x)) {
      # set new levels
      levels(x) <- vl
      # remove attributes
      x <- remove_all_labels(x)
    } else {
      for (i in 1:length(vl)) x[x == vn[i]] <- vl[i]
      # to factor
      x <- factor(x, levels = unique(vl))
    }
  }
  # return as factor
  return(x)
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
#' @return A character vector with the associated value labels as values, or a
#'           data frame with such factor variables (if \code{x} was a data frame).
#'
#' @details See 'Details' in \code{\link{get_na}}.
#'
#' @examples
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
#' set_labels(x) <- c("ape", "bear", "cat")
#'
#' to_character(x, prefix = FALSE)
#' to_character(x, prefix = TRUE)
#'
#'
#' # create vector
#' x <- c(1, 2, 3, 2, 4, NA)
#' # add less labels than values
#' x <- set_labels(x, c("yes", "maybe", "no"),
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
#' @export
to_character <- function(x, add.non.labelled = FALSE, prefix = FALSE, drop.na = TRUE) {
  if (is.matrix(x) || is.data.frame(x)) {
    for (i in 1:ncol(x)) {
      x[[i]] <- as.character(to_label_helper(x[[i]], add.non.labelled, prefix, drop.na))
    }
    return(x)
  } else {
    return(as.character(to_label_helper(x, add.non.labelled, prefix, drop.na)))
  }
}
