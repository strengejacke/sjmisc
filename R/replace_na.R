#' @title Replace NA with specific values
#' @name replace_na
#'
#' @description This function replaces (tagged) NA's of a variable, data frame
#'                or list of variables with \code{value}.
#'
#' @seealso \code{\link{set_na}} for setting \code{NA} values, \code{\link{rec}}
#'            for general recoding of variables and \code{\link{recode_to}}
#'            for re-shifting value ranges.
#'
#' @param x Variable (vector), \code{data.frame} or \code{list} of variables where
#'          missing values should be replaced with \code{value}.
#' @param value Value that will replace the \code{\link{NA}}'s.
#' @param na.label Optional character vector, used to label the the former NA-value
#'          (i.e. adding a \code{labels} attribute for \code{value} to \code{x}).
#' @param tagged.na Optional, specifies a \code{\link[haven]{tagged_na}} value
#'          that will be replaced by \code{value}. Herewith it is possible
#'          to replace only specific \code{NA} values of \code{x}.
#'
#' @return \code{x}, where \code{NA}'s are replaced with \code{value}.
#'
#' @note Value and variable label attributes (see, for instance, \code{\link{get_labels}}
#'         or \code{\link{set_labels}}) are preserved.
#'
#' @details While regular \code{NA} values can only be completely replaced with
#'            a single value, \code{\link[haven]{tagged_na}} allows to differentiate
#'            between different qualitative values of \code{NA}s.
#'            Tagged \code{NA}s work exactly like regular R missing values
#'            except that they store one additional byte of information: a tag,
#'            which is usually a letter ("a" to "z") or character number ("0" to "9").
#'            Therewith it is possible to replace only specific NA values, while
#'            other NA values with be preserved.
#'
#' @examples
#' data(efc)
#' table(efc$e42dep, exclude = NULL)
#' table(replace_na(efc$e42dep, 99), exclude = NULL)
#'
#' # the original labels
#' get_labels(replace_na(efc$e42dep, 99))
#' # NA becomes "99", and is labelled as "former NA"
#' get_labels(replace_na(efc$e42dep, 99, na.label = "former NA"),
#'            include.values = "p")
#'
#' dummy <- list(efc$c82cop1, efc$c83cop2, efc$c84cop3)
#' # show original distribution
#' lapply(dummy, table, exclude = NULL)
#' # show variables, NA's replaced with 99
#' lapply(replace_na(dummy, 99), table, exclude = NULL)
#'
#' library(haven)
#' x <- labelled(c(1:3, tagged_na("a", "c", "z"), 4:1),
#'               c("Agreement" = 1, "Disagreement" = 4, "First" = tagged_na("c"),
#'                 "Refused" = tagged_na("a"), "Not home" = tagged_na("z")))
#' # get current NA values
#' x
#' get_na(x)
#'
#' # replace only the NA, which is tagged as NA(c)
#' replace_na(x, 2, tagged.na = "c")
#' get_na(replace_na(x, 2, tagged.na = "c"))
#'
#' table(x)
#' table(replace_na(x, 2, tagged.na = "c"))
#'
#' @export
replace_na <- function(x, value, na.label = NULL, tagged.na = NULL) {
  # check for valid value
  if (is.null(value) || is.na(value)) return(x)
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # replace NA
    for (i in 1:nvars) x[[i]] <- replace_na_helper(x[[i]], value, na.label, tagged.na)
    return(x)
  } else {
    return(replace_na_helper(x, value, na.label, tagged.na))
  }
  return(x)
}


replace_na_helper <- function(x, value, na.label, tagged.na) {
  # create named vector, for labelleing
  if (!is.null(na.label)) {
    na.vec <- value
    names(na.vec) <- as.character(na.label)
  }
  if (anyNA(x)) {
    # do we have a factor? then check for levels
    if (is.factor(x)) {
      # is value in levels?
      if (!any(levels(x) %in% as.character(value))) {
        # if not, add value to levels
        levels(x) <- c(levels(x), as.character(value))
      }
    }
    # check if we have tagged NA
    if (!is.null(tagged.na)) {
      # coerce to tagged NA
      if (!haven::is_tagged_na(tagged.na)) tagged.na <- haven::tagged_na(tagged.na)
      # replace tagged NA
      x[which(haven::na_tag(x) == haven::na_tag(tagged.na))] <- value
      # remove label
      remove_labels(x) <- tagged.na
    } else {
      x[is.na(x)] <- value
    }
    # add NA label
    if (!is.null(na.label)) add_labels(x) <- na.vec
  } else {
    message("`x` has no missings.")
  }
  return(x)
}


#' @rdname replace_na
#' @export
`replace_na<-` <- function(x, na.label = NULL, tagged.na = NULL, value) {
  UseMethod("replace_na<-")
}

#' @export
`replace_na<-.default` <- function(x, na.label = NULL, tagged.na = NULL, value) {
  x <- replace_na(x = x, value = value, na.label = na.label, tagged.na = tagged.na)
  x
}
