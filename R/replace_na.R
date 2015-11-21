#' @title Replace NA with specific values
#' @name replace_na
#'
#' @description This function replaces NA's of a variable, data frame
#'                or list of variables with \code{value}.
#'
#' @seealso \code{\link{set_na}} for setting \code{NA} values, \code{\link{rec}}
#'            for general recoding of variables and \code{\link{recode_to}}
#'            for re-shifting value ranges.
#'
#' @param x Variable (vector), \code{data.frame} or \code{list} of variables where
#'          missing values should be replaced with \code{value}.
#' @param value Value that will replace the \code{\link{NA}}'s.
#' @param na.label Optional character vector, used to label the NA-value (i.e.
#'          adding a \code{labels} attribute to \code{x}).
#'
#' @return \code{x}, where \code{NA}'s are replaced with \code{value}.
#'
#' @note Value and variable label attributes (see, for instance, \code{\link{get_labels}}
#'         or \code{\link{set_labels}}) are preserved.
#'
#' @examples
#' data(efc)
#' table(efc$e42dep, exclude = NULL)
#' table(replace_na(efc$e42dep, 99), exclude = NULL)
#'
#' dummy <- list(efc$c82cop1, efc$c83cop2, efc$c84cop3)
#' # show original distribution
#' lapply(dummy, table, exclude = NULL)
#' # show variables, NA's replaced with 99
#' lapply(replace_na(dummy, 99), table, exclude = NULL)
#'
#' @export
replace_na <- function(x, value, na.label = NULL) {
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
    for (i in 1:nvars) x[[i]] <- replace_na_helper(x[[i]], value, na.label)
    return(x)
  } else {
    return(replace_na_helper(x, value, na.label))
  }
  return(x)
}


replace_na_helper <- function(x, value, na.label) {
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
    x[is.na(x)] <- value
    # add NA label
    if (!is.null(na.label)) add_labels(x) <- na.vec
  } else {
    message("`x` has no missings.")
  }
  return(x)
}


#' @rdname replace_na
#' @export
`replace_na<-` <- function(x, na.label = NULL, value) {
  UseMethod("replace_na<-")
}

#' @export
`replace_na<-.default` <- function(x, na.label = NULL, value) {
  x <- replace_na(x = x, value = value, na.label = na.label)
  x
}
