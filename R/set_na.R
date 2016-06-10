#' @title Set NA for specific variable values
#' @name set_na
#'
#' @description This function sets specific values of a variable, data frame
#'                or list of variables as missings (\code{NA}).
#'
#' @seealso \code{\link{replace_na}} to replace \code{\link{NA}}'s with specific
#'            values, \code{\link{rec}} for general recoding of variables and
#'            \code{\link{recode_to}} for re-shifting value ranges. See
#'            \code{\link{get_na}} to get values of missing values in
#'            labelled vectors and \code{\link{to_na}} to convert missing value
#'            codes into \code{NA}.
#'
#' @param x Variable (vector), \code{data.frame} or \code{list} of variables where new
#'          missing values should be defined. If \code{x} is a \code{data.frame}, each
#'          column is assumed to be a new variable, where missings should be defined.
#' @param value Numeric vector with values that should be replaced with \code{\link{NA}}'s.
#'          Thus, for each variable in \code{x}, \code{value} are replaced by \code{NA}'s.
#'          Or: a logical vector describing which values should be translated
#'          to missing values. See 'Details' and 'Examples'.
#' @param as.attr Logical, if \code{TRUE}, \code{value} of \code{x} will \strong{not}
#'          be converted to \code{NA}. Rather, the missing code values of \code{value}
#'          will be added as missing-attribute \code{is_na} to the vector. See
#'          \code{\link{labelled}} for more details, and 'Examples'.
#'
#' @return \code{x}, where each value of \code{value} is replaced by an \code{NA}.
#'
#' @note Value and variable label attributes (see, for instance, \code{\link{get_labels}}
#'         or \code{\link{set_labels}}) are preserved.
#'
#' @details \code{set_na} converts those values to \code{NA} that are
#'            specified in the function's \code{value} argument; hence,
#'            by default, \code{set_na} ignores any missing code attributes
#'            like \code{is_na}. \code{\link{to_na}}, by contrast, converts
#'            values to \code{NA}, which are defined as missing through the
#'            \code{is_na}-attribute of a vector (see \code{\link{labelled}}).
#'            \cr \cr
#'            If \code{as.attr = TRUE}, \code{value} in \code{x} will \strong{not}
#'            be converted to \code{NA}. Instead, the attribute \code{is_na}
#'            will be added to \code{x}, indicating which values should be coded
#'            as missing. \code{value} may either be numeric, with each number
#'            indicating a value that should be defined as missing; or a vector
#'            of logicals, describing which values should be translated to
#'            missing values (see 'Examples').
#'            \cr \cr
#'            Furthermore, see 'Details' in \code{\link{get_na}}.
#'
#' @examples
#' # create random variable
#' dummy <- sample(1:8, 100, replace = TRUE)
#' # show value distribution
#' table(dummy)
#' # set value 1 and 8 as missings
#' dummy <- set_na(dummy, c(1, 8))
#' # show value distribution, including missings
#' table(dummy, exclude = NULL)
#'
#' # create sample data frame
#' dummy <- data.frame(var1 = sample(1:8, 100, replace = TRUE),
#'                     var2 = sample(1:10, 100, replace = TRUE),
#'                     var3 = sample(1:6, 100, replace = TRUE))
#' # show head of data frame
#' head(dummy)
#' # set value 2 and 4 as missings
#' dummy <- set_na(dummy, c(2, 4))
#' # show head of new data frame
#' head(dummy)
#'
#' # create list of variables
#' data(efc)
#' dummy <- list(efc$c82cop1, efc$c83cop2, efc$c84cop3)
#' # check original distribution of categories
#' lapply(dummy, table, exclude = NULL)
#' # set 3 to NA
#' lapply(set_na(dummy, 3), table, exclude = NULL)
#'
#' # create random variable
#' dummy <- sample(1:5, 100, replace = TRUE)
#' # declare missing values, but only as attribute
#' dummy <- set_na(dummy, c(3, 5), as.attr = TRUE)
#'
#' str(dummy)
#' table(dummy)
#' get_na(dummy)
#'
#' # create random variable
#' dummy <- sample(1:5, 100, replace = TRUE)
#' # declare missing values, but only as attribute
#' # missing code definition may be logical indices
#' dummy <- set_na(dummy, c(FALSE, FALSE, FALSE, TRUE, TRUE), as.attr = TRUE)
#'
#' str(dummy)
#' table(dummy)
#' get_na(dummy)
#'
#' @export
set_na <- function(x, value) {
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # dichotomize all
    for (i in 1:nvars) x[[i]] <- set_na_helper(x[[i]], value)
    return(x)
  } else {
    return(set_na_helper(x, value))
  }
}


#' @importFrom stats na.omit
#' @importFrom haven tagged_na na_tag
set_na_helper <- function(x, value) {
  # check if we have any values at all?
  if (is.null(value)) return(x)
  # get label attribute
  attr.string <- getValLabelAttribute(x)

  # check if value is a named vector
  na.names <- names(value)
  # get values for value labels
  lab.values <- get_values(x)

  # get na-tags, to check whether NA already was defined
  nat <- as.vector(stats::na.omit(haven::na_tag(x)))
  # stop if user wants to assign a value to NA that is
  # already assigned as NA
  if (any(nat %in% as.character(value)))
    stop("Can't set NA values. At least of `value` is already defined as NA.", call. = F)

  # iterate all NAs
  for (i in 1:length(value)) {
    # find associated values in x and set them as tagged NA
    x[x %in% value[i]] <- haven::tagged_na(as.character(value[i]))
    # is na-value in labelled values?
    lv <- which(lab.values == value[i])
    # if yes, replace label
    if (!is_empty(lv)) {
      # change value
      attr(x, attr.string)[lv] <- haven::tagged_na(as.character(value[i]))
      # change label as well?
      if (!is.null(na.names)) names(attr(x, attr.string))[lv] <- na.names[i]
    } else {
      # no attribute string yet?
      if (is.null(attr.string)) attr.string <- haven_attr_string()
      # get labels and label values
      lv <- attr(x, attr.string, exact = T)
      ln <- names(attr(x, attr.string, exact = T))
      # add NA
      attr(x, attr.string) <- c(lv, haven::tagged_na(as.character(value[i])))
      if (!is.null(na.names))
        names(attr(x, attr.string)) <- c(ln, na.names[i])
      else
        names(attr(x, attr.string)) <- c(ln, as.character(value[i]))
    }
  }

  return(x)
}

#' @rdname set_na
#' @export
`set_na<-` <- function(x, value) {
  UseMethod("set_na<-")
}

#' @export
`set_na<-.default` <- function(x, value) {
  x <- set_na(x, value)
  x
}
