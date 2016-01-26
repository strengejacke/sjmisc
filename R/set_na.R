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
#' dummy <- set_na(dummy,
#'                 c(FALSE, FALSE, FALSE, TRUE, TRUE),
#'                 as.attr = TRUE)
#'
#' str(dummy)
#' table(dummy)
#' get_na(dummy)
#'
#' @export
set_na <- function(x, value, as.attr = FALSE) {
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # dichotomize all
    for (i in 1:nvars) x[[i]] <- set_na_helper(x[[i]], value, as.attr)
    return(x)
  } else {
    return(set_na_helper(x, value, as.attr))
  }
}


#' @importFrom stats na.omit
set_na_helper <- function(x, value, as.attr = FALSE) {
  # ----------------------------
  # does user want to add missing codes as is_na attribute?
  # if yes, do so here...
  # ----------------------------
  if (as.attr) {
    x <- set_na_attr(x, value)
  } else {
    # ----------------------------
    # check if we have any values at all?
    # ----------------------------
    if (is.null(value)) return(x)
    # ---------------------------------------
    # find associated values in x
    # and set them to NA
    # ---------------------------------------
    x[x %in% value] <- NA
    # ----------------------------
    # auto-detect variable label attribute
    # ----------------------------
    attr.string <- getValLabelAttribute(x)
    # check if x has label attributes
    if (!is.null(attr.string)) {
      # retrieve value labels
      vl <- attr(x, attr.string, exact = T)
      # retrieve label names
      ln <- names(vl)
      # ---------------------------------------
      # check if value labels exist, and if yes, remove them
      # ---------------------------------------
      labelpos <- suppressWarnings(which(as.numeric(vl) %in% value))
      # ---------------------------------------
      # remove NA label
      # ---------------------------------------
      if (length(labelpos > 0)) {
        vl <- vl[-labelpos]
        ln <- ln[-labelpos]
      } else {
        # ---------------------------------------
        # if vl were not numeric convertable, try character conversion
        # check if value labels exist, and if yes, remove them
        # ---------------------------------------
        labelpos <- suppressWarnings(which(as.character(vl) %in% value))
        # remove NA label
        if (length(labelpos > 0)) {
          vl <- vl[-labelpos]
          ln <- ln[-labelpos]
        }
      }
      # ---------------------------------------
      # do we have any labels left?
      # ---------------------------------------
      if (length(vl) > 0) {
        # if yes, set back label attribute
        attr(x, attr.string) <- vl
        names(attr(x, attr.string)) <- ln
        # ---------------------------------------
        # shorten is_na attribute
        # ---------------------------------------
        na.flag <- get_na_flags(x)
        if (!is.null(na.flag)) {
          # ---------------------------------------
          # do we have is_na attribute? if yes,
          # remove missing flags of values set to NA
          # ---------------------------------------
          attr(x, getNaAttribute()) <- na.flag[-labelpos]
        }
      } else {
        # ---------------------------------------
        # else remove attribute
        # ---------------------------------------
        attr(x, attr.string) <- NULL
        # remove is_na attribute, no longer needed
        attr(x, getNaAttribute()) <- NULL
        # ---------------------------------------
        # unclass labelled, because it may result
        # in errors when printing a labelled-class-vector
        # without labelled and is_na attribute
        # ---------------------------------------
        if (is_labelled(x)) x <- unclass(x)
      }
    }
    # --------------------------------
    # if we have a factor, remove unused levels
    # --------------------------------
    if (is.factor(x)) x <- droplevels(x)
  }
  return(x)
}


set_na_attr <- function(x, na.values) {
  # get values
  all.values <- get_values(x, sort.val = FALSE, drop.na = FALSE)
  # do we have value attributes?
  if (is.null(all.values)) {
    # we assume a simple numeric vector, so let's add
    # some label attributes
    all.values <- sort(unique(stats::na.omit(x)))
    x <- set_labels(x, as.character(all.values))
  }
  if (is.null(na.values)) {
    # is na.values NULL? Then set FALSE (no missing)
    # for all value codes
    na.values <- rep(FALSE, length(all.values))
  } else if (!is.logical(na.values)) {
    # if we do not have logical indices,
    # set TRUE for all NA-codes and FALSE for all other
    na.values <- !is.na(match(all.values, na.values))
  }
  # same length?
  if (length(na.values) != length(all.values))
    # If not, warn user
    warning("Length of logical indices for missing codes did not match length of values.", call. = F)
  # set is_na attribute
  attr(x, getNaAttribute()) <- na.values
  return(x)
}

#' @rdname set_na
#' @export
`set_na<-` <- function(x, as.attr = FALSE, value) {
  UseMethod("set_na<-")
}

#' @export
`set_na<-.default` <- function(x, as.attr = FALSE, value) {
  x <- set_na(x, value, as.attr)
  x
}
