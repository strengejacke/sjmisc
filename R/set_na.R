#' @title Replace specific values in vector with NA
#' @name set_na
#'
#' @description This function replaces specific values of a variable, data frame
#'                or list of variables with missings (\code{NA}).
#'
#' @seealso \code{\link{replace_na}} to replace \code{\link{NA}}'s with specific
#'            values, \code{\link{rec}} for general recoding of variables and
#'            \code{\link{recode_to}} for re-shifting value ranges. See
#'            \code{\link{get_na}} to get values of missing values in
#'            labelled vectors.
#'
#' @param x Variable (vector), data frame or list of variables where new
#'          missing values should be defined. If \code{x} is a \code{data.frame}, each
#'          column is assumed to be a new variable, where missings should be defined.
#' @param value Numeric vector with values that should be replaced with NA values,
#'        or a character vector if values of factors or character vectors should be
#'        replaced. For labelled vectors, may also be the name of a value label. In
#'        this case, the associated values for the value labels in each vector
#'        will be replaced with NA (see 'Examples').
#' @param drop.levels Logical, if \code{TRUE}, factor levels of values that have
#'          been replaced with \code{NA} are dropped. See 'Examples'.
#' @param as.tag Logical, if \code{TRUE}, values in \code{x} will be replaced
#'          by \code{tagged_na}, else by usual \code{NA} values. Use a named
#'          vector to assign the value label to the tagged NA value (see 'Examples').
#'
#' @return \code{x}, with all elements of \code{value} being replaced by \code{NA}.
#'
#' @note Labels from values that are replaced with NA and no longer used will be
#'         removed from \code{x}, however, other value and variable label
#'         attributes (see, for instance, \code{\link{get_labels}} or
#'         \code{\link{set_labels}}) are preserved.
#'
#' @details \code{set_na} converts all values defined in \code{value} with
#'            a related \code{NA} or tagged NA values (see \code{\link[haven]{tagged_na}}).
#'            Tagged \code{NA}s work exactly like regular R missing values
#'            except that they store one additional byte of information: a tag,
#'            which is usually a letter ("a" to "z") or character number ("0" to "9").
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
#' table(dummy, useNA = "always")
#'
#' # add named vector as further missing value
#' set_na(dummy, c("Refused" = 5), as.tag = TRUE)
#' # see different missing types
#' library(haven)
#' print_tagged_na(set_na(dummy, c("Refused" = 5), as.tag = TRUE))
#'
#'
#' # create sample data frame
#' dummy <- data.frame(var1 = sample(1:8, 100, replace = TRUE),
#'                     var2 = sample(1:10, 100, replace = TRUE),
#'                     var3 = sample(1:6, 100, replace = TRUE))
#' # set value 2 and 4 as missings
#' library(dplyr)
#' dummy %>% set_na(c(2, 4)) %>% head()
#' dummy %>% set_na(c(2, 4), as.tag = TRUE) %>% get_na()
#' dummy %>% set_na(c(2, 4), as.tag = TRUE) %>% get_values()
#'
#' # create list of variables
#' data(efc)
#' dummy <- list(efc$c82cop1, efc$c83cop2, efc$c84cop3)
#' # check original distribution of categories
#' lapply(dummy, table, useNA = "always")
#' # set 3 to NA
#' lapply(set_na(dummy, 3), table, useNA = "always")
#'
#' # drop unused factor levels when being set to NA
#' x <- factor(c("a", "b", "c"))
#' x
#' set_na(x, "b", as.tag = TRUE)
#' set_na(x, "b", drop.levels = FALSE, as.tag = TRUE)
#'
#' # set_na() can also remove a missing by defining the value label
#' # of the value that should be replaced with NA. This is in particular
#' # helpful if a certain category should be set as NA, however, this category
#' # is assigned with different values accross variables
#' x1 <- sample(1:4, 20, replace = TRUE)
#' x2 <- sample(1:7, 20, replace = TRUE)
#' set_labels(x1) <- c("Refused" = 3, "No answer" = 4)
#' set_labels(x2) <- c("Refused" = 6, "No answer" = 7)
#'
#' tmp <- data.frame(x1, x2)
#' get_labels(tmp)
#' get_labels(set_na(tmp, "No answer"))
#' get_labels(set_na(tmp, c("Refused", "No answer")))
#'
#' # show values
#' tmp
#' set_na(tmp, c("Refused", "No answer"))
#'
#'
#' @export
set_na <- function(x, value, drop.levels = TRUE, as.tag = FALSE) {
  UseMethod("set_na")
}

#' @export
set_na.data.frame <- function(x, value, drop.levels = TRUE, as.tag = FALSE) {
  tibble::as_tibble(lapply(x, FUN = set_na_helper, value, drop.levels, as.tag))
}

#' @export
set_na.list <- function(x, value, drop.levels = TRUE, as.tag = FALSE) {
  lapply(x, FUN = set_na_helper, value, drop.levels, as.tag)
}

#' @export
set_na.default <- function(x, value, drop.levels = TRUE, as.tag = FALSE) {
  set_na_helper(x, value, drop.levels, as.tag)
}

#' @importFrom stats na.omit
#' @importFrom haven tagged_na na_tag
set_na_helper <- function(x, value, drop.levels, as.tag) {
  # check if we have any values at all?
  if (is.null(value)) return(x)
  # get label attribute
  attr.string <- getValLabelAttribute(x)

  # check if value is a named vector
  na.names <- names(value)
  # get values for value labels
  lab.values <- get_values(x, drop.na = F)

  # no tagged NA's for date values
  if (inherits(x, "Date")) as.tag <- F

  # if value is a character vector, user may have defined a value label.
  # find value of associated label then
  if (is.character(value)) {
    # get value labels
    val.lab <- get_labels(x, attr.only = TRUE, include.values = "n",
                          include.non.labelled = FALSE, drop.na = TRUE)
    # get value labels that match the values which should be set to NA
    val.match <- val.lab[val.lab %in% value]
    # now get values for this vector
    if (!suppressWarnings(is_empty(val.match)) && !suppressWarnings(is_empty(names(val.match)))) {
      # should be numeric, else we might have a factor
      na.values <- suppressWarnings(as.numeric(names(val.match)))
      # if we have no NA, coercing to numeric worked. Now get these
      # NA values and remove value labels from vector
      if (!anyNA(na.values)) {
        x <- suppressWarnings(remove_labels(x, value))
        value <- na.values
      }
    }
  }

  # haven::na_tag works only for double
  if (is.double(x) && as.tag) {
    # get na-tags, to check whether NA already was defined
    nat <- as.vector(stats::na.omit(haven::na_tag(x)))
    # stop if user wants to assign a value to NA that is
    # already assigned as NA
    if (any(nat %in% as.character(value)))
      stop("Can't set NA values. At least one element of `value` is already defined as NA. Use `zap_na_tags()` to remove tags from NA values.", call. = F)
  }

  # iterate all NAs
  for (i in seq_len(length(value))) {
    if (as.tag) {
      # find associated values in x and set them as tagged NA
      x[x %in% value[i]] <- haven::tagged_na(as.character(value[i]))
      # is na-value in labelled values?
      lv <- which(lab.values == value[i])
      # if yes, replace label
      if (!is_empty(lv)) {
        # for tagged NA, use tag as new attribute
        # change value
        attr(x, attr.string)[lv] <- haven::tagged_na(as.character(value[i]))
        # change label as well?
        if (!is.null(na.names)) names(attr(x, attr.string))[lv] <- na.names[i]
      } else {
        # no attribute string yet?
        if (is.null(attr.string)) attr.string <- "labels"
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
    } else {
      # find associated values in x and set them as tagged NA
      x[x %in% value[i]] <- NA
    }
  }

  # remove value labels
  x <- remove_labels(x, which(get_values(x) %in% value))

  # if we have a factor, check if we have unused levels now due to NA
  # assignment. If yes, drop levels
  if (is.factor(x) && drop.levels && length(levels(x)) != length(levels(droplevels(x)))) {
    # save value and variable labels
    keep.val <- attr(x, "labels", exact = T)
    keep.var <- attr(x, "label", exact = T)
    # drop levels
    x <- droplevels(x)
    # set back labels
    attr(x, "labels") <- keep.val
    attr(x, "label") <- keep.var
  }

  return(x)
}

#' @rdname set_na
#' @export
`set_na<-` <- function(x, drop.levels = TRUE, as.tag = FALSE, value) {
  UseMethod("set_na<-")
}

#' @export
`set_na<-.default` <- function(x, drop.levels = TRUE, as.tag = FALSE, value) {
  x <- set_na(x, value, drop.levels, as.tag)
  x
}
