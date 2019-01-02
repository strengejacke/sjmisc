#' @title Replace specific values in vector with NA
#' @name set_na
#'
#' @description This function replaces specific values of variables with \code{NA}.
#'    \code{set_na_if()} is a scoped variant of \code{set_na()}, where values
#'    will be replaced only with NA's for those variables that match the logical
#'    condition of \code{predicate}.
#'
#' @seealso \code{\link{replace_na}} to replace \code{\link{NA}}'s with specific
#'   values, \code{\link{rec}} for general recoding of variables and
#'   \code{\link{recode_to}} for re-shifting value ranges. See
#'   \code{\link{get_na}} to get values of missing values in
#'   labelled vectors.
#'
#' @param na Numeric vector with values that should be replaced with NA values,
#'   or a character vector if values of factors or character vectors should be
#'   replaced. For labelled vectors, may also be the name of a value label. In
#'   this case, the associated values for the value labels in each vector
#'   will be replaced with \code{NA}. \code{na} can also be a named vector.
#'   If \code{as.tag = FALSE}, values will be replaced only in those variables
#'   that are indicated by the value names (see 'Examples').
#' @param drop.levels Logical, if \code{TRUE}, factor levels of values that have
#'   been replaced with \code{NA} are dropped. See 'Examples'.
#' @param as.tag Logical, if \code{TRUE}, values in \code{x} will be replaced
#'   by \code{tagged_na}, else by usual \code{NA} values. Use a named
#'   vector to assign the value label to the tagged NA value (see 'Examples').
#'
#' @inheritParams to_factor
#' @inheritParams rec
#'
#' @return \code{x}, with all values in \code{na} being replaced by \code{NA}.
#'   If \code{x} is a data frame, the complete data frame \code{x} will
#'   be returned, with NA's set for variables specified in \code{...};
#'   if \code{...} is not specified, applies to all variables in the
#'   data frame.
#'
#' @note Labels from values that are replaced with NA and no longer used will be
#'   removed from \code{x}, however, other value and variable label
#'   attributes are preserved. For more details on labelled data,
#'   see vignette \href{https://cran.r-project.org/package=sjlabelled/vignettes/intro_sjlabelled.html}{Labelled Data and the sjlabelled-Package}.
#'
#' @details \code{set_na()} converts all values defined in \code{na} with
#'   a related \code{NA} or tagged NA value (see \code{\link[haven]{tagged_na}}).
#'   Tagged \code{NA}s work exactly like regular R missing values
#'   except that they store one additional byte of information: a tag,
#'   which is usually a letter ("a" to "z") or character number ("0" to "9").
#'   \cr \cr
#'   \strong{Different NA values for different variables}
#'   \cr \cr
#'   If \code{na} is a named vector \emph{and} \code{as.tag = FALSE}, the names
#'   indicate variable names, and the associated values indicate those values
#'   that should be replaced by \code{NA} in the related variable. For instance,
#'   \code{set_na(x, na = c(v1 = 4, v2 = 3))} would replace all 4 in \code{v1}
#'   with \code{NA} and all 3 in \code{v2} with \code{NA}.
#'   \cr \cr
#'   If \code{na} is a named list \emph{and} \code{as.tag = FALSE}, it is possible
#'   to replace different multiple values by \code{NA} for different variables
#'   separately. For example, \code{set_na(x, na = list(v1 = c(1, 4), v2 = 5:7))}
#'   would replace all 1 and 4 in \code{v1} with \code{NA} and all 5 to 7 in
#'   \code{v2} with \code{NA}.
#'   \cr \cr
#'   Furthermore, see also 'Details' in \code{\link{get_na}}.
#'
#' @examples
#' # create random variable
#' dummy <- sample(1:8, 100, replace = TRUE)
#' # show value distribution
#' table(dummy)
#' # set value 1 and 8 as missings
#' dummy <- set_na(dummy, na = c(1, 8))
#' # show value distribution, including missings
#' table(dummy, useNA = "always")
#'
#' # add named vector as further missing value
#' set_na(dummy, na = c("Refused" = 5), as.tag = TRUE)
#' # see different missing types
#' library(haven)
#' library(sjlabelled)
#' print_tagged_na(set_na(dummy, na = c("Refused" = 5), as.tag = TRUE))
#'
#'
#' # create sample data frame
#' dummy <- data.frame(var1 = sample(1:8, 100, replace = TRUE),
#'                     var2 = sample(1:10, 100, replace = TRUE),
#'                     var3 = sample(1:6, 100, replace = TRUE))
#' # set value 2 and 4 as missings
#' dummy %>% set_na(na = c(2, 4)) %>% head()
#' dummy %>% set_na(na = c(2, 4), as.tag = TRUE) %>% get_na()
#' dummy %>% set_na(na = c(2, 4), as.tag = TRUE) %>% get_values()
#'
#' data(efc)
#' dummy <- data.frame(
#'   var1 = efc$c82cop1,
#'   var2 = efc$c83cop2,
#'   var3 = efc$c84cop3
#' )
#' # check original distribution of categories
#' lapply(dummy, table, useNA = "always")
#' # set 3 to NA for two variables
#' lapply(set_na(dummy, var1, var3, na = 3), table, useNA = "always")
#'
#'
#' # if 'na' is a named vector *and* 'as.tag = FALSE', different NA-values
#' # can be specified for each variable
#' set.seed(1)
#' dummy <- data.frame(
#'   var1 = sample(1:8, 10, replace = TRUE),
#'   var2 = sample(1:10, 10, replace = TRUE),
#'   var3 = sample(1:6, 10, replace = TRUE)
#' )
#' dummy
#'
#' # Replace "3" in var1 with NA, "5" in var2 and "6" in var3
#' set_na(dummy, na = c(var1 = 3, var2 = 5, var3 = 6))
#'
#' # if 'na' is a named list *and* 'as.tag = FALSE', for each
#' # variable different multiple NA-values can be specified
#' set_na(dummy, na = list(var1 = 1:3, var2 = c(7, 8), var3 = 6))
#'
#'
#' # drop unused factor levels when being set to NA
#' x <- factor(c("a", "b", "c"))
#' x
#' set_na(x, na = "b", as.tag = TRUE)
#' set_na(x, na = "b", drop.levels = FALSE, as.tag = TRUE)
#'
#' # set_na() can also remove a missing by defining the value label
#' # of the value that should be replaced with NA. This is in particular
#' # helpful if a certain category should be set as NA, however, this category
#' # is assigned with different values accross variables
#' x1 <- sample(1:4, 20, replace = TRUE)
#' x2 <- sample(1:7, 20, replace = TRUE)
#' x1 <- set_labels(x1, labels = c("Refused" = 3, "No answer" = 4))
#' x2 <- set_labels(x2, labels = c("Refused" = 6, "No answer" = 7))
#'
#' tmp <- data.frame(x1, x2)
#' get_labels(tmp)
#' table(tmp, useNA = "always")
#'
#' get_labels(set_na(tmp, na = "No answer"))
#' table(set_na(tmp, na = "No answer"), useNA = "always")
#'
#' # show values
#' tmp
#' set_na(tmp, na = c("Refused", "No answer"))
#'
#'
#' @export
set_na <- function(x, ..., na, drop.levels = TRUE, as.tag = FALSE) {
  # check for valid value
  if (is.null(na) || anyNA(na)) {
    warning("`na` is not allowed to ne `NULL` or to contain `NA`-values.", call. = FALSE)
    return(x)
  }

  # evaluate arguments, generate data
  .dat <- get_dot_data(x, dplyr::quos(...))

  if (is.data.frame(x)) {
    # iterate variables of data frame
    for (i in colnames(.dat)) {
      x[[i]] <- set_na_helper(
        x = .dat[[i]],
        value = na,
        drop.levels = drop.levels,
        as.tag = as.tag,
        var.name = i
      )
    }
  } else {
    x <- set_na_helper(
      x = .dat,
      value = na,
      drop.levels = drop.levels,
      as.tag = as.tag,
      var.name = NULL
    )
  }

  x
}


#' @importFrom dplyr select_if
#' @rdname set_na
#' @export
set_na_if <- function(x, predicate, na, drop.levels = TRUE, as.tag = FALSE) {

  # select variables that match logical conditions
  .dat <- dplyr::select_if(x, .predicate = predicate)


  # if no variable matches the condition specified
  # in predicate, return original data

  if (sjmisc::is_empty(.dat)) {
    if (append)
      return(x)
    else
      return(.dat)
  }


  if (is.data.frame(x)) {
    # iterate variables of data frame
    for (i in colnames(.dat)) {
      x[[i]] <- set_na_helper(
        x = .dat[[i]],
        value = na,
        drop.levels = drop.levels,
        as.tag = as.tag,
        var.name = i
      )
    }
  } else {
    x <- set_na_helper(
      x = .dat,
      value = na,
      drop.levels = drop.levels,
      as.tag = as.tag,
      var.name = NULL
    )
  }

  x
}


#' @importFrom purrr map2
#' @importFrom stats na.omit
#' @importFrom haven tagged_na na_tag
#' @importFrom sjlabelled get_values get_labels remove_labels
set_na_helper <- function(x, value, drop.levels, as.tag, var.name) {
  # check if values has only NA's
  if (sum(is.na(x)) == length(x)) return(x)

  if (is.list(value)) {
    lnames <- purrr::map2(value, names(value), ~ rep(.y, length(.x))) %>%
      unlist() %>%
      unname()
    value <- unlist(value)
    names(value) <- lnames
  }

  # check if value is a named vector
  na.names <- names(value)
  # get values for value labels
  lab.values <- sjlabelled::get_values(x, drop.na = F)

  # no tagged NA's for date values
  if (inherits(x, "Date")) as.tag <- F

  # get value labels
  val.lab <- attr(x, "labels", exact = T)
  val.lab <- val.lab[!haven::is_tagged_na(val.lab)]

  # if value is a character vector, user may have defined a value label.
  # find value of associated label then
  if (is.character(value)) {
    # get value labels that match the values which should be set to NA
    val.match <- val.lab[names(val.lab) %in% value]
    # now get values for this vector
    if (!sjmisc::is_empty(val.match) && !sjmisc::is_empty(names(val.match))) {
      # should be numeric, else we might have a factor
      na.values <- suppressWarnings(as.numeric(val.match))
      # if we have no NA, coercing to numeric worked. Now get these
      # NA values and remove value labels from vector
      if (!anyNA(na.values)) {
        x <- suppressWarnings(sjlabelled::remove_labels(x, labels = value))
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
      if (!sjmisc::is_empty(lv)) {
        # for tagged NA, use tag as new attribute
        # change value
        attr(x, "labels")[lv] <- haven::tagged_na(as.character(value[i]))
        # change label as well?
        if (!is.null(na.names)) names(attr(x, "labels"))[lv] <- na.names[i]
      } else {
        # get labels and label values
        lv <- attr(x, "labels", exact = T)
        ln <- names(attr(x, "labels", exact = T))
        # add NA
        attr(x, "labels") <- c(lv, haven::tagged_na(as.character(value[i])))
        if (!is.null(na.names))
          names(attr(x, "labels")) <- c(ln, na.names[i])
        else
          names(attr(x, "labels")) <- c(ln, as.character(value[i]))
      }
    } else {
      if (!is.null(na.names) && !is.null(var.name)) {
        if (na.names[i] == var.name)
          x[x %in% value[i]] <- NA
      } else {
        # find associated values in x and set them as tagged NA
        x[x %in% value[i]] <- NA
      }
    }
  }

  # remove unused value labels
  removers <- which(sjlabelled::get_values(x) %in% value)

  if (!is.null(removers) && !sjmisc::is_empty(removers, first.only = T)) {
    attr(x, "labels") <- val.lab[-removers]
  }

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

  x
}
