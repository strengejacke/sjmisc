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
#' @param value Value that will replace the \code{\link{NA}}'s.
#' @param na.label Optional character vector, used to label the the former NA-value
#'          (i.e. adding a \code{labels} attribute for \code{value} to \code{x}).
#' @param tagged.na Optional single character, specifies a \code{\link[haven]{tagged_na}} value
#'          that will be replaced by \code{value}. Herewith it is possible
#'          to replace only specific \code{NA} values of \code{x}.
#'
#' @inheritParams to_factor
#' @inheritParams rec
#'
#' @return \code{x}, where \code{NA}'s are replaced with \code{value}. If \code{x}
#'           is a data frame, the complete data frame \code{x} will be returned,
#'           with replaced NA's for variables specified in \code{...};
#'           if \code{...} is not specified, applies to all variables in the
#'           data frame.
#'
#' @note Value and variable label attributes (see, for instance, \code{\link{get_labels}}
#'         or \code{\link{set_labels}}) are preserved.
#'
#' @details While regular \code{NA} values can only be \emph{completely} replaced with
#'            a single value, \code{\link[haven]{tagged_na}} allows to differentiate
#'            between different qualitative values of \code{NA}s.
#'            Tagged \code{NA}s work exactly like regular R missing values
#'            except that they store one additional byte of information: a tag,
#'            which is usually a letter ("a" to "z") or character number ("0" to "9").
#'            Therewith it is possible to replace only specific NA values, while
#'            other NA values are preserved.
#'
#' @examples
#' data(efc)
#' table(efc$e42dep, useNA = "always")
#' table(replace_na(efc$e42dep, value = 99), useNA = "always")
#'
#' # the original labels
#' get_labels(replace_na(efc$e42dep, value = 99))
#' # NA becomes "99", and is labelled as "former NA"
#' get_labels(replace_na(efc$e42dep, value = 99, na.label = "former NA"),
#'            include.values = "p")
#'
#' dummy <- data.frame(
#'   v1 = efc$c82cop1,
#'   v2 = efc$c83cop2,
#'   v3 = efc$c84cop3
#' )
#' # show original distribution
#' lapply(dummy, table, useNA = "always")
#' # show variables, NA's replaced with 99
#' lapply(replace_na(dummy, v2, v3, value = 99), table, useNA = "always")
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
#' replace_na(x, value = 2, tagged.na = "c")
#' get_na(replace_na(x, value = 2, tagged.na = "c"))
#'
#' table(x)
#' table(replace_na(x, value = 2, tagged.na = "c"))
#'
#' # tagged NA also works for non-labelled class
#' # init vector
#' x <- c(1, 2, 3, 4)
#' # set values 2 and 3 as NA, will automatically become
#' # tagged NAs by 'set_na()'.
#' x <- set_na(x, value = c(2, 3))
#' # see result
#' x
#' # now replace only NA tagged with 2 with value 5
#' replace_na(x, value = 5, tagged.na = "2")
#'
#' @export
replace_na <- function(x, ..., value, na.label = NULL, tagged.na = NULL) {
  # check for valid value
  if (is.null(value) || is.na(value)) return(x)

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
      x[[i]] <- replace_na_helper(
        x = .dat[[i]],
        value = value,
        na.label = na.label,
        tagged.na = tagged.na
      )
    }

    # coerce to tibble
    x <- tibble::as_tibble(x)

  } else {
    x <- replace_na_helper(
      x = .dat,
      value = value,
      na.label = na.label,
      tagged.na = tagged.na
    )
  }

  x
}

replace_na_helper <- function(x, value, na.label, tagged.na) {
  # create named vector, for labelleing
  if (!is.null(na.label)) {
    na.vec <- value
    names(na.vec) <- as.character(na.label)
  }

  # check if we have any misisngs at all
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
      x <- suppressMessages(remove_labels(x, value = tagged.na))
    } else {
      x[is.na(x)] <- value
    }

    # add NA label
    if (!is.null(na.label)) x <- add_labels(x, value = na.vec)
  } else {
    message("`x` has no missings.")
  }
  return(x)
}
