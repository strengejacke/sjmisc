#' @title Retrieve missing values of labelled variables
#' @name get_na
#'
#' @description This function retrieves the value codes associated with missing values
#'                of variables of an imported SPSS, SAS or STATA data set (via \code{\link{read_spss}},
#'                \code{\link{read_sas}} or \code{\link{read_stata}}), where missing
#'                values have not been replaced with \code{NA}s after import,
#'                or of \code{\link[haven]{labelled}} vectors.
#'
#' @seealso \code{\link{get_labels}} to get value labels, or \code{\link{get_values}}
#'            to get values associated with labels; see \code{\link{set_na}} to
#'            replace specific values with \code{NA} and \code{\link{to_na}} to
#'            convert missing value codes into \code{NA}; see \code{\link{get_na_flags}}
#'            to get a logical vector of missing flags.
#'
#' @param x Variable (vector) with value label attributes, including
#'          missing value codes (see \code{\link{labelled}}).
#' @return The missing values associated with value labels from \code{x},
#'           or \code{NULL} if \code{x} has no missing value attribute.
#'
#' @details Other statistical software packages (like 'SPSS') allow to define
#'            multiple missing values, e.g. \emph{not applicable}, \emph{refused answer}
#'            or "real" missing. These missing types may be assigned with
#'            different values, so it is possible to distinguish between these
#'            missing types. In R, multiple declared missings cannot be represented
#'            in a similar way. However, \code{\link{labelled}} vectors
#'            allow to indicate different missings through the
#'            \code{is_na}-\code{\link{attr}}. Technically, these "missings" are
#'            stored as normal values. Thus, the \code{\link{table}} command,
#'            for instance, would include these values by default. The
#'            \pkg{sjmisc} package offers capabilities to deal with multiple
#'            declared missings and enhances the possibilities to work with
#'            labelled data, allowing for easy access of multiple declared
#'            missings or conversion into \code{NA} etc.
#'            \cr \cr
#'            Furthermore, see 'Details' in \code{\link{get_values}}.
#'
#' @examples
#' # create labelled factor, with missing flag
#' x <- labelled(c("M", "M", "F", "X", "N/A"),
#'               c(Male = "M", Female = "F",
#'                 Refused = "X", "Not applicable" = "N/A"),
#'               c(FALSE, FALSE, TRUE, TRUE))
#' get_na(x)
#'
#' # create labelled integer, with missing flag
#' x <- labelled(c(1, 2, 1, 3, 4, 1),
#'               c(Male = 1, Female = 2, Refused = 3, "N/A" = 4),
#'               c(FALSE, FALSE, TRUE, TRUE))
#' get_na(x)
#'
#' @export
get_na <- function(x) {
  # get values
  values <- get_values(x, sort.val = FALSE, drop.na = FALSE)
  # get NA logicals
  na.flag <- get_na_flags(x)
  # do we have missing flag?
  if (is.null(na.flag)) {
    message("Variable has no assigned missing value codes.")
    return(NULL)
  }
  # copy NA-codes to new vector, so we can check length
  nas <- values[na.flag]
  # set return value to NULL, if no missing values
  if (length(nas) == 0) nas <- NULL
  # return missing values
  return(nas)
}


getNaAttribute <- function() return("is_na")
