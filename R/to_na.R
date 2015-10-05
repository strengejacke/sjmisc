#' @title Convert missing values of labelled variables into NA
#' @name to_na
#'
#' @description This function converts defined missing values that are stored as
#'                original value code into \code{NA}.
#'
#' @seealso \code{\link{get_na}} to get value codes of missing values.
#'
#' @param x variable (vector), \code{data.frame} or \code{list} of variables
#'          with value label attributes and defined missing value codes
#'          (see \code{\link[haven]{labelled}}).
#' @return \code{x}, where each value code of missing values is converted
#'            to \code{NA}.
#'
#' @details \code{to_na} converts values to \code{NA}, which are defined
#'            as missing through the \code{is_na}-attribute of a vector
#'            (see \code{\link[haven]{labelled}}). \code{\link{set_na}},
#'            by contrast, converts those values to \code{NA} that are
#'            specified in the function's \code{values} argument; hence,
#'            \code{\link{set_na}} ignores the \code{is_na}-attribute.
#'            \cr \cr
#'            Furthermore, see 'Details' in \code{\link{get_values}}
#'            and \code{\link{get_na}}.
#'
#' @note This is a convenient function for \code{set_na(x, get_na(x))}.
#'
#' @examples
#' # create labelled factor, with missing flag
#' x <- labelled(c("M", "M", "F", "X", "N/A"),
#'               c(Male = "M", Female = "F",
#'                 Refused = "X", "Not applicable" = "N/A"),
#'               c(FALSE, FALSE, TRUE, TRUE))
#' x
#' get_na(x)
#' to_na(x)
#'
#' # create labelled integer, with missing flag
#' x <- labelled(c(1, 2, 1, 3, 4, 1),
#'              c(Male = 1, Female = 2, Refused = 3, "N/A" = 4),
#'              c(FALSE, FALSE, TRUE, TRUE))
#' x
#' get_na(x)
#' to_na(x)
#'
#' # get summary
#' x <- labelled(c(1, 2, 1, 3, 4, 1, NA, 5),
#'               c(Male = 1, Female = 2, Refused = 5),
#'               c(FALSE, FALSE, TRUE))
#' frq(x)
#'
#' @export
to_na <- function(x) {
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # na all
    for (i in 1:nvars) x[[i]] <- to_na_helper(x[[i]])
    return(x)
  } else {
    return(to_na_helper(x))
  }
}

to_na_helper <- function(x) set_na(x, suppressMessages(get_na(x)), as.attr = FALSE)
