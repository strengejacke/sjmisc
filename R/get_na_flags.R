#' @title Retrieve missing value flags of labelled variables
#' @name get_na_flags
#'
#' @description This function retrieves the logical missing flags for a
#'                \code{\link{labelled}} variable.
#'
#' @seealso \code{\link{get_na}} to get value codes of labelled missing values;
#'            \code{\link{get_values}} to get values associated with labels;
#'            see \code{\link{set_na}} to replace specific values with \code{NA}
#'            and \code{\link{to_na}} to convert missing value codes into \code{NA}.
#'
#' @param x Variable (vector) with value label attributes, including
#'          missing value codes (see \code{\link{labelled}}).
#' @return Logical vector with missing flags that indicate which labelled value
#'           is considered as missing.
#'
#' @details See 'Details' in \code{\link{get_na}}.
#'
#' @examples
#' # create labelled integer, with missing flag
#' x <- labelled(c(1, 2, 1, 3, 4, 1),
#'               c(Male = 1, Female = 2, Refused = 3, "N/A" = 4),
#'               c(FALSE, FALSE, TRUE, TRUE))
#' get_na_flags(x)
#'
#' @export
get_na_flags <- function(x) return(attr(x, getNaAttribute(), exact = T))
