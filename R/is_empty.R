#' @title Check whether string or vector is empty
#' @name is_empty
#' @description This function checks whether a string or character vector (of
#'                length 1) or any vector (numeric, atomic) is empty or not.
#'
#'
#' @param x String, character vector of length 1, or vector.
#' @return Logical, \code{TRUE} if \code{x} is a character vector or string and
#'           is empty, \code{TRUE} if \code{x} is any vector and of length 0,
#'           \code{FALSE} otherwise.
#'
#' @note \code{NULL}- or \code{NA}-values are also considered as "empty" (see
#'         'Examples') and will return \code{TRUE}.
#'
#' @examples
#' x <- "test"
#' is_empty(x)
#'
#' x <- ""
#' is_empty(x)
#'
#' x <- NA
#' is_empty(x)
#'
#' x <- NULL
#' is_empty(x)
#'
#' # string is not empty
#' is_empty(" ")
#'
#' # however, this trimmed string is
#' is_empty(trim(" "))
#'
#' # numeric vector
#' x <- 1
#' is_empty(x)
#' x <- x[-1]
#' is_empty(x)
#'
#' @export
is_empty <- function(x) {
  # do we have a valid vector?
  if (!is.null(x)) {
    # if it's a character, check if we have only one element in that vector
    if (is.character(x)) {
      if (length(x) > 1) warning("`x` must be of length 1 (i.e. is allowed to have only 1 value). Evaluating first element only.", call. = F)
      # zero chats, so empty?
      zero_len <- nchar(x) == 0
      # we have a non-character vector here. check for length
    } else {
      zero_len <- length(x) == 0
    }
  }
  return(is.null(x) || zero_len || is.na(x))
}
