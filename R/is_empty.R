#' @title Check whether string, list or vector is empty
#' @name is_empty
#' @description This function checks whether a string or character vector (of
#'                length 1), a list or any vector (numeric, atomic) is empty or not.
#'
#'
#' @param x String, character vector, list, data.frame or numeric vector or factor.
#' @param first.only Logical, if \code{FALSE} and \code{x} is a character
#'        vector, each element of \code{x} will be checked if empty. If
#'        \code{TRUE}, only the first element of \code{x} will be checked.
#' @param all.na.empty Logical, if \code{x} is a vector with \code{NA}-values
#'         only, \code{is_empty} will return \code{FALSE} if \code{all.na.empty = FALSE},
#'         and will return \code{TRUE} if \code{all.na.empty = TRUE} (default).
#' @return Logical, \code{TRUE} if \code{x} is a character vector or string and
#'           is empty, \code{TRUE} if \code{x} is a vector or list and of length 0,
#'           \code{FALSE} otherwise.
#'
#' @note \code{NULL}- or \code{NA}-values are also considered as "empty" (see
#'         'Examples') and will return \code{TRUE}, unless \code{all.na.empty==FALSE}.
#'
#' @examples
#' is_empty("test")
#' is_empty("")
#' is_empty(NA)
#' is_empty(NULL)
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
#' # check multiple elements of character vectors
#' is_empty(c("", "a"))
#' is_empty(c("", "a"), first.only = FALSE)
#'
#' # empty data frame
#' d <- data.frame()
#' is_empty(d)
#'
#' # empty list
#' is_empty(list(NULL))
#'
#' # NA vector
#' x <- rep(NA,5)
#' is_empty(x)
#' is_empty(x, all.na.empty = FALSE)
#'
#' @importFrom purrr compact
#' @export
is_empty <- function(x, first.only = TRUE, all.na.empty = TRUE) {
  # do we have a valid vector?
  if (!is.null(x)) {
    # if it's a character, check if we have only one element in that vector
    if (is.character(x)) {
      # characters may also be of length 0
      if (length(x) == 0) return(TRUE)
      # else, check all elements of x
      zero_len <- nchar(x) == 0
      # return result for multiple elements of character vector
      if (first.only) {
        zero_len <- .is_true(zero_len[!is.na(x) & !is.null(x)][1])
        if (length(x) > 0) x <- x[!is.na(x) & !is.null(x)][1]
      } else {
        return(unname(zero_len))
      }
      # we have a non-character vector here. check for length
    } else if (is.list(x)) {
      x <- purrr::compact(x)
      zero_len <- length(x) == 0
    } else {
      zero_len <- length(x) == 0
    }
  }

  any(is.null(x) || zero_len || (all.na.empty && all(is.na(x))))
}


.is_true <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && x
}


#' @title Return or remove variables or observations that are completely missing
#' @name empty_cols
#'
#' @description These functions check which rows or columns of a data frame completely
#'                contain missing values, i.e. which observations or variables
#'                completely have missing values, and either 1) returns their
#'                indices; or 2) removes them from the data frame.
#'
#'
#' @param x A data frame.
#'
#' @return For \code{empty_cols} and \code{empty_rows}, a numeric (named) vector
#'           with row or column indices of those variables that completely have
#'           missing values.
#'           \cr \cr
#'           For \code{remove_empty_cols} and \code{remove_empty_rows}, a
#'           data frame with "empty" columns or rows removed.
#'
#' @examples
#' tmp <- data.frame(a = c(1, 2, 3, NA, 5),
#'                   b = c(1, NA, 3, NA , 5),
#'                   c = c(NA, NA, NA, NA, NA),
#'                   d = c(1, NA, 3, NA, 5))
#'
#' tmp
#'
#' empty_cols(tmp)
#' empty_rows(tmp)
#'
#' remove_empty_cols(tmp)
#' remove_empty_rows(tmp)
#'
#' @export
empty_cols <- function(x) {
  if ((!is.matrix(x) && !is.data.frame(x)) || ncol(x) < 2)
    vector("numeric")
  else
    which(colSums(is.na(x)) == nrow(x))
}


#' @rdname empty_cols
#' @export
empty_rows <- function(x) {
  if ((!is.matrix(x) && !is.data.frame(x)) || nrow(x) < 2)
    vector("numeric")
  else
    which(rowSums(is.na(x)) == ncol(x))
}


#' @rdname empty_cols
#' @importFrom dplyr select
#' @export
remove_empty_cols <- function(x) {
  # check if we have any empty columns at all
  ec <- empty_cols(x)
  # if yes, removing works, else an empty df would be returned
  if (!sjmisc::is_empty(ec))
    dplyr::select(x, !! -ec)
  else
    x
}


#' @rdname empty_cols
#' @importFrom dplyr slice
#' @export
remove_empty_rows <- function(x) {
  # check if we have any empty rows at all
  er <- empty_rows(x)
  # if yes, removing works, else an empty df would be returned
  if (!sjmisc::is_empty(er))
    dplyr::slice(x, !! -er)
  else
    x
}

