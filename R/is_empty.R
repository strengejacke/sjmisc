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
      if (length(x) > 1) warning("`x` must be of length 1. Evaluating first element only.", call. = TRUE)
      # zero chars, so empty?
      zero_len <- nchar(x) == 0
      # if 'x' was empty, we have no chars, so zero_len will be integer(0).
      # check this here, because zero_len needs to be logical
      if (length(zero_len) == 0) zero_len <- TRUE
      # we have a non-character vector here. check for length
    } else {
      zero_len <- length(x) == 0
    }
  }
  return(is.null(x) || zero_len || is.na(x))
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
#'           \code{\link[tibble]{tibble}} with "empty" columns or rows removed.
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
  which(colSums(is.na(x)) == nrow(x))
}

#' @rdname empty_cols
#' @export
empty_rows <- function(x) {
  which(rowSums(is.na(x)) == ncol(x))
}

#' @rdname empty_cols
#' @importFrom tibble as_tibble
#' @export
remove_empty_cols <- function(x) {
  tibble::as_tibble(x[, -empty_cols(x)])
}

#' @rdname empty_cols
#' @importFrom tibble as_tibble
#' @export
remove_empty_rows <- function(x) {
  tibble::as_tibble(x[-empty_rows(x), ])
}

