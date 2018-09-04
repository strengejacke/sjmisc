#' @title Sequence generation for column or row counts of data frames
#' @name seq_col
#'
#' @description \code{seq_col(x)} is a convenient wrapper for \code{seq_len(ncol(x))},
#'    while \code{seq_row(x)} is a convenient wrapper for \code{seq_len(nrow(x))}.
#'
#' @param x A data frame.
#'
#' @return A numeric sequence from 1 to number of columns or rows.
#'
#' @examples
#' data(iris)
#' seq_col(iris)
#' seq_row(iris)
#'
#' @export
seq_col <- function(x) {
  if (!is.matrix(x) && !is.data.frame(x)) {
    warning("`x` needs to be a matrix or data frame.", call. = F)
    return(NULL)
  }

  seq_len(ncol(x))
}

#' @rdname seq_col
#' @export
seq_row <- function(x) {
  if (!is.matrix(x) && !is.data.frame(x)) {
    warning("`x` needs to be a matrix or data frame.", call. = F)
    return(NULL)
  }

  seq_len(nrow(x))
}
