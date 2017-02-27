#' @rdname set_label
#' @export
var_labels <- function(x, ...) {
  # get dots
  .dots <- match.call(expand.dots = FALSE)$`...`

  # select variables
  vars <- names(unlist(.dots))
  # get new labels
  labels <- unname(unlist(.dots))

  # set label for all variables
  for (i in seq_len(length(vars))) {
    attr(x[[vars[i]]], "label") <- labels[i]
  }

  # return data
  x
}



#' @title Rename variables
#' @name var_rename
#'
#' @description This function renames variables in a data frame, i.e. it
#'                renames the columns of the data frame.
#'
#' @param ... Pairs of named vectors, where the name equals the column name that
#'          should be renamed, and the value is the new column name.
#' @inheritParams set_label
#'
#' @return \code{x}, with new column names for those variables specified in \code{...}.
#'
#' @examples
#' # Set variable labels for data frame
#' dummy <- data.frame(a = sample(1:4, 10, replace = TRUE),
#'                     b = sample(1:4, 10, replace = TRUE),
#'                     c = sample(1:4, 10, replace = TRUE))
#'
#' var_rename(dummy, a = "first.col", c = "3rd.col")
#'
#' @export
var_rename <- function(x, ...) {
  # get dots
  .dots <- match.call(expand.dots = FALSE)$`...`

  # select variables
  old_names <- names(unlist(.dots))
  # get new variable names
  new_names <- unname(unlist(.dots))

  # find column indices of variables that should be renamed
  name_pos <- match(old_names, colnames(x))

  # rename column
  colnames(x)[name_pos] <- new_names

  # return data
  x
}
