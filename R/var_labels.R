#' @title Add variable label(s) to variables
#' @name var_labels
#'
#' @description This function adds variable labels as label-attribute
#'                (named \emph{"label"}) to a set of variables in a
#'                data frame. Unlike \code{\link{set_label}}, this
#'                function takes several pairs of named vectors as input to change
#'                multiple variable labels in a data frame at once.
#'                \cr \cr
#'                Furthermore, since the first argument is a data frame and multiple
#'                variables can be passed as arguments, this function fits nicely
#'                into a pipe-workflow.
#'
#' @param data A data frame.
#' @param ... Pairs of named vectors, where the name equals the variable name,
#'          which should be labelled, and the value is the new variable label.
#'
#' @return \code{data}, with variable label attribute(s) for those variables
#'           specified in \code{...}.
#'
#' @examples
#' library(dplyr)
#'
#' # Set variable labels for data frame
#' dummy <- data.frame(a = sample(1:4, 10, replace = TRUE),
#'                     b = sample(1:4, 10, replace = TRUE),
#'                     c = sample(1:4, 10, replace = TRUE))
#'
#' dummy %>%
#'   var_labels(a = "First variable", c = "third variable") %>%
#'   get_label()
#'
#' @export
var_labels <- function(data, ...) {
  # get dots
  .dots <- match.call(expand.dots = FALSE)$`...`

  # select variables
  vars <- names(unlist(.dots))
  # get new labels
  labels <- unname(unlist(.dots))

  # set label for all variables
  for (i in seq_len(length(vars))) {
    attr(data[[vars[i]]], "label") <- labels[i]
  }

  # return data
  data
}



#' @title Rename variables
#' @name var_rename
#'
#' @description This function renames variables in a data frame, i.e. it
#'                renames the columns of the data frame.
#'
#' @param data A data frame.
#' @param ... Pairs of named vectors, where the name equals the old variable name
#'          (column name), and the value is the new variable (column) name.
#'
#' @return \code{data}, with new column names for those variables specified in \code{...}.
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
var_rename <- function(data, ...) {
  # get dots
  .dots <- match.call(expand.dots = FALSE)$`...`

  # select variables
  old_names <- names(unlist(.dots))
  # get new variable names
  new_names <- unname(unlist(.dots))

  # find column indices of variables that should be renamed
  name_pos <- match(old_names, colnames(data))

  # rename column
  colnames(data)[name_pos] <- new_names

  # return data
  data
}
