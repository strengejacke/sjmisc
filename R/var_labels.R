#' @rdname set_label
#' @export
var_labels <- function(x, ...) {
  .Deprecated("var_labels", package = "sjlabelled", msg = "This function will be removed in future versions of sjmisc and has been moved to package 'sjlabelled'. Please use sjlabelled::var_labels() instead.")

  # get dots
  .dots <- match.call(expand.dots = FALSE)$`...`

  # select variables
  vars <- names(unlist(.dots))
  # get new labels
  labels <- unname(unlist(.dots))

  # non-matching column names
  non.vars <- which(vars %nin% colnames(x))

  # check if all variables exist in data frame
  if (!sjmisc::is_empty(non.vars)) {
    # tell user
    warning(sprintf(
      "Following elements are no valid column names in `x`: %s",
      paste(vars[non.vars], collapse = ",")
    ),
    call. = F)
    # remove invalid names
    vars <- vars[-non.vars]
  }

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


  # non-matching column names
  non.match <- which(old_names %nin% colnames(x))

  # check if all variables exist in data frame
  if (!sjmisc::is_empty(non.match)) {
    # tell user
    warning(sprintf(
      "Following elements are no valid column names in `x`: %s",
      paste(old_names[non.match], collapse = ",")
    ),
    call. = F)
    # remove invalid names
    old_names <- old_names[-non.match]
    new_names <- new_names[-non.match]
  }


  # find column indices of variables that should be renamed
  name_pos <- match(old_names, colnames(x))

  # rename column
  colnames(x)[name_pos] <- new_names

  # return data
  x
}
