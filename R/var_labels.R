#' @title Rename variables
#' @name var_rename
#'
#' @description This function renames variables in a data frame, i.e. it
#'    renames the columns of the data frame.
#'
#' @param x A data frame.
#' @param ... A named vector, or pairs of named vectors, where the name (lhs)
#'   equals the column name that should be renamed, and the value (rhs) is
#'   the new column name.
#' @param verbose Logical, if \code{TRUE}, a warning is displayed when variable
#'    names do not exist in \code{x}.
#'
#' @return \code{x}, with new column names for those variables specified in \code{...}.
#'
#' @examples
#' dummy <- data.frame(
#'   a = sample(1:4, 10, replace = TRUE),
#'   b = sample(1:4, 10, replace = TRUE),
#'   c = sample(1:4, 10, replace = TRUE)
#' )
#'
#' var_rename(dummy, a = "first.col", c = "3rd.col")
#'
#' # using quasi-quotation
#' library(rlang)
#' v1 <- "first.col"
#' v2 <- "3rd.col"
#' var_rename(dummy, a = !!v1, c = !!v2)
#'
#' x1 <- "a"
#' x2 <- "b"
#' var_rename(dummy, !!x1 := !!v1, !!x2 := !!v2)
#'
#' # using a named vector
#' new_names <- c(a = "first.col", c = "3rd.col")
#' var_rename(dummy, new_names)
#'
#' @importFrom rlang ensyms as_string
#' @export
var_rename <- function(x, ..., verbose = TRUE) {
  # get dots
  .dots <- match.call(expand.dots = FALSE)$`...`

  if (inherits(.dots, "pairlist")) {
    d <- lapply(rlang::ensyms(...), rlang::as_string) %>% unlist()
    # we might have a simple named vector
    if (sjmisc::is_empty(names(d)) && length(.dots) == 1) d <- eval(.dots[[1]])
    .dots <- d
  } else {
    .dots <- unlist(.dots)
  }

  # select variables
  old_names <- names(.dots)
  # get new variable names
  new_names <- unname(.dots)


  # non-matching column names
  non.match <- which(old_names %nin% colnames(x))

  # check if all variables exist in data frame
  if (!sjmisc::is_empty(non.match)) {
    if (verbose) {
      # tell user
      warning(sprintf(
        "Following elements are no valid column names in `x`: %s",
        paste(old_names[non.match], collapse = ",")
      ),
      call. = F)
    }
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


#' @rdname var_rename
#' @export
rename_variables <- var_rename

#' @rdname var_rename
#' @export
rename_columns <- var_rename
