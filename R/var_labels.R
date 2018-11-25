#' @title Rename variables
#' @name var_rename
#'
#' @description This function renames variables in a data frame, i.e. it
#'    renames the columns of the data frame.
#'
#' @param x A data frame.
#' @param ... Pairs of named vectors, where the name (lhs) equals the column name
#'    that should be renamed, and the value (rhs) is the new column name.
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
#' @importFrom rlang ensyms as_string
#' @export
var_rename <- function(x, ...) {
  # get dots
  .dots <- match.call(expand.dots = FALSE)$`...`

  if (inherits(.dots, "pairlist"))
    .dots <- lapply(rlang::ensyms(...), rlang::as_string) %>% unlist()
  else
    .dots <- unlist(.dots)

  # select variables
  old_names <- names(.dots)
  # get new variable names
  new_names <- unname(.dots)


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
