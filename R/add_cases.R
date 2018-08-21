#' @title Add variables or cases to data frames
#' @name add_variables
#'
#' @description \code{add_variables()} adds a new column to a data frame, while
#'   \code{add_cases()} adds a new row to a data frame. These are convenient
#'   functions to add columns or rows not only at the end of a data frame,
#'   but at any column or row position. Furthermore, they allow easy integration
#'   into a pipe-workflow.
#'
#' @param data A data frame. For \code{add_columns()}, will be bound after data
#'   frames specified in \code{...}. For \code{replace_columns()}, duplicated
#'   columns in \code{data} will be replaced by columns in \code{...}.
#' @param ... One or more names vectors that indicate the variables or values,
#'   which will be added as new column or row to \code{data}. For \code{add_cases()},
#'   non-matching columns in \code{data} will be filled with \code{NA}.
#' @param .after,.before Numerical index of row or column, after or before which
#'   the new variable or case should be added. In case of \code{add_variables()},
#'   \code{.after} and \code{.before} may also be a character name indicating
#'   the column in \code{data}, after/before \code{...} should be inserted.
#'
#' @return \code{data}, including the new variables or cases from \code{...}.
#'
#' @examples
#'
#' @importFrom dplyr select
#' @export
add_variables <- function(data, ..., .after = 1, .before = NULL) {
  if (is.character(.after))
    .after <- which(colnames(data) == .after)

  if (!is.null(.before) && is.character(.before))
    .after <- which(colnames(data) == .before) - 1

  dat <- data.frame(..., stringsAsFactors = FALSE)

  if (.after < 1) {
    cbind(dat, data)
  } else if (is.infinite(.after)) {
    cbind(data, dat)
  } else {
    c1 <- 1:.after
    c2 <- (.after + 1):ncol(data)

    x1 <- dplyr::select(data, !! c1)
    x2 <- dplyr::select(data, !! c2)

    cbind(x1, dat, x2)
  }
}


#' @rdname add_variables
#' @importFrom dplyr select
#' @export
add_case <- function(data, ..., .after = -1, .before = NULL) {

  dat <- data.frame(..., stringsAsFactors = FALSE)
  x <- rbind(data, NA)
  last.row <- nrow(x)

  for (.x in colnames(dat)) {
    x[last.row, .x] <- dat[[.x]]
  }

  if (.after < 1)
    o <- c(last.row, 1:(last.row - 1))
  else if (is.infinite(.after))
    o <- 1:last.row
  else
    o <- c(1:.after, last.row, (.after + 1):(last.row) - 1)

  x[o, , drop = FALSE]
}
