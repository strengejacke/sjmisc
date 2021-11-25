#' @title Add variables or cases to data frames
#' @name add_variables
#'
#' @description \code{add_variables()} adds a new column to a data frame, while
#'   \code{add_case()} adds a new row to a data frame. These are convenient
#'   functions to add columns or rows not only at the end of a data frame,
#'   but at any column or row position. Furthermore, they allow easy integration
#'   into a pipe-workflow.
#'
#' @param data A data frame.
#' @param ... One or more named vectors that indicate the variables or values,
#'   which will be added as new column or row to \code{data}. For \code{add_case()},
#'   non-matching columns in \code{data} will be filled with \code{NA}.
#' @param .after,.before Numerical index of row or column, where after or before
#'   the new variable or case should be added. If \code{.after = -1}, variables
#'   or cases are added at the beginning; if \code{.after = Inf},
#'   variables and cases are added at the end. In case of \code{add_variables()},
#'   \code{.after} and \code{.before} may also be a character name indicating
#'   the column in \code{data}, after or infront of what \code{...} should be
#'   inserted.
#'
#' @return \code{data}, including the new variables or cases from \code{...}.
#'
#' @note For \code{add_case()}, if variable does not exist, a new variable is
#'    created and existing cases for this new variable get the value \code{NA}.
#'    See 'Examples'.
#'
#' @examples
#' d <- data.frame(
#'   a = c(1, 2, 3),
#'   b = c("a", "b", "c"),
#'   c = c(10, 20, 30),
#'   stringsAsFactors = FALSE
#' )
#'
#' add_case(d, b = "d")
#' add_case(d, b = "d", a = 5, .before = 1)
#'
#' # adding a new case for a new variable
#' add_case(d, e = "new case")
#'
#' add_variables(d, new = 5)
#' add_variables(d, new = c(4, 4, 4), new2 = c(5, 5, 5), .after = "b")
#' @export
add_variables <- function(data, ..., .after = Inf, .before = NULL) {

  # copy attributes
  a <- attributes(data)

  if (is.character(.after))
    .after <- which(colnames(data) == .after)

  if (!is.null(.before) && is.character(.before))
    .after <- which(colnames(data) == .before) - 1

  if (!is.null(.before) && is.numeric(.before))
    .after <- .before - 1

  dat <- data.frame(..., stringsAsFactors = FALSE)

  if (.after < 1) {
    x <- cbind(dat, data)
  } else if (is.infinite(.after) || .after >= ncol(data)) {
    x <- cbind(data, dat)
  } else {
    c1 <- 1:.after
    c2 <- (.after + 1):ncol(data)

    x1 <- dplyr::select(data, !! c1)
    x2 <- dplyr::select(data, !! c2)

    x <- cbind(x1, dat, x2)
  }

  a[names(a) %in% names(attributes(x))] <- NULL
  attributes(x) <- c(attributes(x), a)

  x
}


#' @rdname add_variables
#' @export
add_case <- function(data, ..., .after = Inf, .before = NULL) {

  if (!is.null(.before))
    .after <- .before - 1

  dat <- data.frame(..., stringsAsFactors = FALSE)
  x <- rbind(data, NA)
  last.row <- nrow(x)

  for (.x in colnames(dat)) {
    x[last.row, .x] <- dat[[.x]]
  }

  if (.after < 1)
    o <- c(last.row, 1:(last.row - 1))
  else if (is.infinite(.after) || .after >= nrow(x))
    o <- 1:last.row
  else
    o <- c(1:.after, last.row, (.after + 1):(last.row - 1))

  x[o, , drop = FALSE]
}
