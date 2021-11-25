#' @title Move columns to other positions in a data frame
#' @name move_columns
#'
#' @description \code{move_columns()} moves one or more columns in a data frame
#'    to another position.
#'
#' @param data A data frame.
#' @param ... Unquoted names or character vector with names of variables that
#'   should be move to another position. You may also use functions like
#'   \code{:} or tidyselect's select-helpers.
#' @param .before Optional, column name or numeric index of the position where
#'   \code{col} should be moved to. If not missing, \code{col} is moved to the
#'   position \emph{before} the column indicated by \code{.before}.
#' @param .after Optional, column name or numeric index of the position where
#'   \code{col} should be moved to. If not missing, \code{col} is moved to the
#'   position \emph{after} the column indicated by \code{.after}.
#'
#' @return \code{data}, with resorted columns.
#'
#' @note If neither \code{.before} nor \code{.after} are specified, the
#'    column is moved to the end of the data frame by default. \code{.before}
#'    and \code{.after} are evaluated in a non-standard fashion, so you need
#'    quasi-quotation when the value for \code{.before} or \code{.after} is
#'    a vector with the target-column value. See 'Examples'.
#'
#' @examples
#' \dontrun{
#' data(iris)
#'
#' iris %>%
#'   move_columns(Sepal.Width, .after = "Species") %>%
#'   head()
#'
#' iris %>%
#'   move_columns(Sepal.Width, .before = Sepal.Length) %>%
#'   head()
#'
#' iris %>%
#'   move_columns(Species, .before = 1) %>%
#'   head()
#'
#' iris %>%
#'   move_columns("Species", "Petal.Length", .after = 1) %>%
#'   head()
#'
#' library(dplyr)
#' iris %>%
#'   move_columns(contains("Width"), .after = "Species") %>%
#'   head()}
#'
#' # using quasi-quotation
#' target <- "Petal.Width"
#' # does not work, column is moved to the end
#' iris %>%
#'   move_columns(Sepal.Width, .after = target) %>%
#'   head()
#'
#' # using !! works
#' iris %>%
#'   move_columns(Sepal.Width, .after = !!target) %>%
#'   head()
#' @export
move_columns <- function(data, ..., .before, .after) {

  # copy attributes
  a <- attributes(data)

  variables <- dplyr::quos(...)
  dat <- dplyr::select(data, !!! variables)

  remaining <- which(!(colnames(data) %in% colnames(dat)))
  data <- dplyr::select(data, !! remaining)

  pos.before <- rlang::quo_name(rlang::enquo(.before))
  pos.after <- rlang::quo_name(rlang::enquo(.after))

  if (sjmisc::is_empty(pos.before) && sjmisc::is_empty(pos.after)) {
    pos.after <- Inf
  } else {

    if (!sjmisc::is_empty(pos.before)) {
      if (is_num_chr(pos.before))
        pos.after <- as.numeric(pos.before) - 1
      else
        pos.after <- which(colnames(data) == pos.before) - 1
    } else if (!sjmisc::is_empty(pos.after)) {
      if (is_num_chr(pos.after))
        pos.after <- as.numeric(pos.after)
      else
        pos.after <- which(colnames(data) == pos.after)
    } else {
      pos.after <- Inf
    }

  }

  # final test, to make sure we have a valid value here
  if (!length(pos.after)) pos.after <- Inf

  if (!is.infinite(pos.after) && pos.after < 1) {
    x <- cbind(dat, data)
  } else if (is.infinite(pos.after) || pos.after >= ncol(data)) {
    x <- cbind(data, dat)
  } else {
    c1 <- 1:pos.after
    c2 <- (pos.after + 1):ncol(data)

    x1 <- dplyr::select(data, !! c1)
    x2 <- dplyr::select(data, !! c2)

    x <- cbind(x1, dat, x2)
  }

  a[names(a) %in% names(attributes(x))] <- NULL
  attributes(x) <- c(attributes(x), a)

  x
}
