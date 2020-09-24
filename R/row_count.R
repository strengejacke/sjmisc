#' @title Count row or column indices
#' @name row_count
#'
#' @description \code{row_count()} mimics base R's \code{rowSums()}, with sums
#'              for a specific value indicated by \code{count}. Hence, it is equivalent
#'              to \code{rowSums(x == count, na.rm = TRUE)}. However, this function
#'              is designed to work nicely within a pipe-workflow and allows select-helpers
#'              for selecting variables and the return value is always a data frame
#'              (with one variable).
#'              \cr \cr
#'              \code{col_count()} does the same for columns. The return value is
#'              a data frame with one row (the column counts) and the same number
#'              of columns as \code{x}.
#'
#' @param count The value for which the row or column sum should be computed. May
#'          be a numeric value, a character string (for factors or character vectors),
#'          \code{NA}, \code{Inf} or \code{NULL} to count missing or infinite values,
#'          or null-values.
#' @param var Name of new the variable with the row or column counts.
#'
#' @inheritParams to_dummy
#' @inheritParams rec
#'
#' @return For \code{row_count()}, a data frame with one variable: the sum of \code{count}
#'         appearing in each row of \code{x}; for \code{col_count()}, a data frame with
#'         one row and the same number of variables as in \code{x}: each variable
#'         holds the sum of \code{count} appearing in each variable of \code{x}.
#'         If \code{append = TRUE}, \code{x} including this variable will be returned.
#'
#' @examples
#' dat <- data.frame(
#'   c1 = c(1, 2, 3, 1, 3, NA),
#'   c2 = c(3, 2, 1, 2, NA, 3),
#'   c3 = c(1, 1, 2, 1, 3, NA),
#'   c4 = c(1, 1, 3, 2, 1, 2)
#' )
#'
#' row_count(dat, count = 1, append = FALSE)
#' row_count(dat, count = NA, append = FALSE)
#' row_count(dat, c1:c3, count = 2, append = TRUE)
#'
#' col_count(dat, count = 1, append = FALSE)
#' col_count(dat, count = NA, append = FALSE)
#' col_count(dat, c1:c3, count = 2, append = TRUE)
#'
#' @export
row_count <- function(x, ..., count, var = "rowcount", append = TRUE) {
  UseMethod("row_count")
}


#' @importFrom dplyr quos bind_cols
#' @export
row_count.default <- function(x, ..., count, var = "rowcount", append = TRUE) {
  # evaluate arguments, generate data
  .dat <- get_dot_data(x, dplyr::quos(...))


  # remember original data, if user wants to bind columns
  orix <- x

  if (is.data.frame(x)) {
    rc <- row.count(.dat, count)
  } else {
    stop("`x` must be a data frame.", call. = F)
  }


  # rename variable
  rc <- as.data.frame(rc)
  colnames(rc) <- var

  # combine data
  if (append) rc <- dplyr::bind_cols(orix, rc)

  rc
}


#' @export
row_count.mids <- function(x, ..., count, var = "rowcount", append = TRUE) {
  rfun <- row.count
  row_mids(x = x, ..., var = var, append = append, rfun = rfun, count = count)
}


row.count <- function(.dat, count) {
  if (is.na(count))
    rc <- apply(.dat, 1, function(x) sum(is.na(x), na.rm = TRUE))
  else if (is.infinite(count))
    rc <- apply(.dat, 1, function(x) sum(is.infinite(x), na.rm = TRUE))
  else if (is.null(count))
    rc <- apply(.dat, 1, function(x) sum(is.null(x), na.rm = TRUE))
  else
    rc <- apply(.dat, 1, function(x) sum(x == count, na.rm = TRUE))

  rc
}


#' @rdname row_count
#' @importFrom purrr map_df
#' @importFrom dplyr quos bind_rows
#' @export
col_count <- function(x, ..., count, var = "colcount", append = TRUE) {
  # evaluate arguments, generate data
  .dat <- get_dot_data(x, dplyr::quos(...))


  # remember original data, if user wants to bind columns
  orix <- x

  if (is.data.frame(x)) {
    if (is.na(count))
      rc <- purrr::map_df(.dat, function(x) sum(is.na(x), na.rm = TRUE))
    else if (is.infinite(count))
      rc <- purrr::map_df(.dat, function(x) sum(is.infinite(x), na.rm = TRUE))
    else if (is.null(count))
      rc <- purrr::map_df(.dat, function(x) sum(is.null(x), na.rm = TRUE))
    else
      rc <- purrr::map_df(.dat, function(x) sum(x == count, na.rm = TRUE))
  } else {
    stop("`x` must be a data frame.", call. = F)
  }


  # combine data
  if (append) rc <- dplyr::bind_rows(orix, rc)

  rc
}
