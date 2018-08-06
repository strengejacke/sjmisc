#' @title Row sums and means for data frames
#' @name row_sums
#'
#' @description \code{row_sums()} and \code{row_means()} compute row sums or means
#'    for at least \code{n} valid values per row. The functions are designed
#'    to work nicely within a pipe-workflow and allow select-helpers
#'    for selecting variables.
#'
#' @param n May either be
#'          \itemize{
#'            \item a numeric value that indicates the amount of valid values per row to calculate the row mean or sum;
#'            \item a value between 0 and 1, indicating a proportion of valid values per row to calculate the row mean or sum (see 'Details').
#'            \item or \code{Inf}. If \code{n = Inf}, all values per row must be non-missing to compute row mean or sum.
#'          }
#'          If a row's sum of valid (i.e. non-\code{NA}) values is less than \code{n}, \code{NA} will be returned as value for the row mean or sum.
#' @param var Name of new the variable with the row sums or means.
#'
#' @inheritParams to_factor
#' @inheritParams rec
#'
#' @return For \code{row_sums()}, a tibble with a new variable: the row sums from
#'    \code{x}; for \code{row_means()}, a tibble with a new variable: the row
#'    means from \code{x}. If \code{append = FALSE}, only the new variable
#'    with row sums resp. row means is returned. \code{total_mean()} returns
#'    the mean of all values from all specified columns in a data frame.
#'
#' @details For \code{n}, must be a numeric value from \code{0} to \code{ncol(x)}. If
#'    a \emph{row} in \code{x} has at least \code{n} non-missing values, the
#'    row mean or sum is returned. If \code{n} is a non-integer value from 0 to 1,
#'    \code{n} is considered to indicate the proportion of necessary non-missing
#'    values per row. E.g., if \code{n = .75}, a row must have at least \code{ncol(x) * n}
#'    non-missing values for the row mean or sum to be calculated. See 'Examples'.
#'
#' @examples
#' data(efc)
#' efc %>% row_sums(c82cop1:c90cop9, n = 3, append = FALSE)
#'
#' library(dplyr)
#' row_sums(efc, contains("cop"), n = 2, append = FALSE)
#'
#' dat <- data.frame(
#'   c1 = c(1,2,NA,4),
#'   c2 = c(NA,2,NA,5),
#'   c3 = c(NA,4,NA,NA),
#'   c4 = c(2,3,7,8),
#'   c5 = c(1,7,5,3)
#' )
#' dat
#'
#' row_means(dat, n = 4)
#' row_sums(dat, n = 4)
#'
#' row_means(dat, c1:c4, n = 4)
#' # at least 40% non-missing
#' row_means(dat, c1:c4, n = .4)
#' row_sums(dat, c1:c4, n = .4)
#'
#' # total mean of all values in the data frame
#' total_mean(dat)
#'
#' # create sum-score of COPE-Index, and append to data
#' efc %>%
#'   select(c82cop1:c90cop9) %>%
#'   row_sums(n = 1)
#'
#' @export
row_sums <- function(x, ...) {
  UseMethod("row_sums")
}


#' @importFrom dplyr quos bind_cols
#' @importFrom tibble as_tibble
#' @rdname row_sums
#' @export
row_sums.default <- function(x, ..., n, var = "rowsums", append = TRUE) {
  # evaluate arguments, generate data
  .dat <- get_dot_data(x, dplyr::quos(...))

  # remember original data, if user wants to bind columns
  orix <- tibble::as_tibble(x)

  if (is.data.frame(x)) {

    # for Inf-values, use all columns
    if (is.infinite(n)) n <- ncol(.dat)

    # is 'n' indicating a proportion?
    digs <- n %% 1
    if (digs != 0) n <- round(ncol(.dat) * digs)

    # check if we have a data framme with at least two columns
    if (ncol(.dat) < 2) {
      warning("`x` must be a data frame with at least two columns.", call. = TRUE)
      return(NA)
    }

    # n may not be larger as df's amount of columns
    if (ncol(.dat) < n) {
      warning("`n` must be smaller or equal to number of columns in data frame.", call. = TRUE)
      return(NA)
    }

    rs <- apply(.dat, 1, function(x) ifelse(sum(!is.na(x)) >= n, sum(x, na.rm = TRUE), NA))
  } else {
    stop("`x` must be a data frame.", call. = F)
  }


  # to tibble, and rename variable
  rs <- tibble::as_tibble(rs)
  colnames(rs) <- var

  # combine data
  if (append) rs <- dplyr::bind_cols(orix, rs)

  rs
}


#' @rdname row_sums
#' @export
row_sums.mids <- function(x, ..., var = "rowsums", append = TRUE) {
  rfun <- rowSums
  row_mids(x = x, ..., var = var, append = append, rfun = rfun)
}


#' @rdname row_sums
#' @export
row_means <- function(x, ...) {
  UseMethod("row_means")
}


#' @rdname row_sums
#' @export
total_mean <- function(x, ...) {
  UseMethod("total_mean")
}


#' @export
total_mean.data.frame <- function(x, ...) {
  # evaluate arguments, generate data
  .dat <- get_dot_data(x, dplyr::quos(...))
  sum(colSums(.dat, na.rm = TRUE)) / sum(apply(.dat, 1:2, function(x) !is.na(x)))
}


#' @importFrom dplyr quos bind_cols
#' @importFrom tibble as_tibble
#' @rdname row_sums
#' @export
row_means.default <- function(x, ..., n, var = "rowmeans", append = TRUE) {
  # evaluate arguments, generate data
  .dat <- get_dot_data(x, dplyr::quos(...))

  # remember original data, if user wants to bind columns
  orix <- tibble::as_tibble(x)

  if (is.data.frame(x)) {
    # for Inf-values, use all columns
    if (is.infinite(n)) n <- ncol(.dat)

    # is 'n' indicating a proportion?
    digs <- n %% 1
    if (digs != 0) n <- round(ncol(.dat) * digs)

    # check if we have a data framme with at least two columns
    if (ncol(.dat) < 2) {
      warning("`x` must be a data frame with at least two columns.", call. = TRUE)
      return(NA)
    }

    # n may not be larger as df's amount of columns
    if (ncol(.dat) < n) {
      warning("`n` must be smaller or equal to number of columns in data frame.", call. = TRUE)
      return(NA)
    }

    rm <- apply(.dat, 1, function(x) ifelse(sum(!is.na(x)) >= n, mean(x, na.rm = TRUE), NA))

  } else {
    stop("`x` must be a data frame.", call. = F)
  }

  # to tibble, and rename variable
  rm <- tibble::as_tibble(rm)
  colnames(rm) <- var

  # combine data
  if (append) rm <- dplyr::bind_cols(orix, rm)

  rm
}


#' @rdname row_sums
#' @export
row_means.mids <- function(x, ..., var = "rowmeans", append = TRUE) {
  rfun <- rowMeans
  row_mids(x = x, ..., var = var, append = append, rfun = rfun)
}


#' @importFrom dplyr quos group_by select
#' @importFrom tidyr nest unnest
#' @importFrom purrr map
row_mids <- function(x, ..., var, append, rfun, count = NULL) {
  # check if suggested package is available
  if (!requireNamespace("mice", quietly = TRUE))
    stop("Package `mice` needed for this function to work. Please install it.", call. = FALSE)

  # check classes
  if (!inherits(x, "mids"))
    stop("`x` must be a `mids`-object, as returned by the `mice()`-function.", call. = F)


  # quote dots and convert mids into long-data.frame

  vars <- dplyr::quos(...)
  long <- mice::complete(x, action = "long", include = TRUE)


  # group by imputation, so we can easily iterate each imputed dataset

  ndf <- long %>%
    dplyr::group_by(.data$.imp) %>%
    tidyr::nest()


  # select variable and compute rowsums. add this variable
  # to each imputed

  if (is.null(count)) {
    ndf$data <- purrr::map(ndf$data, ~ mutate(
      .x,
      rowsums = .x %>%
        dplyr::select(!!! vars) %>%
        rfun()
    ))
  } else {
    ndf$data <- purrr::map(ndf$data, ~ mutate(
      .x,
      rowsums = .x %>%
        dplyr::select(!!! vars) %>%
        rfun(count = count)
    ))
  }


  # rename new variable

  ndf$data <- purrr::map(ndf$data, function(.x) {
    colnames(.x)[ncol(.x)] <- var
    .x
  })


  # return mids-object. need to use "as.data.frame()",
  # because "as.mids()" can't cope with tibbles

  ndf %>%
    tidyr::unnest() %>%
    as.data.frame() %>%
    mice::as.mids()
}
