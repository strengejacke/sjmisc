#' @title Row sums and means for data frames
#' @name row_sums
#'
#' @description \code{row_sums()} simply wraps \code{\link{rowSums}}, while
#'              \code{row_means()} simply wraps \code{\link[sjstats]{mean_n}},
#'              however, the argument-structure of both functions is designed
#'              to work nicely within a pipe-workflow and allows select-helpers
#'              for selecting variables, the default for \code{na.rm} is \code{TRUE},
#'              and the return value is always a tibble (with one variable).
#'
#' @param n May either be
#'          \itemize{
#'            \item a numeric value that indicates the amount of valid values per row to calculate the row mean;
#'            \item or a value between 0 and 1, indicating a proportion of valid values per row to calculate the row mean (see 'Details').
#'          }
#'          If a row's sum of valid values is less than \code{n}, \code{NA} will be returned as row mean value.
#' @param na.rm Logical, \code{TRUE} if missing values should be omitted from
#'          the calculations.
#' @param var Name of new the variable with the row sums or means.
#'
#' @inheritParams to_factor
#' @inheritParams rec
#'
#' @return For \code{row_sums()}, a tibble with a new variable: the row sums from
#'         \code{x}; for \code{row_means()}, a tibble with a new variable: the row
#'         means from \code{x}. If \code{append = FALSE}, only the new variable
#'         with row sums resp. row means is returned.
#'
#' @details For \code{n}, must be a numeric value from \code{0} to \code{ncol(x)}. If
#'          a \emph{row} in \code{x} has at least \code{n} non-missing values, the
#'          row mean is returned. If \code{n} is a non-integer value from 0 to 1,
#'          \code{n} is considered to indicate the proportion of necessary non-missing
#'          values per row. E.g., if \code{n = .75}, a row must have at least \code{ncol(x) * n}
#'          non-missing values for the row mean to be calculated. See 'Examples'.
#'
#' @examples
#' data(efc)
#' efc %>% row_sums(c82cop1:c90cop9, append = FALSE)
#'
#' library(dplyr)
#' row_sums(efc, contains("cop"), append = FALSE)
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
#' row_means(dat, c1:c4, n = 4)
#' # at least 40% non-missing
#' row_means(dat, c1:c4, n = .4)
#'
#' # create sum-score of COPE-Index, and append to data
#' efc %>%
#'   select(c82cop1:c90cop9) %>%
#'   row_sums()
#'
#' @importFrom tibble as_tibble
#' @export
row_sums <- function(x, ..., na.rm = TRUE, var = "rowsums", append = TRUE) {
  # evaluate arguments, generate data
  .dat <- get_dot_data(x, dplyr::quos(...))


  # remember original data, if user wants to bind columns
  orix <- tibble::as_tibble(x)

  if (is.data.frame(x)) {
    rs <- rowSums(.dat, na.rm = na.rm)
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
row_means <- function(x, ..., n, var = "rowmeans", append = TRUE) {
  # evaluate arguments, generate data
  .dat <- get_dot_data(x, dplyr::quos(...))

  # remember original data, if user wants to bind columns
  orix <- tibble::as_tibble(x)

  if (is.data.frame(x)) {
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
