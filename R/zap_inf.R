#' @title Convert infiite or NaN values into regular NA
#' @name zap_inf
#'
#' @description Replaces all infinite (\code{Inf} and \code{-Inf}) or \code{NaN}
#'                values with regular \code{NA}.
#'
#' @param x A vector or a data frame.
#'
#' @inheritParams to_factor
#'
#' @return \code{x}, where all \code{Inf}, \code{-Inf} and \code{NaN} are converted to \code{NA}.
#'
#' @examples
#' x <- c(1, 2, NA, 3, NaN, 4, NA, 5, Inf, -Inf, 6, 7)
#' zap_inf(x)
#'
#' data(efc)
#' # produce some NA and NaN values
#' efc$e42dep[1] <- NaN
#' efc$e42dep[2] <- NA
#' efc$c12hour[1] <- NaN
#' efc$c12hour[2] <- NA
#' efc$e17age[2] <- NaN
#' efc$e17age[1] <- NA
#'
#' # only zap NaN for c12hour
#' zap_inf(efc$c12hour)
#'
#' # only zap NaN for c12hour and e17age, not for e42dep,
#' # but return complete data framee
#' zap_inf(efc, c12hour, e17age)
#'
#' # zap NaN for complete data frame
#' zap_inf(efc)
#'
#' @importFrom tibble as_tibble
#' @export
zap_inf <- function(x, ...) {
  # evaluate arguments, generate data
  .dat <- get_dot_data(x, dplyr::quos(...))

  if (is.data.frame(x)) {
    # iterate variables of data frame
    for (i in colnames(.dat)) {
      # convert NaN and Inf to missing
      x[[i]][is.nan(x[[i]])] <- NA
      x[[i]][is.infinite(x[[i]])] <- NA
    }
    # coerce to tibble
    x <- tibble::as_tibble(x)
  } else {
    x[is.nan(x)] <- NA
    x[is.infinite(x)] <- NA
  }

  x
}
