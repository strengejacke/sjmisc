#' @title Convert numeric vectors into factors associated value labels
#' @name numeric_to_factor
#'
#' @description This function converts numeric variables into factors,
#'   and uses associated value labels as factor levels.
#'
#' @param x A data frame.
#' @param n Numeric, indicating the maximum amount of unique values in \code{x}
#'   to be considered as "factor". Variables with more unique values than \code{n}
#'   are not converted to factor.
#'
#' @return \code{x}, with numeric values with a maximum of \code{n} unique values
#'   being converted to factors.
#'
#' @details If \code{x} is a labelled vector, associated value labels will be used
#'   as level. Else, the numeric vector is simply coerced using \code{as.factor()}.
#'
#' @examples
#' library(dplyr)
#' data(efc)
#' efc %>%
#'   select(e42dep, e16sex, c12hour, c160age, c172code) %>%
#'   numeric_to_factor()
#'
#' @importFrom dplyr n_distinct
#' @export
numeric_to_factor <- function(x, n = 4) {
  as.data.frame(lapply(x, function(.x) {
    if (is.numeric(.x) && dplyr::n_distinct(.x, na.rm = TRUE) <= n) {

      label <- attr(.x, "label", exact = TRUE)
      labels <- attr(.x, "labels", exact = TRUE)
      labels <- labels[!is.na(labels)]

      if (!sjmisc::is_empty(labels)) {
        for (i in 1:length(labels)) {
          .x[.x == labels[i]] <- names(labels[i])
        }
      }

      .x <- as.factor(.x)
      attr(.x, "label") <- label
    }

    .x
  }))
}
