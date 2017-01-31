#' @title Add columns to a data frame
#' @name add_columns
#'
#' @description This function combines two or more data frames, but unlike
#'              \code{\link{cbind}} or \code{\link[dplyr]{bind_cols}}, this function
#'              binds \code{data} as last columns of a data frame.
#'
#' @param data A data frame. Will be bound after data frames specified in \code{...}.
#' @param ... More data frames to combine.
#'
#' @return A data frame, where columns of \code{data} are appended after columns
#'         of \code{...}.
#'
#' @examples
#' data(efc)
#' d1 <- efc[, 1:3]
#' d2 <- efc[, 4:6]
#'
#' library(dplyr)
#' head(bind_cols(d1, d2))
#' add_columns(d1, d2)
#'
#' # use case: we take the original data frame, select specific
#' # variables and do some transformations or  recodings
#' # (standardization in this example) and add the new, transformed
#' # variables *to the end* of the original data frame
#' efc %>%
#'   select(e17age, c160age) %>%
#'   std() %>%
#'   add_columns(efc)
#'
#' @importFrom dplyr bind_cols
#' @importFrom tibble as_tibble
#' @export
add_columns <- function(data, ...) {
  tibble::as_tibble(dplyr::bind_cols(..., data))
}
