#' @title Rotate a data frame
#' @name rotate_df
#' @description This function rotates a data frame, i.e. columns become rows
#'              and vice versa.
#'
#' @param x A data frame.
#' @param rn Character vector (optional). If not \code{NULL}, the data frame's
#'           rownames will be added as (first) column to the output, with
#'           \code{rn} being the name of this column.
#'
#' @return A (rotated) data frame.
#'
#' @examples
#' x <- mtcars[1:3, 1:4]
#' rotate_df(x)
#' rotate_df(x, rn = "property")
#'
#' # also works on list-results
#' library(purrr)
#'
#' dat <- mtcars[1:3, 1:4]
#' tmp <- purrr::map(dat, function(x) {
#'   sdev <- stats::sd(x, na.rm = TRUE)
#'   ulsdev <- mean(x, na.rm = TRUE) + c(-sdev, sdev)
#'   names(ulsdev) <- c("lower_sd", "upper_sd")
#'   ulsdev
#' })
#' tmp
#' rotate_df(tmp)
#'
#' tmp <- purrr::map_df(dat, function(x) {
#'   sdev <- stats::sd(x, na.rm = TRUE)
#'   ulsdev <- mean(x, na.rm = TRUE) + c(-sdev, sdev)
#'   names(ulsdev) <- c("lower_sd", "upper_sd")
#'   ulsdev
#' })
#' tmp
#' rotate_df(tmp)
#'
#' @importFrom tibble rownames_to_column
#' @export
rotate_df <- function(x, rn = NULL) {
  # rotate data frame for 90°
  x <- x %>%
    as.data.frame() %>%
    t() %>%
    as.data.frame()

  # add rownames as column, if requested
  if (!is.null(rn)) x <- tibble::rownames_to_column(x, var = rn)

  x
}
