#' @title Rotate a data frame
#' @name rotate_df
#' @description This function rotates a data frame, i.e. columns become rows
#'              and vice versa.
#'
#' @param x A data frame.
#' @param rn Character vector (optional). If not \code{NULL}, the data frame's
#'           rownames will be added as (first) column to the output, with
#'           \code{rn} being the name of this column.
#' @param cn Logical (optional), if \code{TRUE}, the values of the first column
#'           in \code{x} will be used as column names in the rotated data frame.
#'
#' @return A (rotated) data frame.
#'
#' @examples
#' x <- mtcars[1:3, 1:4]
#' rotate_df(x)
#' rotate_df(x, rn = "property")
#'
#' # use values in 1. column as column name
#' library(tibble)
#' x <- tibble::rownames_to_column(x)
#' rotate_df(x)
#' rotate_df(x, cn = TRUE)
#' rotate_df(x, rn = "property", cn = TRUE)
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
#' as.data.frame(tmp)
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
#' @importFrom dplyr select
#' @export
rotate_df <- function(x, rn = NULL, cn = FALSE) {

  # check if first column has column names
  # that should be used for rotated df

  if (cn) {
    cnames <- x[[1]]
    x <- dplyr::select(x, -1)
  } else
    cnames <- NULL


  # copy attributes
  a <- attributes(x)


  # rotate data frame for 90°

  x <- x %>%
    as.data.frame() %>%
    t() %>%
    as.data.frame()


  # add column names, if requested

  if (!is.null(cnames)) {
    # check if we have correct length of column names
    if (length(cnames) != ncol(x))
      warning("Length of provided column names does not match number of columns. No column names changed.", call. = FALSE)
    else
      colnames(x) <- cnames
  }


  # add rownames as column, if requested
  if (!is.null(rn)) x <- tibble::rownames_to_column(x, var = rn)


  # add back attributes. therefore, delete the common attributes, like class etc.
  # and then add attributes to our final df

  a[c("names", "row.names", "class", "dim", "dimnames")] <- NULL
  attributes(x) <- c(attributes(x), a)

  x
}
