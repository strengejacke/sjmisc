#' @title Cronbach's Alpha for a matrix or data frame
#' @name cronb
#' @description This function calculates the Cronbach's alpha value
#'                of a data frame or matrix.
#'
#' @seealso \code{\link{reliab_test}}
#'
#' @param df \code{data.frame} or matrix with more than 2 columns.
#' @return The Cronbach's alpha value for \code{df}.
#'
#' @note See 'Examples' from \code{\link[sjPlot]{sjp.pca}} and \code{\link[sjPlot]{sjt.pca}}.
#'
#' @importFrom stats na.omit var
#' @export
cronb <- function(df) {
  df <- stats::na.omit(df)
  if (is.null(ncol(df)) || ncol(df) < 2) {
    warning("Too less columns in this factor to calculate alpha value!", call. = F)
    return(NULL)
  }
  return(dim(df)[2] / (dim(df)[2] - 1) * (1 - sum(apply(df, 2, var)) / stats::var(rowSums(df))))
}
