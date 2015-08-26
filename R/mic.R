#' @title Mean Inter-Item-Correlation
#' @name mic
#' @description This function calculates a mean inter-item-correlation, i.e.
#'                a correlation matrix of \code{data} will be computed (unless
#'                \code{data} is already a matrix as returned by the
#'                \code{\link{cor}}-function) and the mean
#'                of the sum of all item's correlation values is returned.
#'                Requires either a data frame or a computed \code{\link{cor}}-object.
#'
#' @param data A \code{matrix} as returned by the \code{\link{cor}}-function, or
#'          a data frame which correlations should be calculated.
#' @param cor.method Indicates the correlation computation method. May be one of
#'          \code{"spearman"} (default), \code{"pearson"} or \code{"kendall"}.
#'          You may use initial letter only.
#' @return The value of the computed mean inter-item-correlation.
#'
#' @examples
#' # -------------------------------
#' # Data from the EUROFAMCARE sample dataset
#' # -------------------------------
#' data(efc)
#' # recveive first item of COPE-index scale
#' start <- which(colnames(efc) == "c82cop1")
#' # recveive last item of COPE-index scale
#' end <- which(colnames(efc) == "c90cop9")
#' # create data frame with COPE-index scale
#' df <- data.frame(efc[, c(start:end)])
#'
#' mic(df)
#'
#' @importFrom stats cor na.omit
#' @export
mic <- function(data, cor.method = "pearson") {
  # -----------------------------------
  # Check parameter
  # -----------------------------------
  if (cor.method == "s") cor.method <- "spearman"
  if (cor.method == "p") cor.method <- "pearson"
  if (cor.method == "k") cor.method <- "kendall"
  # -----------------------------------
  # Mean-interitem-corelation
  # -----------------------------------
  if (class(data) == "matrix") {
    corr <- data
  } else {
    data <- stats::na.omit(data)
    corr <- stats::cor(data, method = cor.method)
  }
  # -----------------------------------
  # Sum up all correlation values
  # -----------------------------------
  meanic <- c()
  for (j in 1:(ncol(corr) - 1)) {
    # first correlation is always "1" (self-correlation)
    for (i in (j + 1):nrow(corr)) {
      # check for valid bound
      if (i <= nrow(corr) && j <= ncol(corr)) {
        # add up all subsequent values
        meanic <- c(meanic, corr[i, j])
      } else {
        meanic <- c(meanic, "NA")
      }
    }
  }
  return(mean(meanic))
}
