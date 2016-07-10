#' @title Merges multiple imputed data frames into a single data frame
#' @name merge_imputations
#'
#' @description This function merges multiple imputed data frames from
#'                \code{\link[mice]{mids}}-objects into a single data frame
#'                by appending the imputed variables to the original data frame.
#'
#' @param dat The data frame that imputed and used as argument in the
#'        \code{\link[mice]{mice}}-function call.
#' @param imp The \code{\link[mice]{mids}}-object with the imputed data frames
#'        from \code{dat}.
#' @param ori Optional, data frame specifying the original data frame with
#'        missing values, if \code{dat} was only a subset of \code{ori} that
#'        was imputed. If \code{ori} is specified, the imputed variables are
#'        appended to this data frame; else, they are appended to \code{dat}.
#'
#' @return \code{dat}, with appended imputed variables; or \code{ori} with
#'         imputed variables, if \code{ori} was specified.
#'
#' @details This method merges multiple imputations of variables into a single
#'          variable by calculation the (rounded) mean of all imputed values
#'          of missing values. By this, each missing value is replaced by
#'          those values that have been imputed the most times.
#'          \cr \cr
#'          \code{imp} must be a \code{mids}-object, which is returned by the
#'          \code{mice}-function of the \pkg{mice}-package. This function than
#'          creates a data frame for each imputed variable, by combining all
#'          imputations (as returned by the \code{\link[mice]{complete}}-function)
#'          of each variable, and computing the row means of this data frame.
#'          The mean value is then rounded for non-integer values (numerical
#'          values with fractional part), which corresponds to the most frequent
#'          imputed value for a missing value. The original variable with missing
#'          is then copied, missing values replaced by the most frequent imputed
#'          value and appended as new column to the original data frame.
#'
#' @examples
#' library(mice)
#' imp <- mice(nhanes)
#' merge_imputations(nhanes, imp)
#'
#' @export
merge_imputations <- function(dat, imp, ori = NULL) {
  # check if suggested package is available
  if (!requireNamespace("mice", quietly = TRUE)) {
    stop("Package `mice` needed for this function to work. Please install it.", call. = FALSE)
  }
  # check class es
  if (class(imp) != "mids")
    stop("`imp` must be a `mids`-object, as returned by the `mice`-function.", call. = F)
  # check class es
  if (!is.data.frame(dat))
    stop("`dat` must be data frame.", call. = F)
  # check class es
  if (!is.null(ori) && !is.data.frame(ori))
    stop("`ori` must be data frame.", call. = F)

  # copy data frame with missing to return data frame,
  # if not specified
  if (is.null(ori)) ori <- dat
  # iterate all variables of data frame that has missing values
  for (i in seq_len(ncol(dat))) {
    # check if current variable was imputed or not
    if (!is_empty(imp$method[i])) {
      # copy indices of missing values from original variable
      miss_inc <- which(is.na(dat[[i]]))
      miss_inc_dat <- data.frame()
      # create a new data frame from all imputation steps, where only the
      # imputations of the current variables are in
      for (j in seq_len(imp$m)) {
        if (ncol(miss_inc_dat) == 0)
          miss_inc_dat <- data.frame(inc = mice::complete(imp, action = j)[[i]])
        else
          miss_inc_dat <- cbind(miss_inc_dat, data.frame(inc = mice::complete(imp, action = j)[[i]]))
      }
      # convert imputed variable to numeric. needed to perform row means.
      miss_inc_dat <- to_value(miss_inc_dat)
      # copy original variable with missings to a new dummy vector
      x <- ori[[colnames(dat)[i]]]
      # now compute the row means for this variable from all imputed variables
      # (which are in the data frame miss_inc_dat). This mean value represents
      # the most imputed value for a missing value. Copy this "final imputed"
      # value into the variable with missings, thus replacing the missings
      # in the original variable with the most likely imputed value. For numeric
      # integer values, this mean is rounded.
      if (is.numeric(x) && !all(x %% 1 == 0, na.rm = T))
        x[miss_inc] <- rowMeans(miss_inc_dat[miss_inc, ])
      else
        x[miss_inc] <- round(rowMeans(miss_inc_dat[miss_inc, ]))
      # append the imputed variabke to the original data frame and preserve
      # the non-imputed variable with missing values as well
      ori <- cbind(ori, x)
      # give meaningful column-/variable name.
      colnames(ori)[ncol(ori)] <- sprintf("%s_imp", colnames(dat)[i])
    }
  }
  # return data frame with appended imputed variables
  return(ori)
}