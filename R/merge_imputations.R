#' @title Merges multiple imputed data frames into a single data frame
#' @name merge_imputations
#'
#' @description This function merges multiple imputed data frames from
#'                \code{\link[mice]{mids}}-objects into a single data frame
#'                by computing the mean or selecting the most likely imputed value.
#'
#' @param dat The data frame that was imputed and used as argument in the
#'        \code{\link[mice]{mice}}-function call.
#' @param imp The \code{\link[mice]{mids}}-object with the imputed data frames
#'        from \code{dat}.
#' @param ori Optional, if \code{ori} is specified, the imputed variables are
#'        appended to this data frame; else, a new data frame with the imputed
#'        variables is returned.
#' @param summary After merging multiple imputed data, \code{summary} displays
#'          a graphical summary of the "quality" of the merged values, compared
#'          to the original imputed values.
#'          \describe{
#'            \item{\code{"dens"}}{
#'              Creates a density plot, which shows the distribution of the mean
#'              of the imputed values for each variable at each observation. The
#'              larger the areas overlap, the better is the fit of the merged
#'              value compared to the imputed value.
#'            }
#'            \item{\code{"hist"}}{
#'              Similar to \code{summary = "dens"}, however, mean and merged
#'              values are shown as histogram. Bins should have almost equal
#'              height for both groups (mean and merged).
#'            }
#'            \item{\code{"sd"}}{
#'              Creates a dot plot, where data points indicate the standard
#'              deviation for all imputed values (y-axis) at each merged
#'              value (x-axis) for all imputed variables. The higher the
#'              standard deviation, the less precise is the imputation, and
#'              hence the merged value.
#'            }
#'          }
#' @param filter A character vector with variable names that should be plotted.
#'          All non-defined variables will not be shown in the plot.
#'
#'
#' @return A data frame with (merged) imputed variables; or \code{ori} with
#'         appended imputed variables, if \code{ori} was specified.
#'         If \code{summary} is included, returns a list with the data frame
#'         \code{data} with (merged) imputed variables and some other summary
#'         information, which are required for the plot-output.
#'
#' @details This method merges multiple imputations of variables into a single
#'          variable by computing the (rounded) mean of all imputed values
#'          of missing values. By this, each missing value is replaced by
#'          those values that have been imputed the most times.
#'          \cr \cr
#'          \code{imp} must be a \code{mids}-object, which is returned by the
#'          \code{mice}-function of the \pkg{mice}-package. \code{merge_imputations} than
#'          creates a data frame for each imputed variable, by combining all
#'          imputations (as returned by the \code{\link[mice]{complete}}-function)
#'          of each variable, and computing the row means of this data frame.
#'          The mean value is then rounded for integer values (and not for numerical
#'          values with fractional part), which corresponds to the most frequent
#'          imputed value for a missing value. The original variable with missings
#'          is then copied and missing values are replaced by the most frequent imputed
#'          value.
#'
#' @note Typically, further analyses are conducted on pooled results of multiple
#'       imputed data sets (see \code{\link[mice]{pool}}), however, sometimes
#'       (in social sciences) it is also feasible to compute the mean of multiple
#'       imputed variables (see \cite{Burns et al. 2011}).
#'
#' @references Burns RA, Butterworth P, Kiely KM, Bielak AAM, Luszcz MA, Mitchell P, et al. 2011. Multiple imputation was an efficient method for harmonizing the Mini-Mental State Examination with missing item-level data. Journal of Clinical Epidemiology;64:787–93 \doi{10.1016/j.jclinepi.2010.10.011}
#'
#' @examples
#' library(mice)
#' imp <- mice(nhanes)
#'
#' # return data frame with imputed variables
#' merge_imputations(nhanes, imp)
#'
#' # append imputed variables to original data frame
#' merge_imputations(nhanes, imp, nhanes)
#'
#' # show summary of quality of merging imputations
#' merge_imputations(nhanes, imp, summary = "dens", filter = c("chl", "hyp"))
#'
#' @importFrom dplyr bind_cols
#' @export
merge_imputations <- function(dat, imp, ori = NULL, summary = c("none", "dens", "hist", "sd"), filter = NULL) {

  summary <- match.arg(summary)

  # check if suggested package is available
  if (!requireNamespace("mice", quietly = TRUE)) {
    stop("Package `mice` needed for this function to work. Please install it.", call. = FALSE)
  }

  # check classes
  if (!inherits(imp, "mids"))
    stop("`imp` must be a `mids`-object, as returned by the `mice`-function.", call. = F)

  if (!is.data.frame(dat))
    stop("`dat` must be data frame.", call. = F)

  if (!is.null(ori) && !is.data.frame(ori))
    stop("`ori` must be data frame.", call. = F)


  # create return value
  imputed.dat <- data.frame()
  analyse <- list()


  # iterate all variables of data frame that has missing values
  for (i in seq_len(ncol(dat))) {

    # check if current variable was imputed or not
    if (!sjmisc::is_empty(imp$method[i])) {

      # copy indices of missing values from original variable
      miss_inc <- which(is.na(dat[[i]]))


      # create a new data frame from all imputation steps, where only the
      # imputations of the current variables are in

      miss_inc_dat <- as.data.frame(lapply(seq_len(imp$m), function(x) {
        mice::complete(imp, action = x)[[i]]
      }), stringsAsFactors = F)


      # convert imputed variable to numeric. needed to perform row means.
      miss_inc_dat <- to_value(miss_inc_dat)

      # copy original variable with missings to a new dummy vector
      x <- dat[[i]]


      # now compute the row means for this variable from all imputed variables
      # (which are in the data frame miss_inc_dat). This mean value represents
      # the most imputed value for a missing value. Copy this "final imputed"
      # value into the variable with missings, thus replacing the missings
      # in the original variable with the most likely imputed value. For numeric
      # integer values, this mean is rounded.

      if (is_float(x))
        x[miss_inc] <- rowMeans(miss_inc_dat[miss_inc, ])
      else
        x[miss_inc] <- round(rowMeans(miss_inc_dat[miss_inc, ]))


      # analyse quality of merged values, by saving mean and standard deviation
      # for each merged value to a separate list. the mean and sd refer to
      # all imputed values for a case

      analyse.mw <- apply(miss_inc_dat[miss_inc, ], 1, mean)
      analyse.sd <- apply(miss_inc_dat[miss_inc, ], 1, sd)

      merge_result <- list(
        merged = x[miss_inc],
        mean = analyse.mw,
        sd = analyse.sd,
        grp = rep(colnames(dat[i]), length.out = length(miss_inc))
      )


      # and add to final list
      analyse[[length(analyse) + 1]] <- merge_result
      names(analyse)[length(analyse)] <- colnames(dat[i])


      # append the imputed variable to the original data frame and preserve
      # the non-imputed variable with missing values as well

      if (ncol(imputed.dat) == 0)
        imputed.dat <- data.frame(x)
      else
        imputed.dat <- cbind(imputed.dat, x)


      # give meaningful column-/variable name.

      if (is.null(ori))
        colnames(imputed.dat)[ncol(imputed.dat)] <- sprintf("%s", colnames(dat)[i])
      else
        colnames(imputed.dat)[ncol(imputed.dat)] <- sprintf("%s_imp", colnames(dat)[i])
    }
  }

  # user wants summary of quality-analysis of merged value
  if (summary != "none") {

    # bind data
    if (is.null(ori))
      data <- imputed.dat
    else
      data <- dplyr::bind_cols(ori, imputed.dat)

    # return merged data and summary data
    impret <- list(data = data, summary = analyse, sum.type = summary, filter = filter)
    return(structure(class = "sj_merge.imp", impret))
  }

  if (is.null(ori))
    # return imputed variables
    imputed.dat
  else
    # return data frame with appended imputed variables
    dplyr::bind_cols(ori, imputed.dat)
}
