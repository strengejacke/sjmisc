#' @export
mean.labelled <- function(x, trim = 0, na.rm = FALSE, missing_to_na = FALSE, ...) {
  # unclass vector for mean-call
  x <- unclass(x)
  if (!missing_to_na) {
    if (!is.null(suppressMessages(get_na(x)))) {
      warning("`x` has self-defined missing values, which are not converted to NA. Use argument `missing_to_na = TRUE` to convert self-defined missings to NA before computing the mean.", call. = F)
    }
  } else {
    x <- to_na(x)
  }
  # mean
  mean(x, trim = trim, na.rm = na.rm)
}

#' @export
is.na.labelled <- function(x) {
  # unclass vector for is.na-call
  x <- unclass(x)
  if (!is.null(suppressMessages(get_na(x)))) {
    warning("`x` has self-defined missing values, which are not counted as NA. Use `to_na` to convert self-defined missing values to NA.", call. = F)
  }
  # return missings
  is.na(x)
}

#' @importFrom nlme getData getCovariateFormula
#' @export
model.matrix.gls <- function(object, ...) {
  mm <- cbind(`(Intercept)` = 1,
              nlme::getData(object)[, all.vars(nlme::getCovariateFormula(object))])
  return(mm)
}

#' @importFrom dplyr tbl_df trunc_mat
#' @export
print.lbl_df <- function(x, ..., n = NULL, width = NULL) {
  # get labels
  dlab <- get_label(x)
  # if x of class tbl_df?
  if (!"tbl_df" %in% class(x))
    x <- dplyr::tbl_df(x)
  # get df matrix
  dmat <- dplyr::trunc_mat(x, n = n, width = width)
  # set labels, if we have any
  if (!is.null(dlab)) {
    # iterate all columns
    for (i in 1:ncol(dmat[[1]])) {
      # replace first value of each column, which is the class description
      # with variable label
      dmat[[1]][[i]][1] <- dlab[i]
    }
  }
  # use dplyr-print method now
  print(dmat, ..., n = n, width = width)
}
