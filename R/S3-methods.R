#' @export
mean.labelled <- function(x, trim = 0, na.rm = FALSE, missing_to_na = FALSE, ...) {
  # unclass vector for mean-call
  x <- unclass(x)
  # has any missing attributes?
  has_na <- !is.null(suppressMessages(get_na(x)))
  # warn user
  if (!missing_to_na) {
    if (has_na) {
      message("`x` has valus with missing attribute, which are not converted to NA. Use argument `missing_to_na = TRUE` to convert labelled missings to NA before computing the mean.", call. = F)
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


#'@export
print.labelled <- function(x, ...) {
  # code partially taken from haven:::print.labelled
  cat("<Labelled>\n")
  xx <- unclass(x)
  attr(xx, "label") <- NULL
  attr(xx, "labels") <- NULL
  attr(xx, "is_na") <- NULL
  attr(xx, "note") <- NULL
  # print values
  print(xx)
  # print variable label
  cat("\nVariable label:\n")
  cat("  ", attr(x, "label", exact = TRUE), "\n")
  # print value  labels
  cat("\nValue labels:\n")
  print(attr(x, "labels", exact = TRUE))
  # print missing
  cat("\nMissing values:\n")
  cat("  ", format(get_na(x)), "\n")
  # do we have a note?
  note <- attr(x, "note", exact = TRUE)
  if (!is.null(note) && !is_empty(note)) {
    cat("\nNote:\n")
    cat("  ", note, "\n")
  }
  invisible()
}