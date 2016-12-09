#' @importFrom tibble trunc_mat as_tibble
#' @export
print.lbl_df <- function(x, ..., n = NULL, width = NULL) {
  # get labels
  dlab <- get_label(x)
  # if x of class tbl_df?
  if (!"tbl_df" %in% class(x))
    x <- tibble::as_tibble(x)
  # get df matrix
  dmat <- tibble::trunc_mat(x, n = n, width = width)
  # set labels, if we have any
  if (!is.null(dlab)) {
    # iterate all columns
    for (i in seq_len(ncol(dmat[[1]]))) {
      # replace first value of each column, which is the class description
      # with variable label
      dmat[[1]][[i]][1] <- dlab[i]
    }
  }
  # use dplyr-print method now
  print(dmat, ..., n = n, width = width)
}

#' @export
print.sjmisc.frq <- function(x, ...) {
  # get variable label
  lab <- attr(x, "label", exact = T)
  # print label
  if (!is.null(lab)) cat(sprintf("# %s\n\n", lab))
  # print frq-table
  print.data.frame(x, ..., row.names = FALSE)
}
