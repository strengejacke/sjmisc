#' @export
print.sjmisc.frq <- function(x, ...) {
  # get variable label
  lab <- attr(x, "label", exact = T)
  # print label
  if (!is.null(lab)) cat(sprintf("# %s\n\n", lab))
  # print frq-table
  print.data.frame(x, ..., row.names = FALSE)
}

#' @export
print.sjmisc.descr <- function(x, ...) {
  cat("## Basic descriptive statistics\n\n")
  # round values
  x[, c(4:6, 8, 12:14)] <- round(x[, c(4:6, 8, 12:14)], 2)
  # print frq-table
  print.data.frame(x, ..., row.names = FALSE)
}

