#' @export
print.sjmisc.frq <- function(x, ...) {
  purrr::walk(x, function(dat) {
    # get grouping title label
    grp <- attr(dat, "group", exact = T)
    # print title for grouping
    if (!is.null(grp)) cat(sprintf("Grouped by:\n%s\n\n", grp))

    # get variable label
    lab <- attr(dat, "label", exact = T)
    # print label
    if (!is.null(lab)) cat(sprintf("# %s\n\n", lab))

    # print frq-table
    print.data.frame(dat, ..., row.names = FALSE)

    cat("\n\n")
  })
}

#' @export
print.sjmisc.descr <- function(x, ...) {
  cat("## Basic descriptive statistics\n\n")
  # round values
  x[, c(4:6, 8, 12:14)] <- round(x[, c(4:6, 8, 12:14)], 2)
  # print frq-table
  print.data.frame(x, ..., row.names = FALSE)
}

