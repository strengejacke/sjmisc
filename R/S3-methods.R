#' @importFrom purrr walk
#' @export
print.sjmisc.frq <- function(x, ...) {
  purrr::walk(x, function(dat) {
    # get grouping title label
    grp <- attr(dat, "group", exact = T)
    # print title for grouping
    if (!is.null(grp)) cat(sprintf("Grouped by:\n%s\n\n", grp))

    # get variable label
    lab <- attr(dat, "label", exact = T)
    vt <- attr(dat, "vartype", exact = T)

    # print label
    if (!is.null(lab)) cat(sprintf("# %s <%s>\n", lab, vt))

    # add Total N
    cat(sprintf(
      "# Total N = %i (valid N = %i)\n\n",
      sum(dat$frq, na.rm = TRUE),
      sum(dat$frq[1:(nrow(dat) - 1)], na.rm = TRUE)
    ))

    # print frq-table
    print.data.frame(dat, ..., row.names = FALSE, quote = FALSE)

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


#' @importFrom purrr map_df
#' @importFrom dplyr n_distinct filter
#' @importFrom tidyr gather
#' @importFrom rlang .data
#' @export
print.sj_merge.imp <- function(x, ...) {

  # check if ggplot is installed
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package `ggplot2` needed for to plot summaries. Please install it.", call. = FALSE)
  }

  if (x$sum.type == "sd") {
    analyse <- x$summary %>% purrr::map_df(~.x)

    if (!is.null(x$filter))
      analyse <- analyse %>% dplyr::filter(.data$grp %in% x$filter)

    p <- ggplot2::ggplot(
      data = analyse,
      mapping = ggplot2::aes_string(x = "merged", y = "sd")
    ) +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(
        facets = ~grp,
        scales = "free",
        ncol = ceiling(sqrt(dplyr::n_distinct(analyse$grp)))
      ) +
      ggplot2::theme_bw() +
      ggplot2::labs(
        x = NULL,
        y = NULL,
        fill = NULL,
        title = "Standard Deviation of imputed values for each merged value"
      )
  } else {
    analyse <- x$summary %>%
      purrr::map_df(~.x) %>%
      tidyr::gather(key = "value", value = "xpos", 1:2)

    if (!is.null(x$filter))
      analyse <- analyse %>% dplyr::filter(.data$grp %in% x$filter)

    p <- ggplot2::ggplot(
      data = analyse,
      mapping = ggplot2::aes_string(x = "xpos", fill = "value")
    ) +
      ggplot2::facet_wrap(
        facets = ~grp,
        scales = "free",
        ncol = ceiling(sqrt(dplyr::n_distinct(analyse$grp)))
      ) +
      ggplot2::theme_bw() +
      ggplot2::labs(
        x = NULL,
        y = NULL,
        fill = NULL,
        title = "Comparison between mean of imputed values and final merged values"
      )

    # check type of summary diagram
    if (x$sum.type == "dens")
      p <- p + ggplot2::geom_density(alpha = .2)
    else
      p <- p + ggplot2::geom_histogram(position = "dodge")
  }

  graphics::plot(p, ...)
}
