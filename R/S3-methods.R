#' @importFrom purrr walk
#' @importFrom crayon blue cyan italic
#' @importFrom cli cat_line
#' @importFrom dplyr select n_distinct
#' @importFrom rlang .data
#' @export
print.sjmisc.frq <- function(x, ...) {
  cat("\n")
  purrr::walk(x, function(dat) {

    # get grouping title label
    grp <- attr(dat, "group", exact = T)

    # print title for grouping
    if (!is.null(grp))
      cli::cat_line(crayon::cyan(crayon::italic(sprintf("Grouped by:\n%s\n", grp))))

    # get variable label
    lab <- attr(dat, "label", exact = T)
    vt <- attr(dat, "vartype", exact = T)

    # print label
    if (!is.null(lab)) cli::cat_line(crayon::blue(sprintf("# %s <%s>", lab, vt)))

    # add Total N
    cli::cat_line(crayon::blue(sprintf(
      "# Total N = %i (valid N = %i)\n",
      sum(dat$frq, na.rm = TRUE),
      sum(dat$frq[1:(nrow(dat) - 1)], na.rm = TRUE)
    )))

    # don't print labels, if all are "none"
    if (dplyr::n_distinct(dat$label) == 1 && unique(dat$label) == "<none>")
      dat <- dplyr::select(dat, -.data$label)

    # print frq-table
    print.data.frame(dat, ..., row.names = FALSE, quote = FALSE)

    cat("\n\n")
  })
}


#' @importFrom crayon blue
#' @importFrom cli cat_line
#' @export
print.sjmisc_descr <- function(x, ...) {
  cat("\n")
  cli::cat_line(crayon::blue("## Basic descriptive statistics\n"))
  print_descr_helper(x, ...)
}

print_descr_helper <- function(x, ...) {
  digits <- 2

  # do we have digits argument?
  add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)
  if ("digits" %in% names(add.args)) digits <- eval(add.args[["digits"]])

  # round values
  x[, c(5:10, 14:15)] <- round(x[, c(5:10, 14:15)], digits = digits)
  # print frq-table
  print.data.frame(x, ..., row.names = FALSE)
}

#' @importFrom crayon blue cyan italic
#' @importFrom cli cat_line
#' @export
print.sjmisc_grpdescr <- function(x, ...) {
  cat("\n")
  cli::cat_line(crayon::blue("## Basic descriptive statistics"))

  purrr::walk(x, function(.x) {
    # print title for grouping
    cli::cat_line(crayon::cyan(crayon::italic(
      sprintf("\nGrouped by:\n%s", attr(.x, "group", exact = TRUE))
    )))

    print_descr_helper(.x, ...)
  })
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
