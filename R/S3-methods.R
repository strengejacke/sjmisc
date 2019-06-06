#' @importFrom insight print_color
#' @importFrom purrr walk
#' @importFrom dplyr select n_distinct
#' @importFrom rlang .data
#' @export
print.sjmisc_frq <- function(x, ...) {
  purrr::walk(x, function(dat) {

    # get variable label
    lab <- attr(dat, "label", exact = T)
    vt <- attr(dat, "vartype", exact = T)

    # fix variable type string
    if (!sjmisc::is_empty(vt))
      vt <- sprintf(" <%s>", vt)
    else
      vt <- ""

    cat("\n")

    # print label
    if (!is.null(lab)) {
      insight::print_color(sprintf("%s", lab), "red")
      insight::print_color(sprintf("%s\n", vt), "blue")
    }

    # get grouping title label
    grp <- attr(dat, "group", exact = T)

    # print title for grouping
    if (!is.null(grp)) {
      insight::print_color("# grouped by: ", "blue")
      insight::print_color(sprintf("%s\n", grp), "cyan")
    }

    # add Total N
    insight::print_color(sprintf(
      "# total N=%i  valid N=%i  mean=%.2f  sd=%.2f\n\n",
      sum(dat$frq, na.rm = TRUE),
      sum(dat$frq[1:(nrow(dat) - 1)], na.rm = TRUE),
      attr(dat, "mean", exact = T),
      attr(dat, "sd", exact = T)
    ), "blue")

    # don't print labels, if all are "none"
    if (dplyr::n_distinct(dat$label) == 1 && unique(dat$label) == "<none>")
      dat <- dplyr::select(dat, -.data$label)

    # print frq-table
    print.data.frame(dat, ..., row.names = FALSE, quote = FALSE)

    cat("\n")
  })
}


#' @export
print.sjmisc_descr <- function(x, ...) {
  cat("\n")
  insight::print_color("## Basic descriptive statistics\n\n", "blue")
  print_descr_helper(x, ...)
}

print_descr_helper <- function(x, ...) {
  digits <- 2

  # do we have digits argument?
  add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)
  if ("digits" %in% names(add.args)) digits <- eval(add.args[["digits"]])

  # round values
  if (is.null(attr(x, "weights", exact = TRUE)))
    x[, c(5:10, 12)] <- round(x[, c(5:10, 12)], digits = digits)
  else
    x[, c(5:8)] <- round(x[, c(5:8)], digits = digits)
  # print frq-table
  print.data.frame(x, ..., row.names = FALSE)
}

#' @export
print.sjmisc_grpdescr <- function(x, ...) {
  cat("\n")
  insight::print_color("## Basic descriptive statistics\n", "blue")

  purrr::walk(x, function(.x) {
    # print title for grouping
    insight::print_color("\n\nGrouped by: ", "red")
    insight::print_color(sprintf("%s\n\n", attr(.x, "group", exact = TRUE)), "cyan")

    print_descr_helper(.x, ...)
  })
}

#' @importFrom purrr map_df
#' @importFrom dplyr n_distinct filter
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
    analyse <- purrr::map_df(x$summary, ~.x)
    analyse <- .gather(analyse, key = "value", value = "xpos", colnames(analyse)[1:2])

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


#' @export
print.sj_has_na <- function(x, ...) {
  insight::print_color("## Variables with missing or infinite values (in red)\n\n", "blue")

  s1 <- max(c(nchar(x$name), nchar("Name")))
  s2 <- max(c(nchar(x$label), nchar("Variable Label")))

  cat(sprintf("   Column   %*s   %*s\n\n", s1, "Name", s2, "Variable Label"))

  for (i in 1:nrow(x)) {
    row <- sprintf("   %*i   %*s   %*s\n", 6, x[i, "col"], s1, x[i, "name"], s2, x[i, "label"])
    if (.is_true(x[i, "has.na"]))
      insight::print_color(row, "red")
    else
      insight::print_color(row, "green")
  }

  cat("\n")
}
