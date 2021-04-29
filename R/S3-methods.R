#' @importFrom insight print_color export_table
#' @importFrom purrr walk
#' @importFrom dplyr select n_distinct
#' @importFrom rlang .data
#' @export
print.sjmisc_frq <- function(x, ...) {
  purrr::walk(x, function(dat) {

    # get variable label
    lab <- attr(dat, "label", exact = TRUE)
    vt <- attr(dat, "vartype", exact = TRUE)

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
    grp <- attr(dat, "group", exact = TRUE)

    # print title for grouping
    if (!is.null(grp)) {
      insight::print_color("# grouped by: ", "blue")
      insight::print_color(sprintf("%s\n", grp), "cyan")
    }

    # add Total N
    insight::print_color(sprintf(
      "# total N=%i  valid N=%i  mean=%.2f  sd=%.2f\n\n",
      sum(dat$frq, na.rm = TRUE),
      sum(dat$frq[0:(nrow(dat) - 1)], na.rm = TRUE),
      attr(dat, "mean", exact = TRUE),
      attr(dat, "sd", exact = TRUE)
    ), "blue")

    # don't print labels, if all except for the NA value are "none"
    if ((dplyr::n_distinct(dat$label[!is.na(dat$val)]) == 1 && unique(dat$label[!is.na(dat$val)]) == "<none>") || (length(dat$val) == 1 && is.na(dat$val)))
      dat <- dplyr::select(dat, -.data$label)


    # fix colnames
    colnames(dat)[names(dat) == "val"] <- "Value"
    colnames(dat)[names(dat) == "label"] <- "Label"
    colnames(dat)[names(dat) == "frq"] <- "N"
    colnames(dat)[names(dat) == "raw.prc"] <- "Raw %"
    colnames(dat)[names(dat) == "valid.prc"] <- "Valid %"
    colnames(dat)[names(dat) == "cum.prc"] <- "Cum. %"

    # print frq-table
    cat(insight::export_table(dat, missing = "<NA>"))

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
  to.round <- c("NA.prc", "mean", "sd", "se", "md", "trimmed")
  if (is.null(attr(x, "weights", exact = TRUE))) to.round <- c(to.round, "skew")
  to.round <- intersect(to.round, colnames(x))
  x[, to.round] <- round(x[, to.round], digits = digits)

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

#' @export
print.sj_merge.imp <- function(x, ...) {
  graphics::plot(x$plot, ...)
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
