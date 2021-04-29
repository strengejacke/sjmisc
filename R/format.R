#' @export
format.sjmisc_frq <- function(x, ...) {
  lapply(x, .format_frq)
}


.format_frq <- function(x) {
  # get variable label
  lab <- attr(x, "label", exact = TRUE)
  vt <- attr(x, "vartype", exact = TRUE)

  # fix variable type string
  if (!sjmisc::is_empty(vt))
    vt <- sprintf(" (%s)", vt)
  else
    vt <- ""

  table_title <- NULL
  table_subtitle <- NULL
  table_footer <- NULL

  # print label
  if (!is.null(lab)) {
    table_title <- sprintf("%s%s", lab, vt)
  }

  # get grouping title label
  grp <- attr(x, "group", exact = TRUE)

  # print title for grouping
  if (!is.null(grp)) {
    table_footer <- paste0("grouped by: ", sprintf("%s", grp))
  }

  # add Total N
  table_subtitle <- sprintf(
    "total N=%i  valid N=%i  mean=%.2f  sd=%.2f",
    sum(x$frq, na.rm = TRUE),
    sum(x$frq[0:(nrow(x) - 1)], na.rm = TRUE),
    attr(x, "mean", exact = TRUE),
    attr(x, "sd", exact = TRUE)
  )

  # don't print labels, if all except for the NA value are "none"
  if ((dplyr::n_distinct(x$label[!is.na(x$val)]) == 1 && unique(x$label[!is.na(x$val)]) == "<none>") || (length(x$val) == 1 && is.na(x$val)))
    x <- dplyr::select(x, -.data$label)


  # fix colnames
  colnames(x)[names(x) == "val"] <- "Value"
  colnames(x)[names(x) == "label"] <- "Label"
  colnames(x)[names(x) == "frq"] <- "N"
  colnames(x)[names(x) == "raw.prc"] <- "Raw %"
  colnames(x)[names(x) == "valid.prc"] <- "Valid %"
  colnames(x)[names(x) == "cum.prc"] <- "Cum. %"

  attr(x, "table_title") <- table_title
  attr(x, "table_subtitle") <- table_subtitle
  attr(x, "table_footer") <- table_footer

  x
}
