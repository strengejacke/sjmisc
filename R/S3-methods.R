#' @export
summary.labelled <- function(x, ...) {
  # add non-labelled value labels
  x <- fill_labels(x)
  # prepare summary
  cat("\nSummary:\n")
  labels <- attr(x, "labels")
  # frequencies, including real missings
  frq <- as.vector(table(x, exclude = NULL))
  # raw percentage, including real missings
  raw.prc <- as.vector(prop.table(table(x, exclude = NULL)))
  # valid percentage, excluding real and
  # labelled missings
  valid.prc <- rep(NA, length(raw.prc))
  vp <- as.vector(prop.table(table(na.omit(as.vector(to_na(x))))))
  valid.prc[1:length(vp)] <- vp
  # create df
  lab_df <- data.frame(value = c(unname(labels), NA),
                       label = c(names(labels), "NA"),
                       count = frq,
                       raw.prc = round(100 * raw.prc, 2),
                       valid.prc = round(100 * valid.prc, 2),
                       cum.prc = round(100 * cumsum(valid.prc), 2),
                       is_na = c(attr(x, "is_na"), NA))

  print(lab_df, row.names = FALSE)

  invisible(lab_df)
}
