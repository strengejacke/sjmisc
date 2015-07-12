#' @importFrom stats quantile median
#' @export
summary.labelled <- function(object, ...) {
  x <- object
  # add non-labelled value labels, if we have less
  # labels than values
  x <- fill_labels(x)
  # get value labels
  labels <- attr(x, "labels", exact = T)
  # when we have character vectors, simply do table
  if (is.character(object)) {
    print(table(x))
    cat("\n")
    lab_df <- data.frame(value = unname(labels),
                         label = names(labels),
                         is_na = attr(x, "is_na"))
    print(lab_df, row.names = FALSE)
    invisible(lab_df)
  } else {
    # prepare summary
    cat("\nSummary:\n")
    # get value without missings
    no_mis <- unclass(na.omit(as.vector(to_na(x))))
    # create named vector with all necessray summary
    # information, equal to base summary function
    summary_line <- data.frame(round(min(no_mis), 3),
                               round(stats::quantile(no_mis)[2], 3),
                               round(stats::median(no_mis), 3),
                               round(mean(no_mis), 3),
                               round(stats::quantile(no_mis)[4], 3),
                               round(max(no_mis), 3))
    # set column names
    colnames(summary_line) <- c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max")
    # output
    print(summary_line, row.names = FALSE)
    # do we have any labels? continuous variables
    # usually don't have label attributes after reading
    # from SPSS
    if (!is.null(labels)) {
      cat("\n")
      # get all possible values as vector. We may have some labelled
      # values that have no counts in the data. in such cases, we get
      # less values from the table than excpected. Here we set up a
      # vector with all values, and "match" the actual values
      len <- length(labels) + 1
      f.ind <- as.numeric(names(table(x, exclude = NULL)))
      f.ind <- replace_na(f.ind, max(f.ind, na.rm = T) + 1)
      # valid length? In some cases, when some values are not
      # present in the data (and not missing), label length
      # differs from maximum value index. in this case,
      # correct values
      if (length(f.ind) > len || max(f.ind) > len) f.ind <- seq_len(len)
      # frequencies, including real missings
      frq <- rep(0, len)
      frq[f.ind] <- as.vector(table(x, exclude = NULL))
      # raw percentage, including real missings
      raw.prc <- rep(0, len)
      raw.prc[f.ind] <- as.vector(prop.table(table(x, exclude = NULL)))
      # valid percentage, excluding real and
      # labelled missings
      valid.prc <- c(rep(0, len - 1), NA)
      vp <- as.vector(prop.table(table(na.omit(as.vector(to_na(x))))))
      valid.prc[f.ind[1:length(vp)]] <- vp
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
  }
}


#' @export
mean.labelled <- function(x, trim = 0, na.rm = TRUE, ...) {
  # unclass vector for mean-call
  x <- unclass(x)
  # mean
  mean(to_na(x), trim = trim, na.rm = na.rm)
}


#' @export
is.na.labelled <- function(x) {
  # unclass vector for mean-call
  x <- unclass(x)
  # return missings
  is.na(to_na(x))
}


# #' @importFrom stats sd
# #' @export
# sd.labelled <- function(x, na.rm = TRUE) {
#   # unclass vector for mean-call
#   x <- unclass(x)
#   # sd
#   stats::sd(to_na(x), na.rm = na.rm)
# }
#
#
# #' @importFrom stats median
# #' @export
# median.labelled <- function(x, na.rm = TRUE) {
#   # unclass vector for mean-call
#   x <- unclass(x)
#   # median
#   stats::median(to_na(x), na.rm = na.rm)
# }
