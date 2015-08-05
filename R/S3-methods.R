#' @importFrom stats quantile median na.omit
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
    no_mis <- unclass(stats::na.omit(as.vector(to_na(x))))
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
      f.ind <- replace_na(f.ind, len)
      # frequencies, including real missings
      fdat <- data.frame(index = c(as.numeric(unname(labels)), len),
                         frq = 0,
                         raw = 0,
                         valid = 0)
      fdat$frq[match(f.ind, fdat$index)] <- as.vector(table(x, exclude = NULL))
      # raw percentage, including real missings
      fdat$raw[match(f.ind, fdat$index)] <- as.vector(prop.table(table(x, exclude = NULL)))
      # valid percentage, excluding real and
      # labelled missings
      vp <- as.vector(prop.table(table(stats::na.omit(as.vector(to_na(x))))))
      fdat$valid[match(f.ind[1:length(vp)], fdat$index)] <- as.vector(prop.table(table(stats::na.omit(as.vector(to_na(x))))))
      fdat$valid[length(fdat$valid)] <- NA
      # create df
      lab_df <- data.frame(value = c(unname(labels), NA),
                           label = c(names(labels), "NA"),
                           count = fdat$frq,
                           raw.prc = round(100 * fdat$raw, 2),
                           valid.prc = round(100 * fdat$valid, 2),
                           cum.prc = round(100 * cumsum(fdat$valid), 2),
                           is_na = c(attr(x, "is_na"), NA))
      print(lab_df, row.names = FALSE)
      invisible(lab_df)
    }
  }
}


#' @export
mean.labelled <- function(x, trim = 0, na.rm = FALSE, missing_to_na = FALSE, ...) {
  # unclass vector for mean-call
  x <- unclass(x)
  if (!missing_to_na) {
    if (!is.null(suppressMessages(get_na(x)))) {
      warning("`x` has self-defined missing values, which are not converted to NA. Use argument `missing_to_na = TRUE` to convert self-defined missings to NA before computing the mean.", call. = F)
    }
  } else {
    x <- to_na(x)
  }
  # mean
  mean(x, trim = trim, na.rm = na.rm)
}


#' @export
is.na.labelled <- function(x) {
  # unclass vector for is.na-call
  x <- unclass(x)
  if (!is.null(suppressMessages(get_na(x)))) {
    warning("`x` has self-defined missing values, which not counted as NA. Use `to_na` to convert self-defined missing values to NA.", call. = F)
  }
  # return missings
  is.na(x)
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
