#' @title Summary of labelled vectors
#' @name frq
#' @description This function prints a summary, including frequency table,
#'                of labelled vectors. Unlike \code{\link{summary}}, the
#'                \code{frq} method also prints label and missing attributes.
#'
#' @param x A labelled vector.
#' @param print.frq Optional logical, if \code{TRUE} (default), frequency
#'          table will be printed to the console.
#'
#' @return A data frame with the summary information of \code{x}.
#'
#' @examples
#' # create labelled factor, with missing flag
#' x <- labelled(c("M", "M", "F", "X", "N/A"),
#'               c(Male = "M", Female = "F",
#'                 Refused = "X", "Not applicable" = "N/A"),
#'               c(FALSE, FALSE, TRUE, TRUE))
#' frq(x)
#'
#' # create labelled numeric vector, with missing flag
#' x <- labelled(c(1, 2, 1, 3, 4, 1, NA, 5),
#'               c(Male = 1, Female = 2, Refused = 5),
#'               c(FALSE, FALSE, TRUE))
#' frq(x)
#'
#' @importFrom stats quantile median na.omit
#' @export
frq <- function(x, print.frq = TRUE) {
  # --------------------------
  # check for labelled class
  # --------------------------
  if (!is_labelled(x)) {
    stop("`x` must be of class `labelled`.", call. = F)
  }
  # copy vector
  object <- x
  # add non-labelled value labels, if we have less
  # labels than values
  x <- fill_labels(x)
  # get value labels
  labels <- attr(x, "labels", exact = T)
  # when we have character vectors, simply do table
  if (is.character(object)) {
    # create data frame as return value
    lab_df <- data.frame(value = unname(labels),
                         label = names(labels),
                         is_na = attr(x, "is_na"))
    # check if results should be printed
    if (print.frq) {
      print(table(x))
      cat("\n")
      print(lab_df, row.names = FALSE)
    }
    # return
    invisible(lab_df)
  } else {
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
    # prepare and print summary
    if (print.frq) {
      cat("\nSummary:\n")
      # output
      print(summary_line, row.names = FALSE)
    }
    # do we have any labels? continuous variables
    # usually don't have label attributes after reading
    # from SPSS
    if (!is.null(labels)) {
      if (print.frq) cat("\n")
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
      fdat$valid[match(f.ind[1:length(vp)], fdat$index)] <-
        as.vector(prop.table(table(stats::na.omit(as.vector(to_na(x))))))
      fdat$valid[length(fdat$valid)] <- NA
      # create df
      lab_df <- data.frame(value = c(unname(labels), NA),
                           label = c(names(labels), "NA"),
                           count = fdat$frq,
                           raw.prc = round(100 * fdat$raw, 2),
                           valid.prc = round(100 * fdat$valid, 2),
                           cum.prc = round(100 * cumsum(fdat$valid), 2),
                           is_na = c(attr(x, "is_na"), NA))
      # print table
      if (print.frq) print(lab_df, row.names = FALSE)
      invisible(lab_df)
    }
  }
}



#' @title Get summary of labelled vectors
#' @name get_frq
#' @description This function returns a summary, including frequency table,
#'                of labelled vectors, as data frame. Unlike \code{\link{summary}}, the
#'                \code{frq} method also prints label and missing attributes.
#'
#' @param x A labelled vector.
#' @param coerce Logical, if \code{TRUE}, vectors will be coerced to \code{labelled}
#'          class if necessary.
#'
#' @return A data frame with the summary information of \code{x}.
#'
#' @examples
#' # create labelled factor, with missing flag
#' x <- labelled(c("M", "M", "F", "X", "N/A"),
#'               c(Male = "M", Female = "F",
#'                 Refused = "X", "Not applicable" = "N/A"),
#'               c(FALSE, FALSE, TRUE, TRUE))
#'
#' get_frq(x)
#'
#' @importFrom stats quantile median na.omit
#' @export
get_frq <- function(x, coerce = TRUE) {
  if (!is_labelled(x) && TRUE == coerce)
    x <- as_labelled(x)
  .dat <- frq(x, print.frq = FALSE)
  .dat
}
