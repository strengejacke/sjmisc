#' @title Frequencies of labelled variables
#' @name frq
#'
#' @description This function returns a frequency table of labelled vectors, as data frame.
#'
#' @param x A labelled vector or a \code{data.frame} with labelled vectors.
#' @param sort.frq Determines whether categories should be sorted
#'          according to their frequencies or not. Default is \code{"none"}, so
#'          categories are not sorted by frequency. Use \code{"asc"} or
#'          \code{"desc"} for sorting categories ascending or descending order.
#' @param weight.by Vector of weights that will be applied to weight all observations.
#'          Must be a vector of same length as the input vector. Default is
#'          \code{NULL}, so no weights are used.
#'
#' @return A data frame with values, value labels, frequencies, raw, valid and
#'           cumulative percentages of \code{x}.
#'
#' @seealso \code{\link{flat_table}} for labelled (proportional) tables.
#'
#' @examples
#' library(haven)
#' # create labelled integer
#' x <- labelled(c(1, 2, 1, 3, 4, 1),
#'               c(Male = 1, Female = 2, Refused = 3, "N/A" = 4))
#' frq(x)
#'
#' x <- labelled(c(1:3, tagged_na("a", "c", "z"), 4:1, 2:3),
#'               c("Agreement" = 1, "Disagreement" = 4, "First" = tagged_na("c"),
#'                 "Refused" = tagged_na("a"), "Not home" = tagged_na("z")))
#' frq(x)
#'
#' # in a pipe
#' data(efc)
#' library(dplyr)
#' efc %>% select(e42dep, e15relat, c172code) %>% frq()
#'
#' @importFrom stats na.omit
#' @importFrom dplyr full_join
#' @export
#' @export
frq <- function(x, sort.frq = c("none", "asc", "desc"), weight.by = NULL) {
  UseMethod("frq")
}

#' @export
frq.data.frame <- function(x, sort.frq = c("none", "asc", "desc"), weight.by = NULL) {
  sort.frq <- match.arg(sort.frq)
  lapply(x, FUN = frq_helper, sort.frq = sort.frq, weight.by = weight.by)
}

#' @export
frq.list <- function(x, sort.frq = c("none", "asc", "desc"), weight.by = NULL) {
  sort.frq <- match.arg(sort.frq)
  lapply(x, FUN = frq_helper, sort.frq = sort.frq, weight.by = weight.by)
}

#' @export
frq.default <- function(x, sort.frq = c("none", "asc", "desc"), weight.by = NULL) {
  sort.frq <- match.arg(sort.frq)
  frq_helper(x = x, sort.frq = sort.frq, weight.by = weight.by)
}


frq_helper <- function(x, sort.frq, weight.by) {
  #---------------------------------------------------
  # variable with only mising?
  #---------------------------------------------------
  if (length(stats::na.omit(x)) == 0) {
    mydat <- data.frame(val = NA,
                        label = NA,
                        frq = NA,
                        raw.prc = NA,
                        valid.prc = NA,
                        cum.perc = NA)
    return(structure(class = "sjmisc.frq", list(mydat = mydat)))
  }
  #---------------------------------------------------
  # get value labels (if any)
  #---------------------------------------------------
  labels <- get_labels(x, attr.only = T, include.values = "n", include.non.labelled = T)
  #---------------------------------------------------
  # get variable label (if any)
  #---------------------------------------------------
  varlab <- get_label(x)
  #---------------------------------------------------
  # do we have a labelled vector?
  #---------------------------------------------------
  if (!is.null(labels)) {
    # add rownames and values as columns
    dat <- data.frame(n = names(labels), v = as.character(labels), stringsAsFactors = FALSE)
    colnames(dat) <- c("val", "label")
    # character vectors need to be converted with to_value
    # to avoid NAs, but only if character is non-numeric
    if (is.character(dat$val) && anyNA(suppressWarnings(as.numeric(dat$val))))
      dat$val <- to_value(dat$val, keep.labels = F)
    else
      dat$val <- as.numeric(dat$val)
    # weight data?
    if (!is.null(weight.by)) {
      dat2 <- data.frame(
        round(stats::xtabs(weights ~ x,
                           data = data.frame(weights = weight.by, x = x),
                           na.action = stats::na.pass,
                           exclude = NULL), 0)
        )
    } else {
      # create frequency table
      dat2 <- data.frame(table(x, useNA = "always"))
    }
    colnames(dat2) <- c("val", "frq")
    dat2$val <- to_value(dat2$val, keep.labels = F)
    # join frq table and label columns
    mydat <- suppressMessages(dplyr::full_join(dat, dat2))
    # replace NA with 0, for proper percentages, i.e.
    # missing values don't appear (zero counts)
    suppressMessages(replace_na(mydat$frq) <- 0)
  } else {
    # weight data?
    if (!is.null(weight.by)) {
      mydat <- data.frame(
        round(stats::xtabs(weights ~ x,
                           data = data.frame(weights = weight.by, x = x),
                           na.action = stats::na.pass,
                           exclude = NULL), 0)
      )
    } else {
      # if we have no labels, do simple frq table
      mydat <- data.frame(table(x, useNA = "always"))
    }
    colnames(mydat) <- c("val", "frq")
    # add values as label
    mydat$label <- labels <- as.character(mydat$val)
  }
  #---------------------------------------------------
  # need numeric
  #---------------------------------------------------
  if (is.factor(x) || is.character(x)) {
    x <- to_value(x, keep.labels = F)
  }
  # valid values are one row less, because last row is NA row
  valid.vals <- nrow(mydat) - 1
  # --------------------------------------------------------
  # sort categories ascending or descending
  # --------------------------------------------------------
  if (!is.null(sort.frq) && (sort.frq == "asc" || sort.frq == "desc")) {
    ord <- order(mydat$frq[seq_len(valid.vals)], decreasing = (sort.frq == "desc"))
    mydat <- mydat[c(ord, valid.vals + 1), ]
    labels <- labels[ord]
  }
  # raw percentages
  mydat$raw.prc <- mydat$frq / sum(mydat$frq)
  # compute valud and cumulative percentages
  mydat$valid.prc <- c(mydat$frq[1:valid.vals] / length(stats::na.omit(x)), NA)
  mydat$cum.prc <- c(cumsum(mydat$valid.prc[1:valid.vals]), NA)
  # proper rounding
  mydat$raw.prc <- 100 * round(mydat$raw.prc, 4)
  mydat$cum.prc <- 100 * round(mydat$cum.prc, 4)
  mydat$valid.prc <- 100 * round(mydat$valid.prc, 4)
  # -------------------------------------
  # "rename" NA values
  # -------------------------------------
  if (!is.null(mydat$label)) mydat$label[is.na(mydat$label)] <- "NA"
  suppressMessages(replace_na(mydat$val) <- max(to_value(mydat$val), na.rm = T) + 1)
  # save original order
  reihe <- to_value(mydat$val, keep.labels = F)
  # sort for x-axis
  mydat$val <- sort(reihe)
  # add variable label as attribute, for print-method
  attr(mydat, "label") <- varlab
  # -------------------------------------
  # return results
  # -------------------------------------
  return(structure(class = "sjmisc.frq", list(mydat = mydat)))
}
