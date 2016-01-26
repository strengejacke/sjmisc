#' @title Insert line breaks in long labels
#' @name word_wrap
#'
#' @description Insert line breaks in long character strings. Useful if you want to wordwrap
#'                labels / titles for plots or tables.
#'
#' @param labels Label(s) as character string, where a line break should be
#'          inserted. Several strings may be passed as vector  (see 'Examples').
#' @param wrap Maximum amount of chars per line (i.e. line length). If code{wrap = Inf},
#'          no word wrap will be performed (i.e. \code{labels} will be returned as is).
#' @param linesep By default, this argument is \code{NULL} and a regular new line
#'          string (\code{"\\n"}) is used. For HTML-purposes, for instance, \code{linesep}
#'          could be \code{"<br>"}.
#' @return New label(s) with line breaks inserted at every \code{wrap}'s position.
#'
#' @examples
#' word_wrap(c("A very long string", "And another even longer string!"), 10)
#'
#' message(word_wrap("Much too long string for just one line!", 15))
#'
#' @importFrom stats na.omit
#' @export
word_wrap <- function(labels, wrap, linesep = NULL) {
  # check if labels have NA values and remove them
  if (anyNA(labels)) labels <- as.character(stats::na.omit(labels))
  # check for valid value
  if (is.null(labels) || length(labels) == 0) return(NULL)
  # infinite wrap? then return labels
  if (is.infinite(wrap)) return(labels)
  # default line separator is \n
  if (is.null(linesep)) {
    linesep <- '\\1\n'
    lsub <- 0
    ori.linesep <- '\n'
  } else {
    # however, for html-function we can use "<br>"
    # as argument
    lsub <- nchar(linesep) - 1
    ori.linesep <- linesep
    linesep <- sprintf("\\1%s", linesep)
  }
  # create regex pattern for line break
  pattern <- c(paste('(.{1,', wrap, '})(\\s|$)', sep = ""))
  # iterate all labels
  for (n in 1:length(labels)) {
    # check if wrap exceeds lengths of labels
    if (wrap > 0 && nchar(labels[n]) > wrap) {
      # insert line breaks
      labels[n] <- gsub(pattern, linesep, labels[n])
      # -----------------------
      # in case label was short enough, we still have a line break
      # at the end of the label. here we remove any trailing line breaks
      # -----------------------
      # get length of label
      l <- nchar(labels[n])
      # get last char
      lc <- substr(labels[n], l - lsub, l)
      # check if line break
      if (lc == ori.linesep) {
        # if yes, remove it
        labels[n] <- substr(labels[n], 0, l - (lsub + 1))
      }
    }
  }
  return(labels)
}
