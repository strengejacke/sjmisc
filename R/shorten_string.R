#' @title Shorten character strings
#' @name shorten_string
#'
#' @description This function shortens strings that are longer than \code{max.length}
#'    chars, without cropping words.
#'
#' @param s A string.
#' @param max.length Maximum length of chars for the string.
#' @param abbr String that will be used as suffix, if \code{s} was shortened.
#'
#' @return A shortened string.
#'
#' @details If the string length defined in \code{max.length} happens to be inside
#'   a word, this word is removed from the returned string (see 'Examples'), so
#'   the returned string has a \emph{maximum length} of \code{max.length}, but
#'   might be shorter.
#'
#' @examples
#' s <- "This can be considered as very long string!"
#'
#' # string is shorter than max.length, so returned as is
#' shorten_string(s, 60)
#'
#' # string is shortened to as many words that result in
#' # a string of maximum 20 chars
#' shorten_string(s, 20)
#'
#' # string including "considered" is exactly of length 22 chars
#' shorten_string(s, 22)
#'
#' @export
shorten_string <- function(s, max.length = NULL, abbr = "...") {
  # check if labels should be truncated
  if (!is.null(max.length)) {
    # create pattern to find words uo to number of max.length chars in vector
    pattern <- paste('(.{1,', max.length, '})(\\s|$)', sep = "")

    # I *hate* regular expressions and will never understand them...
    tmp <-
      paste0(substr(s, 0, unlist(regexec(
        abbr, sub(pattern, replacement = paste0("\\1", abbr), s), fixed = T
      )) - 1), abbr)

    # only replace strings that are longer than max.length
    too.long <- nchar(s) > max.length
    s[too.long] <- tmp[too.long]
  }

  s
}
