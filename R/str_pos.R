#' @title Find partial matching and close distance elements in strings
#' @name str_find
#' @description This function finds the element indices of partial matching or
#'    similar strings in a character vector. Can be used to find exact or
#'    slightly mistyped elements in a string vector.
#'
#' @seealso \code{\link{group_str}}
#'
#' @param string Character vector with string elements.
#' @param pattern String that should be matched against the elements of \code{string}.
#' @param partial Activates similar matching (close distance strings) for parts (substrings)
#'    of the \code{string}. Following values are accepted:
#'    \itemize{
#'      \item 0 for no partial distance matching
#'      \item 1 for one-step matching, which means, only substrings of same length as \code{pattern} are extracted from \code{string} matching
#'      \item 2 for two-step matching, which means, substrings of same length as \code{pattern} as well as strings with a slightly wider range are extracted from \code{string} matching
#'    }
#'    Default value is 0. See 'Details' for more information.
#'
#' @inheritParams group_str
#'
#' @return A numeric vector with index position of elements in \code{string} that
#'    partially match or are similar to \code{pattern}. Returns \code{-1} if no
#'    match was found.
#'
#' @note This function does \emph{not} return the position of a matching string \emph{inside}
#'    another string, but the element's index of the \code{string} vector, where
#'    a (partial) match with \code{pattern} was found. Thus, searching for "abc" in
#'    a string "this is abc" will not return 9 (the start position of the substring),
#'    but 1 (the element index, which is always 1 if \code{string} only has one element).
#'
#' @details \strong{Computation Details}
#'   \cr \cr
#'   Fuzzy string matching is based on regular expressions, in particular
#'   \code{grep(pattern = "(<pattern>){~<precision>}", x = string)}. This
#'   means, \code{precision} indicates the number of chars inside \code{pattern}
#'   that may differ in \code{string} to cosinder it as "matching". The higher
#'   \code{precision} is, the more tolerant is the search (i.e. yielding more
#'   possible matches). Furthermore, the higher the value for \code{partial}
#'   is, the more matches may be found.
#'   \cr \cr
#'   \strong{Partial Distance Matching}
#'   \cr \cr
#'   For \code{partial = 1}, a substring of \code{length(pattern)} is extracted
#'   from \code{string}, starting at position 0 in \code{string} until
#'   the end of \code{string} is reached. Each substring is matched against
#'   \code{pattern}, and results with a maximum distance of \code{precision}
#'   are considered as "matching". If \code{partial = 2}, the range
#'   of the extracted substring is increased by 2, i.e. the extracted substring
#'   is two chars longer and so on.
#'
#' @examples
#' string <- c("Hello", "Helo", "Hole", "Apple", "Ape", "New", "Old", "System", "Systemic")
#' str_find(string, "hel")   # partial match
#' str_find(string, "stem")  # partial match
#' str_find(string, "R")     # no match
#' str_find(string, "saste") # similarity to "System"
#'
#' # finds two indices, because partial matching now
#' # also applies to "Systemic"
#' str_find(string,
#'         "sytsme",
#'         partial = 1)
#'
#' # finds partial matching of similarity
#' str_find("We are Sex Pistols!", "postils")
#' @export
str_find <- function(
  string,
  pattern,
  precision = 2,
  partial = 0,
  verbose = FALSE
) {
  # init return value
  indices <- c()

  # find element indices from partial matching of string and find term
  pos <- as.numeric(grep(pattern, string, ignore.case = TRUE))
  if (length(pos) > 0) indices <- c(indices, pos)

  # find element indices from similar strings
  pos <- which(sapply(tolower(string), function(.x) string_dist(tolower(pattern), .x) <= precision))
  if (length(pos) > 0) indices <- c(indices, pos)

  # find element indices from partial similar (distance)
  # string matching
  if (partial > 0) {
    ftlength <- nchar(pattern)
    # create progress bar
    if (verbose) pb <- utils::txtProgressBar(min = 0,
                                               max = length(string),
                                               style = 3)

    # iterate search string vector
    for (ssl in seq_len(length(string))) {
      # retrieve each element of search string vector
      # we do this step by step instead of vectorizing
      # due to the substring approach
      sst <- string[ssl]

      # we extract substrings of same length as pattern
      # starting from first char of string until end
      # and try to find similar matches
      steps <- nchar(sst) - ftlength + 1
      if (steps > 0) {
        for (pi in seq_len(steps)) {
          # retrieve substring
          sust <- trim(substr(sst, pi, pi + ftlength - 1))

          # find element indices from similar substrings
          pos <- which(string_dist(tolower(pattern), tolower(sust)) <= precision)
          if (length(pos) > 0) indices <- c(indices, ssl)
        }
      }
      if (partial > 1) {

        # 2nd loop picks longer substrings, because similarity
        # may also be present if length of strings differ
        # (e.g. "app" and "apple")
        steps <- nchar(sst) - ftlength
        if (steps > 1) {
          for (pi in 2:steps) {
            # retrieve substring
            sust <- trim(substr(sst, pi - 1, pi + ftlength))

            # find element indices from similar substrings
            pos <- which(string_dist(tolower(pattern), tolower(sust)) <= precision)
            if (length(pos) > 0) indices <- c(indices, ssl)
          }
        }
      }
      # update progress bar
      if (verbose) utils::setTxtProgressBar(pb, ssl)
    }
  }
  if (verbose) close(pb)

  # return result
  if (length(indices) > 0) return(sort(unique(indices)))

  return(-1)
}
