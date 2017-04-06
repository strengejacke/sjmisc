#' @title Find partial matching and close distance elements in strings
#' @name str_pos
#' @description This function finds the element indices of partial matching or similar strings
#'                in a character vector. Can be used to find exact or slightly mistyped elements
#'                in a string vector.
#'
#' @seealso \code{\link{group_str}}
#'
#' @param search.string Character vector with string elements.
#' @param find.term String that should be matched against the elements of \code{search.string}.
#' @param maxdist Maximum distance between two string elements, which is allowed to treat them
#'          as similar or equal. Smaller values mean less tolerance in matching.
#' @param part.dist.match Activates similar matching (close distance strings) for parts (substrings)
#'          of the \code{search.string}. Following values are accepted:
#'          \itemize{
#'            \item 0 for no partial distance matching
#'            \item 1 for one-step matching, which means, only substrings of same length as \code{find.term} are extracted from \code{search.string} matching
#'            \item 2 for two-step matching, which means, substrings of same length as \code{find.term} as well as strings with a slightly wider range are extracted from \code{search.string} matching
#'          }
#'          Default value is 0. See 'Details' for more information.
#' @param show.pbar Logical; f \code{TRUE}, the progress bar is displayed when computing the distance matrix.
#'          Default in \code{FALSE}, hence the bar is hidden.
#'
#' @return A numeric vector with index position of elements in \code{search.string} that
#'           partially match or are similar to \code{find.term}. Returns \code{-1} if no
#'           match was found.
#'
#' @note This function does \emph{not} return the position of a matching string \emph{inside}
#'         another string, but the element's index of the \code{search.string} vector, where
#'         a (partial) match with \code{find.term} was found. Thus, searching for "abc" in
#'         a string "this is abc" will not return 9 (the start position of the substring),
#'         but 1 (the element index, which is always 1 if \code{search.string} only has one element).
#'
#' @details For \code{part.dist.match = 1}, a substring of \code{length(find.term)} is extracted
#'            from \code{search.string}, starting at position 0 in \code{search.string} until
#'            the end of \code{search.string} is reached. Each substring is matched against
#'            \code{find.term}, and results with a maximum distance of \code{maxdist}
#'            are considered as "matching". If \code{part.dist.match = 2}, the range
#'            of the extracted substring is increased by 2, i.e. the extracted substring
#'            is two chars longer and so on.
#'
#' @examples
#' \dontrun{
#' string <- c("Hello", "Helo", "Hole", "Apple", "Ape", "New", "Old", "System", "Systemic")
#' str_pos(string, "hel")   # partial match
#' str_pos(string, "stem")  # partial match
#' str_pos(string, "R")     # no match
#' str_pos(string, "saste") # similarity to "System"
#'
#' # finds two indices, because partial matching now
#' # also applies to "Systemic"
#' str_pos(string,
#'         "sytsme",
#'         part.dist.match = 1)
#'
#' # finds nothing
#' str_pos("We are Sex Pistols!", "postils")
#' # finds partial matching of similarity
#' str_pos("We are Sex Pistols!", "postils", part.dist.match = 1)}
#'
#' @importFrom stringdist stringdist
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export
str_pos <- function(search.string,
                    find.term,
                    maxdist = 2,
                    part.dist.match = 0,
                    show.pbar = FALSE) {
  # init return value
  indices <- c()

  # find element indices from partial matching of string and find term
  pos <- as.numeric(grep(find.term, search.string, ignore.case = T))
  if (length(pos) > 0) indices <- c(indices, pos)

  # find element indices from similar strings
  pos <- which(stringdist::stringdist(tolower(find.term), tolower(search.string)) <= maxdist)
  if (length(pos) > 0) indices <- c(indices, pos)

  # find element indices from partial similar (distance)
  # string matching
  if (part.dist.match > 0) {
    ftlength <- nchar(find.term)
    # create progress bar
    if (show.pbar) pb <- utils::txtProgressBar(min = 0,
                                               max = length(search.string),
                                               style = 3)

    # iterate search string vector
    for (ssl in seq_len(length(search.string))) {
      # retrieve each element of search string vector
      # we do this step by step instead of vectorizing
      # due to the substring approach
      sst <- search.string[ssl]

      # we extract substrings of same length as find.term
      # starting from first char of search.string until end
      # and try to find similar matches
      steps <- nchar(sst) - ftlength + 1
      if (steps > 0) {
        for (pi in seq_len(steps)) {
          # retrieve substring
          sust <- trim(substr(sst, pi, pi + ftlength - 1))

          # find element indices from similar substrings
          pos <- which(stringdist::stringdist(tolower(find.term), tolower(sust)) <= maxdist)
          if (length(pos) > 0) indices <- c(indices, ssl)
        }
      }
      if (part.dist.match > 1) {

        # 2nd loop picks longer substrings, because similarity
        # may also be present if length of strings differ
        # (e.g. "app" and "apple")
        steps <- nchar(sst) - ftlength
        if (steps > 1) {
          for (pi in 2:steps) {
            # retrieve substring
            sust <- trim(substr(sst, pi - 1, pi + ftlength))

            # find element indices from similar substrings
            pos <- which(stringdist::stringdist(tolower(find.term), tolower(sust)) <= maxdist)
            if (length(pos) > 0) indices <- c(indices, ssl)
          }
        }
      }
      # update progress bar
      if (show.pbar) utils::setTxtProgressBar(pb, ssl)
    }
  }
  if (show.pbar) close(pb)

  # return result
  if (length(indices) > 0) return(sort(unique(indices)))
  return(-1)
}
