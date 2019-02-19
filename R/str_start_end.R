#' @title Find start and end index of pattern in string
#' @name str_start
#' @description \code{str_start()} finds the beginning position of \code{pattern}
#'   in each element of \code{x}, while \code{str_end()} finds the stopping position
#'   of \code{pattern} in each element of \code{x}.
#'
#' @param x A character vector.
#' @param pattern Character string to be matched in \code{x}. \code{pattern} might also
#'          be a regular-expression object, as returned by \code{\link[stringr]{regex}}.
#'          Alternatively, use \code{regex = TRUE} to treat \code{pattern} as a regular
#'          expression rather than a fixed string.
#' @param regex Logical, if \code{TRUE}, \code{pattern} is treated as a regular
#'          expression rather than a fixed string.
#'
#' @inheritParams find_var
#'
#' @return A numeric vector with index of start/end position(s) of \code{pattern}
#'          found in \code{x}, or an empty vector, if \code{pattern} was not found
#'          in \code{x}.
#'
#' @examples
#' path <- "this/is/my/fileofinterest.csv"
#' str_start(path, "/")
#'
#' path <- "this//is//my//fileofinterest.csv"
#' str_start(path, "//")
#' str_end(path, "//")
#'
#' x <- c("my_friend_likes me", "your_friend likes_you")
#' str_start(x, "_")
#'
#' # pattern "likes" starts at position 11 in first, and
#' # position 13 in second string
#' str_start(x, "likes")
#'
#' # pattern "likes" ends at position 15 in first, and
#' # position 17 in second string
#' str_end(x, "likes")
#'
#' x <- c("I like to move it, move it", "You like to move it")
#' str_start(x, "move")
#' str_end(x, "move")
#'
#' x <- c("test1234testagain")
#' str_start(x, "\\d+4")
#' str_start(x, "\\d+4", regex = TRUE)
#' str_end(x, "\\d+4", regex = TRUE)
#'
#' @importFrom dplyr pull
#' @importFrom purrr map
#' @export
str_start <- function(x, pattern, ignore.case = TRUE, regex = FALSE) {
  if (regex) class(pattern) <- c("regex", class(pattern))
  str_start_end(x, pattern, ignore.case, index = "start")
}


#' @rdname str_start
#' @export
str_end <- function(x, pattern, ignore.case = TRUE, regex = FALSE) {
  if (regex) class(pattern) <- c("regex", class(pattern))
  str_start_end(x, pattern, ignore.case, index = "end")
}


str_start_end <- function(x, pattern, ignore.case, index) {
  # get all locations of pattern
  pos <- gregexpr(pattern, text = x, fixed = !inherits(pattern, "regex"))

  # add end index if required
  if (index == "end") {
    pos <- lapply(pos, function(i) i <- i + attr(i, "match.length", exact = TRUE) - 1)
  }

  # remove attributes
  l <- lapply(pos, as.vector)

  if (length(l) == 1)
    unlist(l)
  else
    l
}
