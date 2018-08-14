#' @title Find start and end index of pattern in string
#' @name str_start
#' @description \code{str_start()} finds the beginning position of \code{pattern}
#'   in each element of \code{x}, while \code{str_end()} finds the stopping position
#'   of \code{pattern} in each element of \code{x}.
#'
#' @param x A character vector.
#' @param pattern Character string to be matched in \code{x}. \code{pattern} might also
#'          be a regular-expression object, as returned by \code{\link[stringr]{regex}},
#'          or any of \pkg{stringr}'s supported \code{\link[stringr]{modifiers}}.
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
#' @importFrom stringr str_locate_all coll
#' @importFrom dplyr pull
#' @importFrom purrr map
#' @export
str_start <- function(x, pattern, ignore.case = TRUE) {
  str_start_end(x, pattern, ignore.case, index = 1)
}


#' @rdname str_start
#' @export
str_end <- function(x, pattern, ignore.case = TRUE) {
  str_start_end(x, pattern, ignore.case, index = -1)
}


str_start_end <- function(x, pattern, ignore.case, index) {
  # get all locations of pattern
  if (inherits(pattern, "regex"))
    pos <- stringr::str_locate_all(x, pattern)
  else
    pos <- stringr::str_locate_all(x, stringr::coll(pattern, ignore_case = ignore.case))

  # return starting indices
  if (length(pos) > 1) {
    purrr::map(pos, function(st) {
      st %>%
        as.data.frame() %>%
        dplyr::pull(index)
    })
  } else {
    pos[[1]] %>%
      as.data.frame() %>%
      dplyr::pull(index)
  }
}
