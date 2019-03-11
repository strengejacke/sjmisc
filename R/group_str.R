#' @title Group near elements of string vectors
#' @name group_str
#'
#' @seealso \code{\link{str_find}}
#'
#' @description This function groups elements of a string vector (character or string
#'                variable) according to the element's distance ('similatiry'). The
#'                more similar two string elements are, the higher is the
#'                chance to be combined into a group.
#'
#' @param strings Character vector with string elements.
#' @param precision Maximum distance ("precision") between two string elements,
#'    which is allowed to treat them as similar or equal. Smaller values mean
#'    less tolerance in matching.
#' @param strict Logical; if \code{TRUE}, value matching is more strictly. See 'Examples'.
#' @param trim.whitespace Logical; if \code{TRUE} (default), leading and trailing white spaces will
#'          be removed from string values.
#' @param remove.empty Logical; if \code{TRUE} (default), empty string values will be removed from the
#'          character vector \code{strings}.
#' @param verbose Logical; if \code{TRUE}, the progress bar is displayed when computing the distance matrix.
#'          Default in \code{FALSE}, hence the bar is hidden.
#' @param maxdist Deprecated. Please use \code{precision} now.
#'
#' @return A character vector where similar string elements (values) are recoded
#'           into a new, single value. The return value is of same length as
#'           \code{strings}, i.e. grouped elements appear multiple times, so
#'           the count for each grouped string is still avaiable (see 'Examples').
#'
#' @examples
#' oldstring <- c("Hello", "Helo", "Hole", "Apple",
#'                "Ape", "New", "Old", "System", "Systemic")
#' newstring <- group_str(oldstring)
#'
#' # see result
#' newstring
#'
#' # count for each groups
#' table(newstring)
#'
#' # print table to compare original and grouped string
#' frq(oldstring)
#' frq(newstring)
#'
#' # larger groups
#' newstring <- group_str(oldstring, precision = 3)
#' frq(oldstring)
#' frq(newstring)
#'
#' # be more strict with matching pairs
#' newstring <- group_str(oldstring, precision = 3, strict = TRUE)
#' frq(oldstring)
#' frq(newstring)
#'
#' @importFrom utils txtProgressBar
#' @export
group_str <- function(
  strings,
  precision = 2,
  strict = FALSE,
  trim.whitespace = TRUE,
  remove.empty = TRUE,
  verbose = FALSE,
  maxdist
) {
  # coerce to character, if necessary
  if (!is.character(strings)) strings <- as.character(strings)

  if (!missing(maxdist)) precision <- maxdist

  # trim white spaces
  if (trim.whitespace) strings <- unname(sapply(strings, trim))

  # remove empty values
  if (remove.empty) {
    removers <- which(sjmisc::is_empty(strings, first.only = F))
    if (length(removers) > 0) strings <- strings[-removers]
  }

  # create matrix from string values of variable
  m <- string_dist_matrix(strings)

  # init variable that contains "close" pairs
  pairs <- list()

  # create progress bar
  if (verbose) pb <- utils::txtProgressBar(min = 0, max = ncol(m), style = 3)

  # iterate matrix
  for (i in seq_len(nrow(m))) {
    # update progress bar
    if (verbose) utils::setTxtProgressBar(pb, i)

    # check if current element is already grouped
    if (!findInPairs(rownames(m)[i], pairs)) {
      # current row element has not been grouped
      # yet, so go on...
      pairvector <- c()

      for (j in seq_len(ncol(m))) {
        # check if we found a pair's distance that
        # is within the maximum requested distance
        # i.e. which are "close" enough
        if (!is.na(m[i, j]) && m[i, j] <= precision) {
          # go through all rows of this column and
          # check if there's a better match for the
          # currently compared token
          foundBetterToken <- !strict
          for (cnt in seq_len(nrow(m))) {
            if (!is.na(m[cnt, j]) && !is.na(m[i, cnt])) {
              if (strict) {
                if (m[cnt, j] > 0 && m[cnt, j] < m[i, j]) {
                  foundBetterToken <- TRUE
                  break
                }
              } else {
                if (m[cnt, j] <= precision && m[i, cnt] <= precision) {
                  foundBetterToken <- FALSE
                  break
                }
              }
            }
          }

          # in the current column, there's no better
          # matching of strings, so we pick this values
          # and add it to our results
          if (!foundBetterToken) {
            # remember string value
            token <- colnames(m)[j]

            # check if we already found a string value
            # within this column. if not, add string values
            # to "close" pairs of this column
            if (!any(pairvector == token) && !findInPairs(token, pairs)) pairvector <- c(pairvector, token)
          }
        }
      }

      # now we have a vector with all "close" string values
      # from the current row's value
      pairvector <- sort(pairvector)

      # check if we already have saved these values to our list
      # if not, add "close" values as new list element
      if (!any(unlist(lapply(pairs, function(x) length(x) == length(pairvector) && any(x == pairvector))))) pairs <- c(pairs, list(pairvector))
    }
  }

  # we now have a list, where each list element
  # is a vector of "close" string values
  strings.new <- rep(NA, length(strings))

  # go through each list element
  for (i in seq_len(length(pairs))) {
    r <- pairs[[i]]
    # find vector indices of "close" values in
    # original string
    indices <- unlist(lapply(r, function(x) which(strings == x)))
    strings.new[indices] <- paste0(pairs[[i]], collapse = ", ")
  }

  if (verbose) close(pb)

  # return new vector, where all single "close"
  # values are replaced by the group of closed values.
  # e.g. the three values "hello", "holle" and "hole"
  # will be "recoded" into on value "hello, holle, hole"
  strings.new
}


# helper function that finds elements in
# final list of grouped elements
findInPairs <- function(curel, pairs) {
  elfound <- FALSE
  if (length(pairs) > 0) {
    for (ll in seq_len(length(pairs))) {
      pel <- pairs[[ll]]
      if (!is.na(curel) && any(pel == curel)) return(TRUE)
    }
  }
  elfound
}


fuzzy_grep <- function(x, pattern, precision = NULL) {
  if (is.null(precision)) precision <- round(nchar(pattern) / 3)
  if (precision > nchar(pattern)) return(NULL)
  p <- sprintf("(%s){~%i}", pattern, precision)
  grep(pattern = p, x = x, ignore.case = FALSE)
}


string_dist_matrix <- function(string) {
  l <- length(string)
  m <- matrix(nrow = l, ncol = l)
  for (i in 1:(l - 1)) {
    for (j in (i + 1):l) {
      pos <- string_dist(string[i], string[j])
      if (pos == -1) pos <- 8
      m[i, j] <- m[j, i] <- pos
    }
  }

  rownames(m) <- string
  colnames(m) <- string

  m
}


string_dist <- function(s1, s2) {
  if (is.na(s1) || is.na(s2))
    return(-1)

  if (nchar(s1) > nchar(s2)) {
    x <- s2
    pattern <- s1
  } else {
    x <- s1
    pattern <- s2
  }

  len <- nchar(pattern)
  if (len > 8) len <- 8

  for (p in 1:len) {
    pos <- grep(pattern = sprintf("(%s){~%i}", pattern, p), x = x, ignore.case = FALSE)
    if (length(pos)) {
      return(p)
    }
  }

  return(-1)
}
