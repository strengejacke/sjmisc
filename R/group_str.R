#' @title Group near elements of string vectors
#' @name group_str
#'
#' @seealso \code{\link{str_pos}}
#'
#' @description This function groups elements of a string vector (character or string
#'                variable) according to the element's distance ('similatiry'). The
#'                more similar two string elements are, the higher is the
#'                chance to be combined into a group.
#'
#' @param strings Character vector with string elements.
#' @param maxdist Maximum distance between two string elements, which is allowed to treat two
#'          elements as similar or equal.
#' @param strict Logical; if \code{TRUE}, value matching is more strictly. See 'Examples'.
#' @param trim.whitespace Logical; if \code{TRUE} (default), leading and trailing white spaces will
#'          be removed from string values.
#' @param remove.empty Logical; if \code{TRUE} (default), empty string values will be removed from the
#'          character vector \code{strings}.
#' @param verbose Logical; if \code{TRUE}, the progress bar is displayed when computing the distance matrix.
#'          Default in \code{FALSE}, hence the bar is hidden.
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
#' newstring <- group_str(oldstring, maxdist = 3)
#' frq(oldstring)
#' frq(newstring)
#'
#' # be more strict with matching pairs
#' newstring <- group_str(oldstring, maxdist = 3, strict = TRUE)
#' frq(oldstring)
#' frq(newstring)
#'
#' @importFrom utils txtProgressBar
#' @export
group_str <- function(strings,
                      maxdist = 2,
                      strict = FALSE,
                      trim.whitespace = TRUE,
                      remove.empty = TRUE,
                      verbose = FALSE) {
  # coerce to character, if necessary
  if (!is.character(strings)) strings <- as.character(strings)

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
        if (!is.na(m[i, j]) && m[i, j] <= maxdist) {
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
                if (m[cnt, j] <= maxdist && m[i, cnt] <= maxdist) {
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


fuzzy_find <- function(x, pattern, precision = NULL) {
  if (is.null(precision)) precision <- round(nchar(pattern) / 3)
  if (nchar(precision) > nchar(pattern)) precision <- round(nchar(pattern) / 3)
  p <- sprintf("(%s){~%i}", pattern, precision)
  grep(pattern = p, x = x)
}


string_dist_matrix <- function(string) {
  l <- length(string)
  m <- matrix(nrow = l, ncol = l)
  for (i in 1:l) {
    for (j in 1:l) {
      if (nchar(string[j]) > nchar(string[i])) {
        x <- string[i]
        pattern <- string[j]
      } else {
        x <- string[j]
        pattern <- string[i]
      }

      len <- nchar(pattern)
      if (len > 8) len <- 8

      for (p in 0:len) {
        pos <- fuzzy_find(x = x, pattern = pattern, precision = p)
        if (length(pos)) {
          m[i, j] <- p
          break
        }
      }
      if (!length(pos)) m[i, j] <- 8
    }
  }

  rownames(m) <- string
  colnames(m) <- string

  m
}
