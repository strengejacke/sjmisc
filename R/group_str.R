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
#' @param method Method for distance calculation. The default is \code{"lv"}. See
#'          \code{\link[stringdist]{stringdist}} for details.
#' @param strict Logical; if \code{TRUE}, value matching is more strictly. See 'Examples'.
#' @param trim.whitespace Logical; if \code{TRUE} (default), leading and trailing white spaces will
#'          be removed from string values.
#' @param remove.empty Logical; if \code{TRUE} (default), empty string values will be removed from the
#'          character vector \code{strings}.
#' @param showProgressBar Logical; if \code{TRUE}, the progress bar is displayed when computing the distance matrix.
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
#' \dontrun{
#' library(sjPlot)
#' # print table to compare original and grouped string
#' sjt.frq(data.frame(oldstring, newstring),
#'         removeStringVectors = FALSE,
#'         autoGroupStrings = FALSE)
#'
#' # larger groups
#' newstring <- group_str(oldstring, maxdist = 3)
#' sjt.frq(data.frame(oldstring, newstring),
#'         removeStringVectors = FALSE,
#'         autoGroupStrings = FALSE)
#'
#' # be more strict with matching pairs
#' newstring <- group_str(oldstring, maxdist = 3, strict = TRUE)
#' sjt.frq(data.frame(oldstring, newstring),
#'         removeStringVectors = FALSE,
#'         autoGroupStrings = FALSE)}
#'
#' @importFrom utils txtProgressBar
#' @importFrom stringdist stringdistmatrix
#' @export
group_str <- function(strings,
                      maxdist = 2,
                      method = "lv",
                      strict = FALSE,
                      trim.whitespace = TRUE,
                      remove.empty = TRUE,
                      showProgressBar = FALSE) {
  # -------------------------------------
  # check if required package is available
  # -------------------------------------
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    stop("Package `stringdist` needed for this function to work. Please install it.", call. = FALSE)
  }
  # -------------------------------------
  # coerce to character, if necessary
  # -------------------------------------
  if (!is.character(strings)) strings <- as.character(strings)
  # -------------------------------------
  # trim white spaces
  # -------------------------------------
  if (trim.whitespace) {
    for (i in 1:length(strings)) strings[i] <- trim(strings[i])
  }
  # -------------------------------------
  # remove empty values
  # -------------------------------------
  if (remove.empty) {
    removers <- c()
    for (i in 1:length(strings)) {
      if (is_empty(strings[i])) removers <- c(removers, i)
    }
    if (length(removers) > 0) strings <- strings[-removers]
  }
  # -------------------------------------
  # create matrix from string values of variable
  # -------------------------------------
  m <- stringdist::stringdistmatrix(strings,
                                    strings,
                                    method = method,
                                    useNames = "strings")
  # -------------------------------------
  # init variable that contains "close" pairs
  # -------------------------------------
  pairs <- list()
  # -------------------------------------
  # helper function that finds elements in
  # final list of grouped elements
  # -------------------------------------
  findInPairs <- function(curel) {
    elfound <- FALSE
    if (length(pairs) > 0) {
      for (ll in 1:length(pairs)) {
        pel <- pairs[[ll]]
        if (any(pel == curel)) elfound <- TRUE
      }
    }
    return(elfound)
  }
  # -------------------------------------
  # create progress bar
  # -------------------------------------
  if (showProgressBar) pb <- utils::txtProgressBar(min = 0,
                                                   max = ncol(m),
                                                   style = 3)
  # -------------------------------------
  # iterate matrix
  # -------------------------------------
  for (i in 1:nrow(m)) {
    # update progress bar
    if (showProgressBar) utils::setTxtProgressBar(pb, i)
    # -------------------------------------
    # check if current element is already grouped
    # -------------------------------------
    if (!findInPairs(rownames(m)[i])) {
      # -------------------------------------
      # current row element has not been grouped
      # yet, so go on...
      # -------------------------------------
      pairvector <- c()
      for (j in 1:ncol(m)) {
        # -------------------------------------
        # check if we found a pair's distance that
        # is within the maximum requested distance
        # i.e. which are "close" enough
        # -------------------------------------
        if (m[i, j] <= maxdist) {
          # -------------------------------------
          # go through all rows of this column and
          # check if there's a better match for the
          # currently compared token
          # -------------------------------------
          foundBetterToken <- !strict
          for (cnt in 1:nrow(m)) {
            if (strict) {
              if (m[cnt, j] > 0 && m[cnt, j] < m[i, j]) foundBetterToken <- TRUE
            } else {
              if (m[cnt, j] <= maxdist && m[i, cnt] <= maxdist) foundBetterToken <- FALSE
            }
          }
          # -------------------------------------
          # in the current column, there's no better
          # matching of strings, so we pick this values
          # and add it to our results
          # -------------------------------------
          if (!foundBetterToken) {
            # -------------------------------------
            # remember string value
            # -------------------------------------
            token <- colnames(m)[j]
            # -------------------------------------
            # check if we already found a string value
            # within this column. if not, add string values
            # to "close" pairs of this column
            # -------------------------------------
            if (!any(pairvector == token) && !findInPairs(token)) pairvector <- c(pairvector, token)
          }
        }
      }
      # -------------------------------------
      # now we have a vector with all "close" string values
      # from the current row's value
      # -------------------------------------
      pairvector <- sort(pairvector)
      # -------------------------------------
      # check if we already have saved these values to our list
      # if not, add "close" values as new list element
      # -------------------------------------
      if (!any(unlist(lapply(pairs, function(x) length(x) == length(pairvector) && any(x == pairvector))))) pairs <- c(pairs, list(pairvector))
    }
  }
  # -------------------------------------
  # we now have a list, where each list element
  # is a vector of "close" string values
  # -------------------------------------
  strings.new <- c()
  # -------------------------------------
  # go through each list element
  # -------------------------------------
  for (i in 1:length(pairs)) {
    r <- pairs[[i]]
    # -------------------------------------
    # find vector indices of "close" values in
    # original string
    # -------------------------------------
    indices <- unlist(lapply(r, function(x) which(strings == x)))
    newvalue <- r[1]
    count <- 2
    # -------------------------------------
    # "merge" each close values into one
    # single value that combines all close values
    # -------------------------------------
    while (count <= length(r)) {
      newvalue <- paste0(newvalue, ", ", r[count])
      count <- count + 1
    }
    strings.new[indices] <- newvalue
  }
  if (showProgressBar) close(pb)
  # -------------------------------------
  # return new vector, where all single "close"
  # values are replaced by the group of closed values.
  # e.g. the three values "hello", "holle" and "hole"
  # will be "recoded" into on value "hello, holle, hole"
  # -------------------------------------
  return(strings.new)
}
