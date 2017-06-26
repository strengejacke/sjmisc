#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom dplyr quos select
get_dot_data <- function(x, qs) {
  if (sjmisc::is_empty(qs))
    x
  else
    suppressMessages(dplyr::select(x, !!!qs))
}

# return names of objects passed as ellipses argument
dot_names <- function(dots) unname(unlist(lapply(dots, as.character)))

is_float <- function(x) is.numeric(x) && !all(x %% 1 == 0, na.rm = T)

is_foreign <- function(x) !is.null(x) && x == "value.labels"


# auto-detect attribute style for value labels.
# either haven style ("labels") or foreign style
# ("value.labels")
getValLabelAttribute <- function(x) {
  attr.string <- NULL

  # check if x is data frame. if yes, just retrieve one "example" variable
  if (is.data.frame(x)) {
    # find first variable with labels or value.labels attribute
    for (i in seq_len(ncol(x))) {
      # has any attribute?
      if (!is.null(attr(x[[i]], "labels", exact = T))) {
        attr.string <- "labels"
        break
      } else if (!is.null(attr(x[[i]], "value.labels", exact = T))) {
        attr.string <- "value.labels"
        break
      }
    }
  } else {
    # check if vector has labels attribute
    if (!is.null(attr(x, "labels", exact = T))) attr.string <- "labels"
    # check if vector has value.labels attribute
    if (!is.null(attr(x, "value.labels", exact = T))) attr.string <- "value.labels"
  }

  # not found any label yet?
  if (is.null(attr.string)) attr.string <- "labels"

  attr.string
}

# shorten a string
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
