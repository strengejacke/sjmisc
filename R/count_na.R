utils::globalVariables("label")

#' @title Frequency table of tagged NA values
#' @name count_na
#'
#' @description This method counts tagged NA values (see \code{\link[haven]{tagged_na}})
#'              in a vector and prints a frequency table of counted tagged NAs.
#'
#' @param x A vector with tagged NA values, or a data frame or list with such
#'          vectors.
#'
#' @return A \code{\link[tibble]{tibble}} with counted tagged NA values.
#'
#' @examples
#' library(haven)
#' x <- labelled(c(1:3, tagged_na("a", "c", "z"), 4:1, tagged_na("a", "a", "c"),
#'                 1:3, tagged_na("z", "c", "c"), 1:4, tagged_na("a", "c", "z")),
#'               c("Agreement" = 1, "Disagreement" = 4, "First" = tagged_na("c"),
#'                 "Refused" = tagged_na("a"), "Not home" = tagged_na("z")))
#' count_na(x)
#'
#' y <- labelled(c(1:3, tagged_na("e", "d", "f"), 4:1, tagged_na("f", "f", "d"),
#'                 1:3, tagged_na("f", "d", "d"), 1:4, tagged_na("f", "d", "f")),
#'               c("Agreement" = 1, "Disagreement" = 4, "An E" = tagged_na("e"),
#'                 "A D" = tagged_na("d"), "The eff" = tagged_na("f")))
#' count_na(tibble::data_frame(x, y))
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr select_ filter
#' @importFrom haven is_tagged_na na_tag
#' @export
count_na <- function(x) {
  UseMethod("count_na")
}

#' @export
count_na.data.frame <- function(x) {
  lapply(x, FUN = count_na_helper)
}

#' @export
count_na.list <- function(x) {
  lapply(x, FUN = count_na_helper)
}

#' @export
count_na.default <- function(x) {
  count_na_helper(x)
}

count_na_helper <- function(x) {
  # check if x has any tagged NA values
  if (sum(haven::is_tagged_na(x)) < 1) {
    message("`x` has no tagged NA values.")
    return(NULL)
  }

  # get NA as tagged NA
  nav <- haven::na_tag(get_na(x, as.tag = F))
  nav.labels <- names(get_na(x, as.tag = T))
  # get values from x, including different NA tags
  values <- haven::na_tag(x)
  # only keep missing values
  values <- values[values %in% nav]
  # replace NA tag with label
  for (i in seq_len(length(nav))) {
    values[values == nav[i]] <- nav.labels[i]
  }
  # now compute frequency, and return a proper data frame
  tibble::as_tibble(frq(values)) %>%
    dplyr::select_("-val") %>%
    dplyr::filter(label != "NA")
}

