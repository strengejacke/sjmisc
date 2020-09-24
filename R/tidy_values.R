#' @title Clean values of character vectors.
#' @name tidy_values
#'
#' @description This function "cleans" values of a character vector or levels of
#'   a factor by removing space and punctuation characters.
#'
#' @inheritParams to_dummy
#'
#' @return \code{x}, with "cleaned" values or levels.
#'
#' @examples
#' f1 <- sprintf("Char %s", sample(LETTERS[1:5], size = 10, replace = TRUE))
#' f2 <- as.factor(sprintf("F / %s", sample(letters[1:5], size = 10, replace = TRUE)))
#' f3 <- sample(1:5, size = 10, replace = TRUE)
#'
#' x <- data.frame(f1, f2, f3, stringsAsFactors = FALSE)
#'
#' clean_values(f1)
#' clean_values(f2)
#' clean_values(x)
#'
#' @importFrom dplyr quos
#' @importFrom purrr map_df
#' @export
tidy_values <- function(x, ...) {
  # evaluate arguments, generate data
  .dat <- get_dot_data(x, dplyr::quos(...))

  if (is.data.frame(x)) {
    purrr::map_df(.dat, ~ tidy_value_helper(.x))
  } else {
    tidy_value_helper(x)
  }
}


tidy_value_helper <- function(x) {
  pattern <- "[[:space:][:punct:]]+"
  if (is.character(x)) {
    x <- gsub(pattern = pattern, replacement = "_", x = x)
  } else if (is.factor(x)) {
    levels(x) <- gsub(pattern = pattern, replacement = "_", x = levels(x))
  }
  x
}


#' @rdname tidy_values
#' @export
clean_values <- tidy_values
