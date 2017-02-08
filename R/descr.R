#' @title Basic descriptive statistics
#' @name descr
#'
#' @description This function wraps the \code{\link[psych]{describe}}-function
#'              and prints a basic descriptive statistic, including variable labels.
#'
#' @param x A vector or a data frame. May also be a grouped data frame
#'          (see 'Note' and 'Examples').
#' @param max.length Numeric, indicating the maximum length of variable labels
#'          in the output. If variable names are longer than \code{max.length},
#'          they will be shortened to the last whole word within the first
#'          \code{max.length} chars.
#'
#' @inheritParams to_factor
#'
#' @return A data frame with basic descriptive statistics, derived from the
#'         \code{\link[psych]{describe}}-function. The additional column
#'         \code{NA.prc} informs about the percentage of missing values in
#'         a variable.
#'
#' @note \code{data} may also be a grouped data frame (see \code{\link[dplyr]{group_by}})
#'       with up to two grouping variables. Descriptive tables are created for each
#'       subgroup then.
#'
#' @examples
#' data(efc)
#' descr(efc, e17age, c160age)
#'
#' library(dplyr)
#' efc %>% select(e42dep, e15relat, c172code) %>% descr()
#'
#' # with grouped data frames
#' efc %>%
#'   group_by(e16sex) %>%
#'   select(e16sex, e42dep, e15relat, c172code) %>%
#'   descr()
#'
#' # you can select variables also inside 'descr()'
#' efc %>%
#'   group_by(e16sex, c172code) %>%
#'   descr(e16sex, c172code, e17age, c160age)
#'
#' # or even use select-helpers
#' descr(efc, ~contains("cop"), max.length = 20)
#'
#' @importFrom tibble as_tibble rownames_to_column
#' @importFrom dplyr select mutate
#' @importFrom psych describe
#' @export
descr <- function(x, ..., max.length = NULL) {

  # get dot data
  dd <- get_dot_data(x, match.call(expand.dots = FALSE)$`...`)

  # do we have a grouped data frame?
  if (inherits(dd, "grouped_df")) {
    # get grouped data
    grps <- get_grouped_data(dd)
    # now plot everything
    for (i in seq_len(nrow(grps))) {
      # copy back labels to grouped data frame
      tmp <- copy_labels(grps$data[[i]], dd)
      # print title for grouping
      cat(sprintf("\nGrouped by:\n%s\n", get_grouped_title(dd, grps, i, sep = "\n")))
      # print frequencies
      print(descr_helper(tmp, max.length))
      cat("\n")
    }
  } else {
    descr_helper(dd, max.length)
  }
}


descr_helper <- function(dd, max.length) {
  # get default variable name
  var.name <- colnames(dd)
  if (is.null(var.name)) var.name <- NA

  # call psych::describe and convert to tibble, remove some unnecessary
  # columns and and a variable label column
  x <- tibble::as_tibble(psych::describe(dd)) %>%
    tibble::rownames_to_column(var = "variable") %>%
    dplyr::select_("-vars", "-mad") %>%
    dplyr::mutate(
      label = unname(get_label(dd, def.value = var.name)),
      NA.prc = purrr::map_dbl(dd, ~ 100 * sum(is.na(.x)) / length(.x))
    ) %>%
    var_rename(median = "md")

  # check if labels should be truncated
  x$label <- shorten_string(x$label, max.length)

  # sort columns a bit
  x <- x[, c(1, 13, 2, 14, 3, 4, 12, 5, 6, 7, 8, 9, 10, 11)]

  # add own class for print-method
  class(x) <- c("sjmisc.descr", "data.frame")
  x
}