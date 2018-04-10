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
#' @param out Character vector, indicating whether the results should be printed
#'        to console (\code{out = "txt"}) or as HTML-table in the viewer-pane
#'        (\code{out = "viewer"}) or browser (\code{out = "browser"}).
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
#' descr(efc, contains("cop"), max.length = 20)
#'
#' @importFrom tibble as_tibble rownames_to_column
#' @importFrom dplyr select mutate
#' @importFrom psych describe
#' @importFrom sjlabelled copy_labels
#' @export
descr <- function(x, ..., max.length = NULL, out = c("txt", "viewer", "browser")) {

  out <- match.arg(out)

  if (out != "txt" && !requireNamespace("sjPlot", quietly = TRUE)) {
    message("Package `sjPlot` needs to be loaded to print HTML tables.")
    out <- "txt"
  }

  # get dot data
  dd <- get_dot_data(x, dplyr::quos(...))

  # return values
  dataframes <- list()

  # do we have a grouped data frame?
  if (inherits(dd, "grouped_df")) {

    # get grouped data
    grps <- get_grouped_data(dd)

    # now plot everything
    for (i in seq_len(nrow(grps))) {

      # copy back labels to grouped data frame
      tmp <- sjlabelled::copy_labels(grps$data[[i]], dd)

      dummy <- descr_helper(tmp, max.length)
      attr(dummy, "group") <- get_grouped_title(x, grps, i, sep = "\n")

      # save data frame for return value
      dataframes[[length(dataframes) + 1]] <- dummy
    }

    # add class-attr for print-method()
    if (out == "txt")
      class(dataframes) <- c("sjmisc_grpdescr", "list")
    else
      class(dataframes) <- c("sjt_grpdescr", "list")

  } else {
    dataframes <- descr_helper(dd, max.length)

    # add class-attr for print-method()
    if (out == "txt")
      class(dataframes) <- c("sjmisc_descr", class(dataframes))
    else
      class(dataframes) <- c("sjt_descr", class(dataframes))
  }

  # save how to print output
  attr(dataframes, "print") <- out

  dataframes
}


#' @importFrom tibble add_column as_tibble rownames_to_column
#' @importFrom psych describe
#' @importFrom dplyr select mutate
#' @importFrom sjlabelled get_label
#' @importFrom purrr map_dbl
descr_helper <- function(dd, max.length) {

  # check if we have a single vector, because purrr would return
  # a result for each *value*, instead one result for the complete vector
  if (!is.data.frame(dd))
    dd <- as.data.frame(dd)

  ff <- function(x) is.numeric(x) | is.factor(x)
  dd <- dplyr::select_if(dd, ff)

  # get default variable name
  var.name <- colnames(dd)
  if (is.null(var.name)) var.name <- NA

  # call psych::describe and convert to tibble, remove some unnecessary
  # columns and and a variable label column
  x <- dd %>%
    psych::describe(fast = FALSE) %>%
    tibble::as_tibble() %>%
    tibble::rownames_to_column(var = "variable") %>%
    dplyr::select(-.data$vars, -.data$mad) %>%
    dplyr::mutate(
      label = unname(sjlabelled::get_label(dd, def.value = var.name)),
      NA.prc = purrr::map_dbl(dd, ~ 100 * sum(is.na(.x)) / length(.x))
    ) %>%
    var_rename(median = "md")

  # check if labels should be truncated
  x$label <- shorten_string(x$label, max.length)

  # sort columns a bit
  x <- x[, c(1, 13, 2, 14, 3, 4, 12, 5, 6, 7, 8, 9, 10, 11)]

  # add type-column
  tibble::add_column(x, type = var_type(dd), .after = 1)
}
