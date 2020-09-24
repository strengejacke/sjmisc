#' @title Flat (proportional) tables
#' @name flat_table
#'
#' @description This function creates a labelled flat table or flat
#'              proportional (marginal) table.
#'
#' @param data A data frame. May also be a grouped data frame (see 'Note' and
#'          'Examples').
#' @param ... One or more variables of \code{data} that should be printed as table.
#' @param margin Specify the table margin that should be computed for proportional
#'               tables. By default, counts are printed. Use \code{margin = "cell"},
#'               \code{margin = "col"} or \code{margin = "row"} to print cell,
#'               column or row percentages of the table margins.
#' @param digits Numeric; for proportional tables, \code{digits} indicates the
#'               number of decimal places.
#' @param show.values Logical, if \code{TRUE}, value labels are prefixed by the
#'          associated value.
#' @inheritParams frq
#'
#' @return An object of class \code{\link[stats]{ftable}}.
#'
#' @note \code{data} may also be a grouped data frame (see \code{\link[dplyr]{group_by}})
#'       with up to two grouping variables. Cross tables are created for each subgroup then.
#'
#' @seealso \code{\link{frq}} for simple frequency table of labelled vectors.
#'
#' @examples
#' data(efc)
#'
#' # flat table with counts
#' flat_table(efc, e42dep, c172code, e16sex)
#'
#' # flat table with proportions
#' flat_table(efc, e42dep, c172code, e16sex, margin = "row")
#'
#' # flat table from grouped data frame. You need to select
#' # the grouping variables and at least two more variables for
#' # cross tabulation.
#' library(dplyr)
#' efc %>%
#'   group_by(e16sex) %>%
#'   select(e16sex, c172code, e42dep) %>%
#'   flat_table()
#'
#' efc %>%
#'   group_by(e16sex, e42dep) %>%
#'   select(e16sex, e42dep, c172code, n4pstu) %>%
#'   flat_table()
#'
#' # now it gets weird...
#' efc %>%
#'   group_by(e16sex, e42dep) %>%
#'   select(e16sex, e42dep, c172code, n4pstu, c161sex) %>%
#'   flat_table()
#'
#' @importFrom insight print_color
#' @importFrom dplyr case_when select
#' @importFrom stats ftable
#' @export
flat_table <- function(data, ..., margin = c("counts", "cell", "row", "col"), digits = 2, show.values = FALSE, weights = NULL) {

  # match arguments
  margin <- match.arg(margin)

  # check whether
  no.prop.table <- is.null(margin) || (margin == "counts")

  # check margins for proportional table
  marge <- dplyr::case_when(
    margin == "cell" ~ 0,
    margin == "row" ~ 1,
    margin == "col" ~ 2,
    TRUE ~ 0
  )

  # correct result
  if (marge == 0) marge <- NULL

  # get dot data
  dd <- get_dot_data(data, dplyr::quos(...))

  # weights
  w <- deparse(substitute(weights))
  if (w != "NULL") {
    w <- gsub("\"", "", w, fixed = FALSE)
    if (!is.null(data[[w]])) {
      w <- data[[w]]
    } else {
      w <- eval(substitute(weights))
    }
  } else {
    w <- NULL
  }

  # do we have a grouped data frame?
  if (inherits(dd, "grouped_df")) {

    # get grouped data
    grps <- get_grouped_data(dd)

    # now plot everything
    for (i in seq_len(nrow(grps))) {

      # copy back labels to grouped data frame
      tmp <- sjlabelled::copy_labels(grps$data[[i]], dd)

      # print title for grouping
      insight::print_color("\nGrouped by: ", "red")
      insight::print_color(sprintf("%s\n\n", get_grouped_title(dd, grps, i, sep = ", ", long = FALSE)), "cyan")

      # print frequencies
      print(com_ft(tmp, show.values, no.prop.table, marge, digits, w = w))
      cat("\n")
    }
  } else {
    com_ft(dd, show.values, no.prop.table, marge, digits, w = w)
  }
}


#' @importFrom stats as.formula xtabs
#' @importFrom sjlabelled as_label
com_ft <- function(dd, show.values, no.prop.table, marge, digits, w = NULL) {
  # select variables, convert to label and create ftable-pbject
  x <- sjlabelled::as_label(dd, add.non.labelled = TRUE, prefix = show.values)

  if (!is.null(w)) {
    f <- paste(colnames(x), collapse = " + ")
    x$.weights <- w
    f <- stats::as.formula(paste(".weights ~ ", f))
    x <- round(stats::xtabs(formula = f, data = x))
  }
  x <- stats::ftable(x)

  # if required, compute table margins
  if (!no.prop.table) {
    x <- x %>%
      prop.table(margin = marge) %>%
      round(digits = digits + 2)
    x <- x * 100
  }

  x
}
