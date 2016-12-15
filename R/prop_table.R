#' @title Flat (proportional) tables
#' @name flat_table
#'
#' @description This function creates a labelled flat table or flat
#'              proportional (marginal) table.
#'
#' @param .data A data frame.
#' @param ... One or more variables of \code{.data} that should be printed as table.
#' @param margin Specify the table margin that should be computed for proportional
#'               tables. By default, counts are printed. Use \code{margin = "cell"},
#'               \code{margin = "col"} or \code{margin = "row"} to print cell,
#'               column or row percentages of the table margins.
#' @param digits Numeric; for proportional tables, \code{digits} indicates the
#'               number of decimal places.
#' @param show.values Logical, if \code{TRUE}, value labels are prefixed by the
#'          associated value.
#'
#' @return An object of class \code{\link[stats]{ftable}}.
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
#' @importFrom dplyr case_when select
#' @importFrom stats ftable
#' @export
flat_table <- function(.data, ..., margin = c("counts", "cell", "row", "col"), digits = 2, show.values = FALSE) {

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
  dd <- get_dot_data(.data, match.call(expand.dots = FALSE)$`...`)

  # select variables, convert to label and create ftable-pbject
  x <- dd %>%
    to_label(add.non.labelled = TRUE, prefix = show.values) %>%
    stats::ftable()

  # if required, compute table margins
  if (!no.prop.table) {
    x <- x %>%
      prop.table(margin = marge) %>%
      round(digits = digits + 2)
    x <- x * 100
  }

  x
}
