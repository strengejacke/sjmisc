#' @title Split numeric variables into smaller groups
#' @name split_var
#'
#' @description Recode numeric variables into equal sized groups, i.e. a
#'                variable is cut into a smaller number of groups at
#'                specific cut points.
#'
#' @seealso \code{\link{group_var}} to group variables into equal ranged groups,
#'          or \code{\link{rec}} to recode variables.
#'
#' @param n The new number of groups that \code{x} should be split into.
#' @param inclusive Logical; if \code{TRUE}, cut point value are included in
#'          the preceeding group. This may be necessary if cutting a vector into
#'          groups does not define proper ("equal sized") group sizes.
#'          See 'Note' and 'Examples'.
#'
#' @inheritParams to_factor
#' @inheritParams group_var
#' @inheritParams rec
#'
#' @return A grouped variable with equal sized groups. If \code{x} is a data frame,
#'         for \code{append = TRUE}, \code{x} including the grouped variables
#'         as new columns is returned; if \code{append = FALSE}, only
#'         the grouped variables will be returned.
#'
#' @details \code{split_var()} splits a variable into equal sized groups, where the
#'            amount of groups depends on the \code{groupcount}-argument. Thus,
#'            this functions \code{\link{cut}s} a variable into groups at the
#'            specified \code{\link[stats]{quantile}s}.
#'            \cr \cr
#'            By contrast, \code{\link{group_var}} recodes a variable into
#'            groups, where groups have the same value range
#'            (e.g., from 1-5, 6-10, 11-15 etc.).
#'            \cr \cr
#'            \code{split_var()} also works on grouped data frames (see \code{\link[dplyr]{group_by}}).
#'            In this case, splitting is applied to the subsets of variables
#'            in \code{x}. See 'Examples'.
#'
#' @note In case a vector has only few number of unique values, splitting into
#'         equal sized groups may fail. In this case, use the \code{inclusive}-argument
#'         to shift a value at the cut point into the lower, preceeding group to
#'         get equal sized groups. See 'Examples'.
#'
#' @examples
#' data(efc)
#' # non-grouped
#' table(efc$neg_c_7)
#'
#' # split into 3 groups
#' table(split_var(efc$neg_c_7, n = 3))
#'
#' # split multiple variables into 3 groups
#' split_var(efc, neg_c_7, pos_v_4, e17age, n = 3)
#' frq(split_var(efc, neg_c_7, pos_v_4, e17age, n = 3))
#'
#' # original
#' table(efc$e42dep)
#'
#' # two groups, non-inclusive cut-point
#' # vector split leads to unequal group sizes
#' table(split_var(efc$e42dep, n = 2))
#'
#' # two groups, inclusive cut-point
#' # group sizes are equal
#' table(split_var(efc$e42dep, n = 2, inclusive = TRUE))
#'
#' # Unlike dplyr's ntile(), split_var() never splits a value
#' # into two different categories, i.e. you always get a clean
#' # separation of original categories
#' library(dplyr)
#'
#' x <- dplyr::ntile(efc$neg_c_7, n = 3)
#' table(efc$neg_c_7, x)
#'
#' x <- split_var(efc$neg_c_7, n = 3)
#' table(efc$neg_c_7, x)
#'
#' # works also with gouped data frames
#' mtcars %>%
#'   split_var(disp, n = 3) %>%
#'   table()
#'
#' mtcars %>%
#'   group_by(cyl) %>%
#'   split_var(disp, n = 3) %>%
#'   table()
#'
#' @importFrom stats quantile
#' @export
split_var <- function(x, ..., n, as.num = FALSE, val.labels = NULL, var.label = NULL, inclusive = FALSE, append = FALSE, suffix = "_g") {

  # evaluate arguments, generate data
  .dat <- get_dot_data(x, dplyr::quos(...))

  recode_fun(
    x = x,
    .dat = .dat,
    fun = get("split_var_helper", asNamespace("sjmisc")),
    suffix = suffix,
    append = append,
    groupcount = n,
    as.num = as.num,
    var.label = var.label,
    val.labels = val.labels,
    inclusive = inclusive
  )
}


#' @importFrom stats quantile
#' @importFrom sjlabelled get_label set_label set_labels
split_var_helper <- function(x, groupcount, as.num, val.labels, var.label, inclusive) {
  # retrieve variable label
  if (is.null(var.label))
    var_lab <- sjlabelled::get_label(x)
  else
    var_lab <- var.label

  # do we have any value labels?
  val_lab <- val.labels

  # amount of "cuts" is groupcount - 1
  zaehler <- seq_len(groupcount - 1)

  # prepare division
  nenner <- rep(groupcount, length(zaehler))

  # get quantiles
  qu_prob <- zaehler / nenner

  # get quantile values
  grp_cuts <- stats::quantile(x, qu_prob, na.rm = TRUE)

  # create breaks. need to check if these are non-unique
  breaks <- unique(c(0, grp_cuts, max(x, na.rm = T)))

  # cut variables into groups
  retval <- cut(
    x = x,
    breaks = breaks,
    include.lowest = !inclusive,
    right = inclusive
  )

  # rename factor levels
  levels(retval) <- seq_len(groupcount)

  # to numeric?
  if (as.num) retval <- to_value(retval)

  # set back variable and value labels
  retval <- suppressWarnings(sjlabelled::set_label(retval, label = var_lab))
  retval <- suppressWarnings(sjlabelled::set_labels(retval, labels = val_lab))

  # return value
  retval
}
