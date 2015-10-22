#' @title Split numeric variables into smaller groups
#' @name split_var
#'
#' @description Recode numeric variables into equal sized groups, i.e. a
#'                variable is cut into a smaller number of groups at
#'                specific cut points.
#'
#' @seealso \itemize{
#'            \item \code{\link{group_var}}
#'            \item \code{\link{rec}}
#'          }
#'
#' @param x Numeric vector, data frame or list of numeric vectors,
#'            which should split into groups.
#' @param groupcount The new number of groups that \code{x} should be split into.
#' @param inclusive Logical; if \code{TRUE}, cut point value are included in
#'          the preceeding group. This may be necessary if cutting a vector into
#'          groups does not define proper ("equal sized") group sizes.
#'          See 'Note' and 'Examples'.
#'
#' @inheritParams group_var
#' @inheritParams rec
#'
#' @return A grouped variable with equal sized groups.
#'
#' @details \code{split_var} splits a variable into equal sized groups, where the
#'            amount of groups depends on the \code{groupcount}-argument. Thus,
#'            this functions \code{\link{cut}s} a variable into groups at the
#'            specified \code{\link[stats]{quantile}s}.
#'            \cr \cr
#'            By contrast, \code{\link{group_var}} recodes a variable into
#'            groups, where all values within a group have the same range
#'            (e.g., from 1-5, 6-10, 11-15 etc.).
#'
#' @note In case a vector has only few different unique values, splitting into
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
#' table(split_var(efc$neg_c_7, 3))
#'
#'
#' # original
#' table(efc$e42dep)
#'
#' # two groups, non-inclusive cut-point
#' # vector split leads to unequal group sizes
#' table(split_var(efc$e42dep, 2))
#'
#' # two groups, inclusive cut-point
#' # group sizes are equal
#' table(split_var(efc$e42dep, 2, inclusive = TRUE))
#'
#' @importFrom stats quantile
#' @export
split_var <- function(x, groupcount, as.num = FALSE, val.labels = NULL, var.label = NULL, inclusive = FALSE) {
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # na all
    for (i in 1:nvars) x[[i]] <- split_var_helper(x[[i]],
                                                  groupcount,
                                                  as.num,
                                                  val.labels,
                                                  var.label,
                                                  inclusive)
    return(x)
  } else {
    return(split_var_helper(x,
                            groupcount,
                            as.num,
                            val.labels,
                            var.label,
                            inclusive))
  }
}

split_var_helper <- function(x, groupcount, as.num, val.labels, var.label, inclusive) {
  # retrieve variable label
  if (is.null(var.label))
    var_lab <- get_label(x)
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
  # cut variables into groups
  retval <- cut(x,
                c(0, grp_cuts, max(x, na.rm = T)),
                include.lowest = !inclusive,
                right = inclusive)
  # rename factor levels
  levels(retval) <- c(1:groupcount)
  # to numeric?
  if (as.num) retval <- to_value(retval)
  # set back variable and value labels
  retval <- suppressWarnings(set_label(retval, var_lab))
  retval <- suppressWarnings(set_labels(retval, val_lab))
  # return value
  return(retval)
}
