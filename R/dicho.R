#' @title Dichotomize variables
#' @name dicho
#'
#' @description Dichotomizes variables into dummy variables (0/1). Dichotomization is
#'                either done by median, mean or a specific value (see \code{dich.by}).
#'
#' @param dich.by Indicates the split criterion where a variable is dichotomized.
#'          Must be one of the following values (may be abbreviated):
#'          \describe{
#'            \item{\code{"median"} or \code{"md"}}{by default, \code{x} is split into two groups at the median.}
#'            \item{\code{"mean"} or \code{"m"}}{splits \code{x} into two groups at the mean of \code{x}.}
#'            \item{numeric value}{splits \code{x} into two groups at the specific value. Note that the value is inclusive, i.e. \code{dich.by = 10} will split \code{x} into one group with values from lowest to 10 and another group with values greater than 10.}
#'            }
#' @param val.labels Optional character vector (of length two), to set value label
#'          attributes of dichotomized variable (see \code{\link[sjlabelled]{set_labels}}).
#'          If \code{NULL} (default), no value labels will be set.
#'
#' @inheritParams to_factor
#' @inheritParams rec
#'
#' @return \code{x}, dichotomized. If \code{x} is a data frame, only
#'         the dichotomized variables will be returned.
#'
#' @note Variable label attributes are preserved (unless changed via
#'       \code{var.label}-argument).
#'
#' @details \code{dicho()} also works on grouped data frames (see \code{\link[dplyr]{group_by}}).
#'          In this case, dichotomization is applied to the subsets of variables
#'          in \code{x}. See 'Examples'.
#'
#' @examples
#' data(efc)
#' summary(efc$c12hour)
#' # split at median
#' table(dicho(efc$c12hour))
#' # split at mean
#' table(dicho(efc$c12hour, dich.by = "mean"))
#' # split between value lowest to 30, and above 30
#' table(dicho(efc$c12hour, dich.by = 30))
#'
#' # sample data frame, values from 1-4
#' head(efc[, 6:10])
#'
#' # dichtomized values (1 to 2 = 0, 3 to 4 = 1)
#' library(dplyr)
#' efc %>%
#'   select(6:10) %>%
#'   dicho(dich.by = 2) %>%
#'   head()
#'
#' # dichtomize several variables in a data frame
#' dicho(efc, c12hour, e17age, c160age, append = FALSE)
#'
#' # dichotomize and set labels
#' frq(dicho(
#'   efc, e42dep,
#'   var.label = "Dependency (dichotomized)",
#'   val.labels = c("lower", "higher"),
#'   append = FALSE
#' ))
#'
#' # works also with gouped data frames
#' mtcars %>%
#'   dicho(disp, append = FALSE) %>%
#'   table()
#'
#' mtcars %>%
#'   group_by(cyl) %>%
#'   dicho(disp, append = FALSE) %>%
#'   table()
#'
#' # dichotomizing grouped data frames leads to different
#' # results for a dichotomized variable, because the split
#' # value is different for each group.
#' # compare:
#' mtcars %>%
#'   group_by(cyl) %>%
#'   summarise(median = median(disp))
#'
#' median(mtcars$disp)
#'
#' @importFrom dplyr group_vars
#' @importFrom tidyr unnest
#' @export
dicho <- function(x, ..., dich.by = "median", as.num = FALSE, var.label = NULL, val.labels = NULL, append = TRUE, suffix = "_d") {
  # check for correct dichotome types
  if (!is.numeric(dich.by) && !dich.by %in% c("median", "mean", "md", "m")) {
    stop("argument `dich.by` must either be `median`, `mean` or a numerical value." , call. = FALSE)
  }

  # evaluate arguments, generate data
  .dat <- get_dot_data(x, dplyr::quos(...))

  recode_fun(
    x = x,
    .dat = .dat,
    fun = get("dicho_helper", asNamespace("sjmisc")),
    suffix = suffix,
    append = append,
    dich.by = dich.by,
    as.num = as.num,
    var.label = var.label,
    val.labels = val.labels
  )
}


#' @importFrom stats median
dicho_helper <- function(x, dich.by, as.num, var.label, val.labels) {
  # do we have labels? if not, try to
  # automatically get variable labels
  if (is.null(var.label))
    varlab <- sjlabelled::get_label(x)
  else
    varlab <- var.label

  # check if factor. factors need conversion
  # to numeric before dichtomizing
  if (is.factor(x)) {
    # non-numeric-factor cannot be converted
    if (is_num_fac(x)) {
      # try to convert to numeric
      x <- as.numeric(as.character(x))
    } else {
      # convert non-numeric factor to numeric
      # factor levels are replaced by numeric values
      x <- to_value(x, keep.labels = FALSE)
      message("Trying to dichotomize non-numeric factor.")
    }
  }
  # split at specific value
  if (is.numeric(dich.by)) {
    x <- ifelse(x <= dich.by, 0, 1)
  } else if (dich.by == "median" || dich.by == "md") {
    x <- ifelse(x <= stats::median(x, na.rm = T), 0, 1)
    # split at mean
  } else if (dich.by == "mean" || dich.by == "m") {
    x <- ifelse(x <= mean(x, na.rm = T), 0, 1)
  }

  if (!as.num) x <- as.factor(x)
  # set back variable labels
  if (!is.null(varlab)) x <- suppressWarnings(sjlabelled::set_label(x, label = varlab))
  # set value labels
  if (!is.null(val.labels)) x <- suppressWarnings(sjlabelled::set_labels(x, labels = val.labels))

  x
}



#' @importFrom tibble as_tibble
#' @importFrom dplyr group_indices group_vars slice ungroup bind_cols
recode_fun <- function(x, .dat, fun, suffix, append, ...) {
  if (is.data.frame(x)) {

    # remember original data, if user wants to bind columns
    orix <- tibble::as_tibble(x)

    # do we have a grouped data frame?
    if (inherits(.dat, "grouped_df")) {

      # get grouping indices and variables
      grps <- dplyr::group_indices(.dat)
      grp.vars <- dplyr::group_vars(.dat)

      # names of grouping variables
      vars <- colnames(.dat)[colnames(.dat) %nin% grp.vars]
      .dat <- dplyr::ungroup(.dat)

      # iterate all groups
      for (i in unique(grps)) {

        # slice cases for each group
        keep <- which(grps == i)
        group <- dplyr::slice(.dat, !! keep)

        # now iterate all variables of interest
        for (j in vars) {
          group[[j]] <- fun(x = group[[j]], ...)
        }

        # write back data
        .dat[keep, ] <- group
      }

      # remove grouping column
      x <- tibble::as_tibble(.dat[colnames(.dat) %nin% grp.vars])
    } else {
      # iterate variables of data frame
      for (i in colnames(.dat)) {
        x[[i]] <- fun(x = .dat[[i]], ...)
      }

      # coerce to tibble and select only recoded variables
      x <- tibble::as_tibble(x[colnames(.dat)])
    }

    # add suffix to recoded variables?
    if (!is.null(suffix) && !sjmisc::is_empty(suffix))
      colnames(x) <- sprintf("%s%s", colnames(x), suffix)

    # combine data
    if (append) x <- dplyr::bind_cols(orix, x)
  } else {
    x <- fun(x = .dat, ...)
  }

  x

}
