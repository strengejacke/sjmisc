#' @title Change reference level of (numeric) factors
#' @name ref_lvl
#'
#' @description Changes the reference level of (numeric) factor.
#'
#' @seealso \code{\link{to_factor}} to convert numeric vectors into factors;
#'            \code{\link{rec}} to recode variables.
#'
#' @param lvl Eiher numeric, indicating the new reference level, or a string,
#'   indicating the value label from the new reference level. If \code{x} is a
#'   factor with non-numeric factor levels, \code{relevel(x, ref = lvl)} is
#'   returned. See 'Examples'.
#'
#' @inheritParams to_factor
#'
#' @return \code{x} with new reference level. If \code{x}
#'           is a data frame, the complete data frame \code{x} will be returned,
#'           where variables specified in \code{...} will be re-leveled;
#'           if \code{...} is not specified, applies to all variables in the
#'           data frame.
#'
#' @details Unlike \code{\link[stats]{relevel}}, this function behaves differently
#'   for factor with numeric factor levels or for labelled data, i.e. factors
#'   with value labels for the values. \code{ref_lvl()} changes the reference
#'   level by recoding the factor's values using the \code{\link{rec}} function.
#'   Hence, all values from lowest up to the reference level indicated by
#'   \code{lvl} are recoded, with \code{lvl} starting as lowest factor value.
#'   For factors with non-numeric factor levels, the function simply returns
#'   \code{relevel(x, ref = lvl)}. See 'Examples'.
#'
#' @examples
#' data(efc)
#' x <- to_factor(efc$e42dep)
#' str(x)
#' frq(x)
#'
#' # see column "val" in frq()-output, which indicates
#' # how values/labels were recoded after using ref_lvl()
#' x <- ref_lvl(x, lvl = 3)
#' str(x)
#' frq(x)
#'
#' library(dplyr)
#' dat <- efc %>%
#'   select(c82cop1, c83cop2, c84cop3) %>%
#'   to_factor()
#'
#' frq(dat)
#' ref_lvl(dat, c82cop1, c83cop2, lvl = 2) %>% frq()
#'
#' # compare numeric and string value for "lvl"-argument
#' x <- to_factor(efc$e42dep)
#' frq(x)
#' ref_lvl(x, lvl = 2) %>% frq()
#' ref_lvl(x, lvl = "slightly dependent") %>% frq()
#'
#' # factors with non-numeric factor levels
#' data(iris)
#' levels(iris$Species)
#' levels(ref_lvl(iris$Species, lvl = 3))
#' levels(ref_lvl(iris$Species, lvl = "versicolor"))
#'
#' @importFrom dplyr quos
#' @export
ref_lvl <- function(x, ..., lvl = NULL) {

  # evaluate arguments, generate data
  .dat <- get_dot_data(x, dplyr::quos(...))

  if (is.data.frame(x)) {
    # iterate variables of data frame
    for (i in colnames(.dat)) {
      x[[i]] <- ref_lvl_helper(.dat[[i]], value = lvl)
    }
  } else {
    x <- ref_lvl_helper(.dat, value = lvl)
  }

  x
}


#' @importFrom sjlabelled get_values get_labels get_label set_label set_labels
ref_lvl_helper <- function(x, value) {
  # check correct arguments
  if (is.null(x)) {
    warning("`x` is NULL.", call. = F)
    return(x)
  }

  if (!is.factor(x)) {
    warning("`x` needs to be a factor.", call. = F)
    return(x)
  }

  if (!is_num_fac(x)) {
    return(stats::relevel(x, ref = value))
  }

  if (is.numeric(value)) {
    # get values from factor
    vals <- as.numeric(levels(x))
  } else {
    # get value labels, check if we have a label instead of number as
    lab.values <- sjlabelled::get_labels(
      x,
      attr.only = TRUE,
      values = "n",
      drop.na = TRUE
    )
    vals <- as.numeric(names(lab.values))
    value <- as.numeric(names(lab.values[lab.values == value]))
  }

  # check if ref-lvl exists in values
  if (!value %in% vals) {
    warning("`x` has no factor level indicated by the reference level `value`.", call. = F)
    return(x)
  }

  # get value labels
  val.labs <- sjlabelled::get_labels(x)

  # get variable label
  var.lab <- sjlabelled::get_label(x)

  # find position of reference level
  refpos <- which(vals == value)

  # new order of factor levels, if reference level
  # is on first position
  neword <- c(vals[refpos], vals[-refpos])

  # now recode variable. therefore, we need a string pattern
  # for the recoding
  rec.pattern <- paste0(sprintf("%i=%i;", neword, vals), collapse = "")

  # recode now
  x <- rec(x, rec = rec.pattern, as.num = FALSE, append = FALSE)

  # set back labels
  if (!is.null(var.lab) && !sjmisc::is_empty(var.lab)) {
    sjlabelled::set_label(x) <- var.lab
  }

  if (!is.null(val.labs)) {
    # we need "order" twice here, because "neword" refers to the actual
    # values of "x", so "neword" might have negative values, or zero.
    # so we first need the "order" function to have numeric values from
    # 1 to length(x) - and a second "order" call to get the correct order
    # of these values.
    x <- sjlabelled::set_labels(x, labels = val.labs[order(order(neword))])
  }

  x
}
