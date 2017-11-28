#' @title Standardize and center variables
#' @name std
#' @description \code{std()} computes a z-transformation (standardized and centered)
#'              on the input. \code{center()} centers the input.
#'
#' @param include.fac Logical, if \code{TRUE}, factors will be converted to numeric
#'   vectors and also standardized or centered.
#' @param robust Character vector, indicating the method applied when
#'   standardizing variables with \code{std()}. By default, standardization is
#'   achieved by dividing the centered variables by their standard deviation
#'   (\code{robust = "sd"}). However, for skewed distributions, the median
#'   absolute deviation (MAD, \code{robust = "mad"}) or Gini's mean difference
#'   (\code{robust = "gmd"}) might be more robust measures of dispersion. For
#'   the latter option, \CRANpkg{sjstats} needs to be installed.
#'
#' @inheritParams to_factor
#' @inheritParams rec
#'
#' @return A vector with standardized or centered variables. If \code{x} is a
#'         data frame, only the transformed variables will be returned.
#'
#' @note \code{std()} and \code{center()} only return a vector, if \code{x} is
#'         a vector. If \code{x} is a data frame and only one variable is specified
#'         in the \code{...}-ellipses argument, both functions do return a
#'         data frame (see 'Examples').
#'
#' @details \code{std()} and \code{center()} also work on grouped data frames
#'          (see \code{\link[dplyr]{group_by}}). In this case, standardization
#'          or centering is applied to the subsets of variables in \code{x}.
#'          See 'Examples'.
#'
#' @examples
#' data(efc)
#' std(efc$c160age) %>% head()
#' std(efc, e17age, c160age) %>% head()
#'
#' center(efc$c160age) %>% head()
#' center(efc, e17age, c160age) %>% head()
#'
#' # NOTE!
#' std(efc$e17age) # returns a vector
#' std(efc, e17age) # returns a tibble
#'
#' # works with mutate()
#' library(dplyr)
#' efc %>%
#'   select(e17age, neg_c_7) %>%
#'   mutate(age_std = std(e17age), burden = center(neg_c_7)) %>%
#'   head()
#'
#' # works also with grouped data frames
#' mtcars %>% std(disp)
#'
#' mtcars %>%
#'   group_by(cyl) %>%
#'   std(disp)
#'
#' data(iris)
#' # also standardize factors
#' std(iris, include.fac = TRUE)
#' # don't standardize factors
#' std(iris, include.fac = FALSE)
#'
#' @importFrom dplyr quos
#' @export
std <- function(x, ..., robust = c("sd", "gmd", "mad"), include.fac = FALSE, append = FALSE, suffix = "_z") {
  # evaluate arguments, generate data
  .dat <- get_dot_data(x, dplyr::quos(...))

  # match arguments
  robust <- match.arg(robust)

  std_and_center(x, .dat, include.fac, append, standardize = TRUE, robust = robust, suffix)
}


#' @rdname std
#' @export
center <- function(x, ..., include.fac = FALSE, append = FALSE, suffix = "_c") {
  # evaluate arguments, generate data
  .dat <- get_dot_data(x, dplyr::quos(...))

  std_and_center(x, .dat, include.fac, append, standardize = FALSE, robust = NULL, suffix)
}


std_and_center <- function(x, .dat, include.fac, append, standardize, robust, suffix) {
  recode_fun(
    x = x,
    .dat = .dat,
    fun = get("std_helper", asNamespace("sjmisc")),
    suffix = suffix,
    append = append,
    include.fac = include.fac,
    standardize = standardize,
    robust = robust
  )
}


#' @importFrom stats sd na.omit mad
#' @importFrom sjlabelled get_label set_label
std_helper <- function(x, include.fac, standardize, robust) {
  # check whether factors should also be standardized
  if (is.factor(x)) {
    if (include.fac)
      x <- to_value(x, keep.labels = FALSE)
    else
      return(x)
  }

  # non-numeric are preserved.
  if (!is.numeric(x)) return(x)

  # remove missings
  tmp <- stats::na.omit(x)

  # save value label, if any
  lab <- sjlabelled::get_label(x)

  # now center and standardize
  tmp <- tmp - mean(tmp)


  # standardization can be achieved by std. dev., MAD or Gini's MD

  if (standardize) {
    if (robust == "mad")
      tmp <- tmp / stats::mad(tmp)
    else if (robust == "gmd" && requireNamespace("sjstats", quietly = TRUE))
      tmp <- tmp / sjstats::gmd(tmp)
    else
      tmp <- tmp / stats::sd(tmp)
  }


  # and fill in values in original vector
  x[!is.na(x)] <- tmp

  # add back label
  sjlabelled::set_label(x, label = lab)
}
