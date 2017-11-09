#' @title Standardize and center variables
#' @name std
#' @description \code{std()} computes a z-transformation (standardized and centered)
#'              on the input. \code{center()} centers the input.
#'
#' @param include.fac Logical, if \code{TRUE}, factors will be converted to numeric
#'          vectors and also standardized or centered.
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
#' # works also with gouped data frames
#' mtcars %>% std(disp)
#'
#' mtcars %>%
#'   group_by(cyl) %>%
#'   std(disp)
#'
#' @importFrom dplyr quos
#' @export
std <- function(x, ..., include.fac = TRUE, append = FALSE, suffix = "_z") {
  # evaluate arguments, generate data
  .dat <- get_dot_data(x, dplyr::quos(...))

  std_and_center(x, .dat, include.fac, append, standardize = TRUE, suffix)
}


#' @rdname std
#' @export
center <- function(x, ..., include.fac = TRUE, append = FALSE, suffix = "_c") {
  # evaluate arguments, generate data
  .dat <- get_dot_data(x, dplyr::quos(...))

  std_and_center(x, .dat, include.fac, append, standardize = FALSE, suffix)
}


std_and_center <- function(x, .dat, include.fac, append, standardize, suffix) {
  recode_fun(
    x = x,
    .dat = .dat,
    fun = get("std_helper", asNamespace("sjmisc")),
    suffix = suffix,
    append = append,
    include.fac = include.fac,
    standardize = standardize
  )
}


#' @importFrom stats sd na.omit
#' @importFrom sjlabelled get_label set_label
std_helper <- function(x, include.fac, standardize) {
  # check whether factors should also be standardized
  if (is.factor(x)) {
    if (include.fac)
      x <- to_value(x, keep.labels = FALSE)
    else
      return(x)
  }

  # remove missings
  tmp <- stats::na.omit(x)

  # save value label, if any
  lab <- sjlabelled::get_label(x)

  # now center and standardize
  tmp <- tmp - mean(tmp)
  if (standardize) tmp <- tmp / stats::sd(tmp)

  # and fill in values in original vector
  x[!is.na(x)] <- tmp

  # add back label
  sjlabelled::set_label(x, label = lab)
}
