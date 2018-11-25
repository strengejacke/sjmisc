#' @title Standardize and center variables
#' @name std
#'
#' @description \code{std()} computes a z-transformation (standardized and centered)
#'   on the input. \code{center()} centers the input. \code{std_if()} and
#'   \code{center_if()} are scoped variants of \code{std()} and \code{center()},
#'   where transformation will be applied only to those variables that match the
#'   logical condition of \code{predicate}.
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
#'   \code{robust = "2sd"} divides the centered variables by two standard
#'   deviations, following a suggestion by \emph{Gelman (2008)}, so the
#'   rescaled input is comparable to binary variables.
#'
#' @inheritParams to_factor
#' @inheritParams rec
#'
#' @return If \code{x} is a vector, returns a vector with standardized or
#'   centered variables. If \code{x} is a data frame, for \code{append = TRUE},
#'   \code{x} including the transformed variables as new columns is returned;
#'   if \code{append = FALSE}, only the transformed variables will be returned.
#'   If \code{append = TRUE} and \code{suffix = ""}, recoded variables will
#'   replace (overwrite) existing variables.
#'
#' @note \code{std()} and \code{center()} only return a vector, if \code{x} is
#'   a vector. If \code{x} is a data frame and only one variable is specified
#'   in the \code{...}-ellipses argument, both functions do return a
#'   data frame (see 'Examples').
#'
#' @details \code{std()} and \code{center()} also work on grouped data frames
#'   (see \code{\link[dplyr]{group_by}}). In this case, standardization
#'   or centering is applied to the subsets of variables in \code{x}.
#'   See 'Examples'.
#'   \cr \cr
#'   For more complicated models with many predictors, Gelman and Hill (2007)
#'   suggest leaving binary inputs as is and only standardize continuous predictors
#'   by dividing by two standard deviations. This ensures a rough comparability
#'   in the coefficients.
#'
#' @references
#'   Gelman A (2008) Scaling regression inputs by dividing by two
#'   standard deviations. \emph{Statistics in Medicine 27: 2865–2873.}
#'   \url{http://www.stat.columbia.edu/~gelman/research/published/standardizing7.pdf}
#'   \cr \cr
#'   Gelman A, Hill J (2007) Data Analysis Using Regression and Multilevel/Hierarchical
#'   Models. Cambdridge, Cambdrige University Press: 55-57
#'
#' @examples
#' data(efc)
#' std(efc$c160age) %>% head()
#' std(efc, e17age, c160age, append = FALSE) %>% head()
#'
#' center(efc$c160age) %>% head()
#' center(efc, e17age, c160age, append = FALSE) %>% head()
#'
#' # NOTE!
#' std(efc$e17age) # returns a vector
#' std(efc, e17age) # returns a data frame
#'
#' # with quasi-quotation
#' x <- "e17age"
#' center(efc, !!x, append = FALSE) %>% head()
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
#' # compare new column "disp_z" w/ output above
#' mtcars %>%
#'   group_by(cyl) %>%
#'   std(disp)
#'
#' data(iris)
#' # also standardize factors
#' std(iris, include.fac = TRUE, append = FALSE)
#' # don't standardize factors
#' std(iris, include.fac = FALSE, append = FALSE)
#'
#' # standardize only variables with more than 10 unique values
#' p <- function(x) dplyr::n_distinct(x) > 10
#' std_if(efc, predicate = p, append = FALSE)
#'
#' @export
std <- function(x, ..., robust = c("sd", "2sd", "gmd", "mad"), include.fac = FALSE, append = TRUE, suffix = "_z") {
  UseMethod("std")
}


#' @importFrom dplyr quos
#' @export
std.default <- function(x, ..., robust = c("sd", "2sd", "gmd", "mad"), include.fac = FALSE, append = TRUE, suffix = "_z") {
  # evaluate arguments, generate data
  .dat <- get_dot_data(x, dplyr::quos(...))

  # match arguments
  robust <- match.arg(robust)

  std_and_center(x, .dat, include.fac, append, standardize = TRUE, robust = robust, suffix)
}


#' @importFrom dplyr bind_cols select quos
#' @importFrom purrr map
#' @export
std.mids <- function(x, ..., robust = c("sd", "2sd", "gmd", "mad"), include.fac = FALSE, append = TRUE, suffix = "_z") {
  vars <- dplyr::quos(...)
  ndf <- prepare_mids_recode(x)

  # select variable and compute rowsums. add this variable
  # to each imputed

  ndf$data <- purrr::map(
    ndf$data,
    function(.x) {
      dat <- dplyr::select(.x, !!! vars)
      dplyr::bind_cols(
        .x,
        std_and_center(
          x = dat,
          .dat = dat,
          include.fac = include.fac,
          append = FALSE,
          standardize = TRUE,
          robust = robust,
          suffix = suffix
        )
      )
    }
  )

  final_mids_recode(ndf)
}


#' @importFrom dplyr select_if
#' @rdname std
#' @export
std_if <- function(x, predicate, robust = c("sd", "2sd", "gmd", "mad"), include.fac = FALSE, append = TRUE, suffix = "_z") {

  # select variables that match logical conditions
  .dat <- dplyr::select_if(x, .predicate = predicate)

  # if no variable matches the condition specified
  # in predicate, return original data

  if (sjmisc::is_empty(.dat)) {
    if (append)
      return(x)
    else
      return(.dat)
  }

  # match arguments
  robust <- match.arg(robust)

  std_and_center(x, .dat, include.fac, append, standardize = TRUE, robust = robust, suffix)
}


#' @rdname std
#' @export
center <- function(x, ..., include.fac = FALSE, append = TRUE, suffix = "_c") {
  UseMethod("center")
}


#' @importFrom dplyr quos
#' @export
center.default <- function(x, ..., include.fac = FALSE, append = TRUE, suffix = "_c") {
  # evaluate arguments, generate data
  .dat <- get_dot_data(x, dplyr::quos(...))

  std_and_center(x, .dat, include.fac, append, standardize = FALSE, robust = NULL, suffix)
}


#' @importFrom dplyr bind_cols select quos
#' @importFrom purrr map
#' @export
center.mids <- function(x, ..., robust = c("sd", "2sd", "gmd", "mad"), include.fac = FALSE, append = TRUE, suffix = "_z") {
  vars <- dplyr::quos(...)
  ndf <- prepare_mids_recode(x)

  # select variable and compute rowsums. add this variable
  # to each imputed

  ndf$data <- purrr::map(
    ndf$data,
    function(.x) {
      dat <- dplyr::select(.x, !!! vars)
      dplyr::bind_cols(
        .x,
        std_and_center(
          x = dat,
          .dat = dat,
          include.fac = include.fac,
          append = FALSE,
          standardize = FALSE,
          robust = robust,
          suffix = suffix
        )
      )
    }
  )

  final_mids_recode(ndf)
}


#' @importFrom dplyr select_if
#' @rdname std
#' @export
center_if <- function(x, predicate, include.fac = FALSE, append = TRUE, suffix = "_c") {

  # select variables that match logical conditions
  .dat <- dplyr::select_if(x, .predicate = predicate)

  # if no variable matches the condition specified
  # in predicate, return original data

  if (sjmisc::is_empty(.dat)) {
    if (append)
      return(x)
    else
      return(.dat)
  }

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
      x <- sjlabelled::as_numeric(x, keep.labels = FALSE)
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
    else if (robust == "2sd")
      tmp <- tmp / (2 * stats::sd(tmp))
    else
      tmp <- tmp / stats::sd(tmp)
  }


  # and fill in values in original vector
  x[!is.na(x)] <- tmp

  # add back label
  sjlabelled::set_label(x, label = lab)
}
