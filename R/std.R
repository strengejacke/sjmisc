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


#' @importFrom purrr map_df
#' @importFrom tibble as_tibble
#' @importFrom tidyr unnest
#' @importFrom dplyr group_indices group_vars slice ungroup
std_and_center <- function(x, .dat, include.fac, append, standardize, suffix) {
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
          group[[j]] <- std_helper(
            x = group[[j]],
            include.fac = include.fac,
            standardize = standardize
          )
        }

        # write back data
        .dat[keep, ] <- group
      }

      # remove grouping column
      x <- tibble::as_tibble(.dat[colnames(.dat) %nin% grp.vars])

    } else {
      # iterate variables of data frame
      for (i in colnames(.dat)) {
        x[[i]] <- std_helper(
          x = .dat[[i]],
          include.fac = include.fac,
          standardize = standardize
        )
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
    x <- std_helper(
      x = .dat,
      include.fac = include.fac,
      standardize = standardize
    )
  }

  x
}


#' @importFrom stats sd na.omit
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
