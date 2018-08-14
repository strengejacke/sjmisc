#' @title Spread model coefficients of list-variables into columns
#' @name spread_coef
#'
#' @description This function extracts coefficients (and standard error and
#'              p-values) of fitted model objects from (nested) data frames,
#'              which are saved in a list-variable, and spreads the coefficients
#'              into new colummns.
#'
#' @param data A (nested) data frame with a list-variable that contains fitted
#'          model objects (see 'Details').
#' @param model.column Name or index of the list-variable that contains the
#'          fitted model objects.
#' @param model.term Optional, name of a model term. If specified, only this model
#'          term (including p-value) will be extracted from each model and
#'          added as new column.
#' @param se Logical, if \code{TRUE}, standard errors for estimates will also be extracted.
#' @param p.val Logical, if \code{TRUE}, p-values for estimates will also be extracted.
#' @param append Logical, if \code{TRUE} (default), this function returns
#'          \code{data} with new columns for the model coefficients; else,
#'          a new data frame with model coefficients only are returned.
#' @param ... Other arguments passed down to the \code{\link[broom]{tidy}}-function.
#'
#' @return A data frame with columns for each coefficient of the models
#'           that are stored in the list-variable of \code{data}; or, if
#'           \code{model.term} is given, a data frame with the term's estimate.
#'           If \code{se = TRUE} or \code{p.val = TRUE}, the returned data frame
#'           also contains columns for the coefficients' standard error and
#'           p-value.
#'           If \code{append = TRUE}, the columns are appended to \code{data},
#'           i.e. \code{data} is also returned.
#'
#' @details This function requires a (nested) data frame (e.g. created by the
#'            \code{\link[tidyr]{nest}}-function of the \pkg{tidyr}-package),
#'            where several fitted models are saved in a list-variable (see
#'            'Examples'). Since nested data frames with fitted models stored as list-variable
#'            are typically fit with an identical formula, all models have the same
#'            dependent and independent variables and only differ in their
#'            subsets of data. The function then extracts all coefficients from
#'            each model and saves each estimate in a new column. The result
#'            is a data frame, where each \emph{row} is a model with each
#'            model's coefficients in an own \emph{column}.
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' library(purrr)
#' data(efc)
#'
#' # create nested data frame, grouped by dependency (e42dep)
#' # and fit linear model for each group. These models are
#' # stored in the list variable "models".
#' model.data <- efc %>%
#'   filter(!is.na(e42dep)) %>%
#'   group_by(e42dep) %>%
#'   nest() %>%
#'   mutate(
#'     models = map(data, ~lm(neg_c_7 ~ c12hour + c172code, data = .x))
#'   )
#'
#' # spread coefficients, so we can easily access and compare the
#' # coefficients over all models. arguments `se` and `p.val` default
#' # to `FALSE`, when `model.term` is not specified
#' spread_coef(model.data, models)
#' spread_coef(model.data, models, se = TRUE)
#'
#' # select only specific model term. `se` and `p.val` default to `TRUE`
#' spread_coef(model.data, models, c12hour)
#'
#' # spread_coef can be used directly within a pipe-chain
#' efc %>%
#'   filter(!is.na(e42dep)) %>%
#'   group_by(e42dep) %>%
#'   nest() %>%
#'   mutate(
#'     models = map(data, ~lm(neg_c_7 ~ c12hour + c172code, data = .x))
#'   ) %>%
#'   spread_coef(models)
#'
#' # spread_coef() makes it easy to generate bootstrapped
#' # confidence intervals, using the 'bootstrap()' and 'boot_ci()'
#' # functions from the 'sjstats' package, which creates nested
#' # data frames of bootstrap replicates
#' library(sjstats)
#' efc %>%
#'   # generate bootstrap replicates
#'   bootstrap(100) %>%
#'   # apply lm to all bootstrapped data sets
#'   mutate(
#'     models = map(strap, ~lm(neg_c_7 ~ e42dep + c161sex + c172code, data = .x))
#'   ) %>%
#'   # spread model coefficient for all 100 models
#'   spread_coef(models, se = FALSE, p.val = FALSE) %>%
#'   # compute the CI for all bootstrapped model coefficients
#'   boot_ci(e42dep, c161sex, c172code)
#'
#' @importFrom broom tidy
#' @importFrom dplyr select bind_cols "%>%"
#' @importFrom tidyr spread
#' @importFrom purrr map_df
#' @importFrom rlang .data
#' @export
spread_coef <- function(data, model.column, model.term, se, p.val, append = TRUE, ...) {
  # check if we have a data frame
  if (!is.data.frame(data))
    stop("`data` needs to be a data frame.", call. = FALSE)

  # evaluate arguments
  model.column <- deparse(substitute(model.column))
  model.term <- deparse(substitute(model.term))

  # check if variable is a list variable
  if (!is.list(data[[model.column]]))
    stop(sprintf("%s needs to be a list-variable.", model.column), call. = FALSE)

  # check for proper defaults, depending on return style
  if (missing(se)) se <- !sjmisc::is_empty(model.term)
  if (missing(p.val)) p.val <- !sjmisc::is_empty(model.term)

  # check if user just wants a specific model term
  # if yes, select this, and its p-value
  if (!sjmisc::is_empty(model.term)) {
    # validate model term, i.e. check if coefficient exists in models
    tmp <- broom::tidy(data[[model.column]][[1]], effects = "fixed")

    # if term is no valid coefficient name, tell user, and make
    # suggestions of possibly meant correct terms
    if (model.term %nin% tmp$term) {

      pos <- str_pos(search.string = tmp$term, find.term = model.term, part.dist.match = 1)

      if (pos != -1) {
        pos_str <-
          sprintf(" Did you mean (one of) `%s`?", paste(tmp$term[pos], collapse = ","))
      } else {
        pos_str <- ""
      }

      stop(
        sprintf(
          "`%s` is no valid model term.%s",
          model.term,
          pos_str
        ),
        call. = F
      )
    }

    # select variables for output
    variables <- "estimate"
    if (se) variables <- c(variables, "std.error")
    if (p.val) variables <- c(variables, "p.value")

    # iterate list variable
    dat <-
      purrr::map_df(data[[model.column]], function(x) {
        # tidy model. for mixed effects, return fixed effects only
        tmp <- broom::tidy(x, effects = "fixed", ...) %>%
          # filter term
          dplyr::filter(.data$term == model.term) %>%
          # just select estimate and p-value
          dplyr::select(string_one_of(pattern = variables, x = colnames(.)))
        # set colnames
        colnames(tmp) <- c(model.term, variables[-1])
        tmp
      })
  } else {
    # iterate list variable
    dat <-
      purrr::map_df(data[[model.column]], function(x) {
        # tidy model. for mixed effects, return fixed effects only
        tmp <- broom::tidy(x, effects = "fixed", ...)

        # just select term name and estimate value
        df1 <- tmp %>%
          dplyr::select(.data$term, .data$estimate) %>%
          # spread to columns
          tidyr::spread(key = .data$term, value = .data$estimate)

        # columns for each data frame
        cols <- ncol(df1)

        # standard error also requested?
        if (se) {
          # just select term name and estimate value
          df2 <- tmp %>%
            dplyr::select(.data$term, .data$std.error) %>%
            # spread to columns
            tidyr::spread(key = .data$term, value = .data$std.error)
          # fix column names
          colnames(df2) <- sprintf("%s.se", colnames(df2))
          # bind together
          df1 <- dplyr::bind_cols(df1, df2)
        }

        # p-value also requested?
        if (p.val) {
          # just select term name and estimate value
          df3 <- tmp %>%
            dplyr::select(.data$term, .data$p.value) %>%
            # spread to columns
            tidyr::spread(key = .data$term, value = .data$p.value)
          # fix column names
          colnames(df3) <- sprintf("%s.p", colnames(df3))
          # bind together
          df1 <- dplyr::bind_cols(df1, df3)
        }

        # return sorted data frame
        df1[, unlist(lapply(1:cols, function(x) seq(from = 1, to = ncol(df1), by = cols) + x - 1))]

      })
  }
  # bind result to original data frame
  if (append)
    dplyr::bind_cols(data, dat)
  else
    dat
}
