#' @title Spread model coefficients of nested data frames into columns
#' @name spread_coef
#'
#' @description This function extracts the coefficients of fitted model objects
#'              from nested data frames and spreads the coefficients into
#'              new colummns.
#'
#' @param data A nested data frame with a list-variable that contains fitted
#'          model objects (see 'Details').
#' @param model.column Name or index of the list-variable that contains the
#'          fitted model objects.
#' @param model.term Optional, name of a model term. If specified, only this model
#'          term (including p-value) will be extracted from each model and
#'          added as new column.
#' @param ... Other arguments passed down to the \code{\link[broom]{tidy}}-function.
#'
#' @return \code{data}, with new columns for each coefficients of the models
#'           that are stored in the list-variable; or, if \code{model.term} is given,
#'           with two new columns (one for the term's estimate value and one
#'           for the related p-value).
#'
#' @details This function requires a nested data frame, as created by the
#'            \code{\link[tidyr]{nest}}-function of the \pkg{tidyr}-package,
#'            where several fitted models are saved in a list-variable (see
#'            'Examples'). As nested data frames with fitted models as list-variable
#'            are typically fit with an identical formula, all models have the same
#'            dependent and independent variables and only differ in their
#'            subsets of data. The function then extracts all coefficients from
#'            each model and saves each estimate in a new column. The result
#'            is a data frame, where each \emph{row} is a model with each
#'            each model's \emph{coefficient} in an own column.
#'
#' @examples
#' library(tidyr)
#' library(dplyr)
#' data(efc)
#'
#' # create nested data frame, grouped by dependency (e42dep)
#' # and fit linear model for each group. These models are
#' # stored in the list variable "models".
#' model.data <- efc %>%
#'   filter(!is.na(e42dep)) %>%
#'   group_by(e42dep) %>%
#'   nest() %>%
#'   mutate(models = lapply(data, function(x) {
#'     lm(neg_c_7 ~ c12hour + c172code, data = x)
#'   }))
#'
#' # spread coefficients, so we can easily access and compare
#' # the coefficients over all models
#' spread_coef(model.data, models)
#'
#' # select only specific model term
#' spread_coef(model.data, models, c12hour)
#'
#' # spread_coef can be used directly within a pipe-chain
#' efc %>%
#'   filter(!is.na(e42dep)) %>%
#'   group_by(e42dep) %>%
#'   nest() %>%
#'   mutate(models = lapply(data, function(x) {
#'     lm(neg_c_7 ~ c12hour + c172code, data = x)
#'   })) %>%
#'   spread_coef(models)
#'
#' @importFrom broom tidy
#' @importFrom dplyr select_ bind_cols
#' @importFrom tidyr spread_
#' @importFrom magrittr "%>%"
#' @export
spread_coef <- function(data, model.column, model.term, ...) {
  # check if we have a data frame
  if (!is.data.frame(data))
    stop("`data` needs to be a data frame.", call. = FALSE)

  # evaluate arguments
  model.column <- deparse(substitute(model.column))
  model.term <- deparse(substitute(model.term))

  # check if variable is a list variable
  if (!is.list(data[[model.column]]))
    stop(sprintf("%s needs to be a list-variable.", model.column), call. = FALSE)

  # check if user just wants a specific model term
  # if yes, select this, and its p-value
  if (!is_empty(model.term)) {
    # iterate list variable
    coef.data <-
      lapply(data[[model.column]], function(x) {
        # tidy model. for mixed effects, return fixed effects only
        tmp <- broom::tidy(x, effects = "fixed", ...) %>%
          # filter term
          dplyr::filter(term == model.term) %>%
          # just select estimate and p-value
          dplyr::select_("estimate", "p.value")
        # set colnames
        colnames(tmp) <- c(model.term, "p.value")
        tmp
      })
  } else {
    # iterate list variable
    coef.data <-
      lapply(data[[model.column]], function(x) {
        # tidy model. for mixed effects, return fixed effects only
        broom::tidy(x, effects = "fixed", ...) %>%
          # just select term name and estimate value
          dplyr::select_("term", "estimate") %>%
          # spread to columns
          tidyr::spread_(key_col = "term", value_col = "estimate")
      })
  }

  # copy all coefficient columns to data frame
  dat <- do.call(rbind, coef.data)
  # bind result to original data frame
  dplyr::bind_cols(data, dat)
}
