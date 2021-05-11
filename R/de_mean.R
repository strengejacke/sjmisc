#' @title Compute group-meaned and de-meaned variables
#' @name de_mean
#'
#' @description \code{de_mean()} computes group- and de-meaned versions of a
#'    variable that can be used in regression analysis to model the between-
#'    and within-subject effect.
#'
#' @param x A data frame.
#' @param ... Names of variables that should be group- and de-meaned.
#' @param grp Quoted or unquoted name of the variable that indicates the
#'    group- or cluster-ID.
#' @param suffix.dm,suffix.gm String value, will be appended to the names of the
#'   group-meaned and de-meaned variables of \code{x}. By default, de-meaned
#'   variables will be suffixed with \code{"_dm"} and grouped-meaned variables
#'   with \code{"_gm"}.
#'
#' @inheritParams to_dummy
#' @inheritParams rec
#'
#' @return For \code{append = TRUE}, \code{x} including the group-/de-meaned
#'   variables as new columns is returned; if \code{append = FALSE}, only the
#'   group-/de-meaned variables will be returned.
#'
#' @details \code{de_mean()} is intended to create group- and de-meaned variables
#'    for complex random-effect-within-between models (see \cite{Bell et al. 2018}),
#'    where group-effects (random effects) and fixed effects correlate (see
#'    \cite{Bafumi and Gelman 2006)}). This violation of one of the
#'    \emph{Gauss-Markov-assumptions} can happen, for instance, when analysing panel
#'    data. To control for correlating predictors and group effects, it is
#'    recommended to include the group-meaned and de-meaned version of
#'    \emph{time-varying covariates} in the model. By this, one can fit
#'    complex multilevel models for panel data, including time-varying,
#'    time-invariant predictors and random effects. This approach is superior to
#'    simple fixed-effects models, which lack information of variation in the
#'    group-effects or between-subject effects.
#'    \cr \cr
#'    A description of how to translate the
#'    formulas described in \emph{Bell et al. 2018} into R using \code{lmer()}
#'    from \pkg{lme4} or \code{glmmTMB()} from \pkg{glmmTMB} can be found here:
#'    \href{https://strengejacke.github.io/mixed-models-snippets/random-effects-within-between-effects-model.html}{for lmer()}
#'    and \href{https://strengejacke.github.io/mixed-models-snippets/random-effects-within-between-effects-model-glmmtmb.html}{for glmmTMB()}.
#'
#' @references
#'   Bafumi J, Gelman A. 2006. Fitting Multilevel Models When Predictors and Group Effects Correlate. In. Philadelphia, PA: Annual meeting of the American Political Science Association.
#'   \cr \cr
#'   Bell A, Fairbrother M, Jones K. 2018. Fixed and Random Effects Models: Making an Informed Choice. Quality & Quantity. \doi{10.1007/s11135-018-0802-x}
#'
#' @examples
#' data(efc)
#' efc$ID <- sample(1:4, nrow(efc), replace = TRUE) # fake-ID
#' de_mean(efc, c12hour, barthtot, grp = ID, append = FALSE)
#' @export
de_mean <- function(x, ..., grp, append = TRUE, suffix.dm = "_dm", suffix.gm = "_gm") {
  group_name <- rlang::quo_name(rlang::enquo(grp))
  group_var <- rlang::as_quosure(rlang::sym(group_name), env = rlang::global_env())
  group_ids <- c(group_name, ".dummyid")

  # evaluate arguments, generate data
  dat <- get_dot_data(x, dplyr::quos(...))

  dat <- x %>%
    dplyr::ungroup() %>%
    dplyr::select(!! group_name) %>%
    add_columns(dat) %>%
    dplyr::mutate(.dummyid = 1:nrow(dat)) %>%
    dplyr::arrange(!! group_var)

  cn <- colnames(dat)[1:(ncol(dat) - 2)]

  x_gm <- dat %>%
    dplyr::group_by(!! group_var) %>%
    dplyr::mutate_at(cn, mean, na.rm = TRUE) %>%
    dplyr::ungroup()

  x_dm_list <- purrr::map(cn, ~ dat[[.x]] - x_gm[[.x]])
  names(x_dm_list) <- cn

  x_dm <- as.data.frame(x_dm_list)
  colnames(x_dm) <- sprintf("%s%s", colnames(x_dm), suffix.dm)

  renamers <- which(colnames(x_gm) %in% cn)
  colnames(x_gm)[renamers] <- sprintf("%s%s", colnames(x_gm)[renamers], suffix.gm)

  dat <- dat %>%
    dplyr::select(!! -group_ids) %>%
    dplyr::bind_cols(x_dm, x_gm) %>%
    dplyr::arrange(rlang::.data$.dummyid) %>%
    dplyr::select(!! -group_ids)

  if (append)
    dat <- add_columns(dat, x)

  dat
}
