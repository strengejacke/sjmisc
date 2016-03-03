#' @title Intraclass-Correlation Coefficient
#' @name icc
#' @description This function calculates the intraclass-correlation
#'                (icc) for random intercepts of mixed effects models.
#'                Currently, only \code{\link[lme4]{merMod}} objects
#'                are supported.
#'
#' @param x Fitted mixed effects model (\code{\link[lme4]{merMod}}-class).
#' @param ... More fitted model objects, to compute multiple intraclass-correlation
#'              coefficients at once.
#'
#' @return A numeric vector with all random intercept intraclass-correlation-coefficients,
#'           or a list of numeric vectors, when more than one model were used
#'           as arguments.
#'
#' @references \itemize{
#'               \item Wu S, Crespi CM, Wong WK (2012) Comparison of methods for estimating the intraclass correlation coefficient for binary responses in cancer prevention cluster randomized trials. Contempory Clinical Trials 33: 869-880 (\href{http://dx.doi.org/10.1016/j.cct.2012.05.004}{doi:10.1016/j.cct.2012.05.004})
#'               \item \href{http://stats.stackexchange.com/questions/18088/intraclass-correlation-icc-for-an-interaction/28100#28100}{CrossValidated (2012) \emph{Intraclass correlation (ICC) for an interaction?}}
#'               \item \href{http://stats.stackexchange.com/questions/113577/interpreting-the-random-effect-in-a-mixed-effect-model/113825#113825}{CrossValidated (2014) \emph{Interpreting the random effect in a mixed-effect model}}
#'               \item \href{http://stats.stackexchange.com/questions/67247/how-to-partition-the-variance-explained-at-group-level-and-individual-level/67356#67356}{CrossValidated (2014) \emph{how to partition the variance explained at group level and individual level}}
#'             }
#'
#' @note \emph{Why ICC is useful}
#'       \enumerate{
#'        \item It can help you determine whether or not a linear mixed model is even necessary. If you find that the correlation is zero, that means the observations within clusters are no more similar than observations from different clusters.  Go ahead and use a simpler analysis technique.
#'        \item It can be theoretically meaningful to understand how much of the overall variation in the response is explained simply by clustering.  For example, in a repeated measures psychological study you can tell to what extent mood is a trait (varies among people, but not within a person on different occasions) or state (varies little on average among people, but varies a lot across occasions).
#'        \item It can also be meaningful to see how the ICC (as well as the between and within cluster variances) changes as variable are added to the model.
#'       }
#'       (Grace-Martin K: \emph{The Intraclass Correlation Coefficient in Mixed Models}, \href{http://www.theanalysisfactor.com/the-intraclass-correlation-coefficient-in-mixed-models/}{web})
#'       \cr \cr
#'       The calculation of the ICC for generalized linear mixed models is based on
#'       Wu et al. (2012).
#'
#' @examples
#' \dontrun{
#' library(lme4)
#' fit1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#' icc(fit1)
#'
#' sleepstudy$mygrp <- sample(1:45, size = 180, replace = TRUE)
#' fit2 <- lmer(Reaction ~ Days + (1 | mygrp) + (Days | Subject), sleepstudy)
#' icc(fit2)
#'
#' # return icc for all models at once
#' icc(fit1, fit2)}
#'
#'
#' @importFrom stats family
#' @export
icc <- function(x, ...) {
  # return value
  icc_ <- icc.lme4(x)
  # check if we have multiple parameters
  if (nargs() > 1) {
    # get input list
    params_ <- list(...)
    icc_ <- list(icc_)
    for (p_ in params_) {
      icc_[[length(icc_) + 1]] <- icc.lme4(p_)
    }
    names(icc_) <- NULL
  }
  return(icc_)
}

#' @importFrom lme4 VarCorr fixef getME
icc.lme4 <- function(fit) {
  # ------------------------
  # check if suggested package is available
  # ------------------------
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' needed for this function to work. Please install it.", call. = FALSE)
  }
  # ------------------------
  # check object class
  # ------------------------
  if (any(class(fit) == "glmerMod") || any(class(fit) == "lmerMod") || any(class(fit) == "merModLmerTest")) {
    # ------------------------
    # get family
    # ------------------------
    fitfam <- stats::family(fit)$family
    # is neg. binomoal?
    is_negbin <- str_contains(fitfam, "Negative Binomial", ignore.case = TRUE)
    # ------------------------
    # random effects variances
    # ------------------------
    reva <- summary(fit)$varcor
    # retrieve only intercepts
    vars <- lapply(reva, function(x) x[[1]])
    # intercept-variance
    sigma_a2 <- sapply(vars, function(x) x[1])
    # residual variances
    if (any(class(fit) == "glmerMod") && fitfam == "binomial") {
      # for logistic models, we use pi / 3
      resid_var <- (pi ^ 2) / 3
    } else if (any(class(fit) == "glmerMod") && is_negbin) {
      # for negative binomial models, we use 0
      resid_var <- 0
    } else {
      # for linear models, we have a clear
      # residual variance
      resid_var <- attr(reva, "sc") ^ 2
    }
    # total variance
    total_var <- sum(sapply(vars, sum), resid_var)
    # check whether we have negative binomial
    if (is_negbin) {
      beta <- as.numeric(lme4::fixef(fit)["(Intercept)"])
      r <- lme4::getME(fit, "glmer.nb.theta")
      ri.icc <- (exp(sigma_a2) - 1) / ((exp(total_var) - 1) + (exp(total_var) / r) + (exp(-beta) - (total_var / 2)))
    } else if (fitfam == "poisson") {
      ri.icc <- sigma_a2 / (1 + total_var)
    } else {
      # random intercept icc
      ri.icc <- sigma_a2 / total_var
      # icc standard errors
      # ri.icc.se <- unlist(lapply(reva, function(x) attr(x, "stddev")[1]))
      # names(ri.icc.se) <- paste0(names(reva), " (S.E.)")
      # return(list(icc = ri.icc, se = ri.icc.se))
    }
    # name values
    names(ri.icc) <- names(reva)
    # return results
    return(ri.icc)
  } else {
    warning("Function 'icc' currently only supports 'merMod' objects (package 'lme4').", call. = F)
  }
}

