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
#'           as arguments. Furthermore, between- and within-group variances as well
#'           as random-slope variance are returned as attributes.
#'
#' @references \itemize{
#'               \item Aguinis H, Gottfredson RK, Culpepper SA. 2013. Best-Practice Recommendations for Estimating Cross-Level Interaction Effects Using Multilevel Modeling. Journal of Management 39(6): 1490–1528 (\doi{10.1177/0149206313478188})
#'               \item Aly SS, Zhao J, Li B, Jiang J. 2014. Reliability of environmental sampling culture results using the negative binomial intraclass correlation coefficient. Springerplus [Internet] 3. Available from: \url{http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3916583/}
#'               \item Stryhn H, Sanchez J, Morley P, Booker C, Dohoo IR. 2006. Interpretation of variance parameters in multilevel Poisson regression models. Proceedings of the 11th International Symposium on Veterinary Epidemiology and Economics, 2006 Available at \url{http://www.sciquest.org.nz/node/64294}
#'               \item Wu S, Crespi CM, Wong WK. 2012. Comparison of methods for estimating the intraclass correlation coefficient for binary responses in cancer prevention cluster randomized trials. Contempory Clinical Trials 33: 869-880 (\doi{10.1016/j.cct.2012.05.004})
#'               \item \href{http://stats.stackexchange.com/questions/18088/intraclass-correlation-icc-for-an-interaction/28100#28100}{CrossValidated (2012) \emph{Intraclass correlation (ICC) for an interaction?}}
#'               \item \href{http://stats.stackexchange.com/questions/113577/interpreting-the-random-effect-in-a-mixed-effect-model/113825#113825}{CrossValidated (2014) \emph{Interpreting the random effect in a mixed-effect model}}
#'               \item \href{http://stats.stackexchange.com/questions/67247/how-to-partition-the-variance-explained-at-group-level-and-individual-level/67356#67356}{CrossValidated (2014) \emph{how to partition the variance explained at group level and individual level}}
#'             }
#'
#' @details Some notes on why the ICC is useful, based on Grace-Martin K: \emph{The Intraclass Correlation Coefficient in Mixed Models}, \href{http://www.theanalysisfactor.com/the-intraclass-correlation-coefficient-in-mixed-models/}{web}:
#'       \itemize{
#'        \item It can help you determine whether or not a linear mixed model is even necessary. If you find that the correlation is zero, that means the observations within clusters are no more similar than observations from different clusters.  Go ahead and use a simpler analysis technique.
#'        \item It can be theoretically meaningful to understand how much of the overall variation in the response is explained simply by clustering.  For example, in a repeated measures psychological study you can tell to what extent mood is a trait (varies among people, but not within a person on different occasions) or state (varies little on average among people, but varies a lot across occasions).
#'        \item It can also be meaningful to see how the ICC (as well as the between and within cluster variances) changes as variable are added to the model.
#'       }
#'
#' @note The calculation of the ICC for generalized linear mixed models with binary outcome is based on
#'       Wu et al. (2012). For Poisson multilevel models, please refere to Stryhn et al. (2006). Aly et al. (2014)
#'       describe computation of ICC for negative binomial models.
#'       \cr \cr
#'       There is a \code{print}-method that prints the variance parameters using
#'       the \code{comp}-argument set to \code{"var"}: \code{print(x, comp = "var")}
#'       (see 'Examples').
#'       \cr \cr
#'       The random effect variances indicate the between- and within-group
#'         variances as well as random-slope variance and random-slope-intercept
#'         correlation are returned. The components are denoted as following:
#'         \itemize{
#'          \item Within-group variance: sigma_2
#'          \item Between-group-variance: tau.00
#'          \item Random-slope-variance: tau.11
#'          \item Random-Intercept-Slope-covariance: tau.01
#'          \item Random-Intercept-Slope-correlation: rho.01
#'         }
#'
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
#' icc(fit1, fit2)
#'
#' icc1 <- icc(fit1)
#' icc2 <- icc(fit2)
#'
#' print(icc1, comp = "var")
#' print(icc2, comp = "var")}
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
#' @importFrom stats family formula
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
    # for details on tau and sigma, see
    # Aguinis H, Gottfredson RK, Culpepper SA2013. Best-Practice Recommendations for Estimating Cross-Level Interaction Effects Using Multilevel Modeling. Journal of Management 39(6): 1490–1528. doi:10.1177/0149206313478188.
    # ------------------------
    reva <- lme4::VarCorr(fit)
    # retrieve only intercepts
    vars <- lapply(reva, function(x) x[[1]])
    # ------------------------
    # random intercept-variances, i.e.
    # between-subject-variance (tau 00)
    # ------------------------
    tau.00 <- sapply(vars, function(x) x[1])
    # ------------------------
    # random slope-variances (tau 11)
    # ------------------------
    tau.11 <- unlist(lapply(reva, function(x) diag(x)[-1]))
    # ------------------------
    # residual variances, i.e.
    # within-cluster-variance (sigma^2)
    # ------------------------
    if (any(class(fit) == "glmerMod") && fitfam == "binomial") {
      # for logistic models, we use pi / 3
      resid_var <- (pi ^ 2) / 3
    } else if (any(class(fit) == "glmerMod") && is_negbin) {
      # for negative binomial models, we use 0
      resid_var <- 0
    } else {
      # for linear and poisson models, we have a clear
      # residual variance
      resid_var <- attr(reva, "sc") ^ 2
    }
    # total variance, sum of random intercept and residual variances
    total_var <- sum(sapply(vars, sum), resid_var)
    # check whether we have negative binomial
    if (is_negbin) {
      # for negative binomial models, we also need the intercept...
      beta <- as.numeric(lme4::fixef(fit)["(Intercept)"])
      # ... and the theta value to compute the ICC
      r <- lme4::getME(fit, "glmer.nb.theta")
      ri.icc <-
        (exp(tau.00) - 1) /
        ((exp(total_var) - 1) + (exp(total_var) / r) + exp(-beta - (total_var / 2)))
    } else {
      # random intercept icc
      ri.icc <- tau.00 / total_var
    }
    # ----------------------------------
    # get random slope random intercep correlations
    # ----------------------------------
    # do we have any rnd slopes?
    has_rnd_slope <- unlist(lapply(reva, function(x) dim(attr(x, "correlation"))[1] > 1))
    tau.01 <- rho.01 <- NULL
    # get rnd slopes
    if (any(has_rnd_slope)) {
      rnd_slope <- reva[has_rnd_slope]
      # get slope-intercept-correlations
      cor_ <- lapply(rnd_slope, function(x) attr(x, "correlation")[1, 2])
      # get standard deviations, multiplied
      std_ <- lapply(rnd_slope, function(x) prod(attr(x, "stddev")))
      # bind to matrix
      tau.01 <- apply(as.matrix(cbind(unlist(cor_), unlist(std_))), MARGIN = 1, FUN = prod)
      rho.01 <- cor_
    }
    # name values
    names(ri.icc) <- names(reva)
    # add attributes, for print method
    class(ri.icc) <- c("icc.lme4", class(ri.icc))
    attr(ri.icc, "family") <- stats::family(fit)$family
    attr(ri.icc, "link") <- stats::family(fit)$link
    attr(ri.icc, "formula") <- stats::formula(fit)
    attr(ri.icc, "model") <- ifelse(isTRUE(any(class(fit) == "glmerMod")), "Generalized inear mixed model", "Linear mixed model")
    attr(ri.icc, "tau.00") <- tau.00
    attr(ri.icc, "tau.01") <- tau.01
    attr(ri.icc, "rho.01") <- rho.01
    attr(ri.icc, "tau.11") <- tau.11
    attr(ri.icc, "sigma_2") <- resid_var
    # return results
    return(ri.icc)
  } else {
    warning("Function `icc` currently only supports `merMod` objects (package `lme4`).", call. = F)
  }
}


#' @title Random effect variances
#' @name re_var
#' @description This function extracts the random effect variances as well as
#'                the random-intercept-slope-correlation of mixed effects models.
#'                Currently, only \code{\link[lme4]{merMod}} objects
#'                are supported.
#'
#' @param x Fitted mixed effects model (\code{\link[lme4]{merMod}}-class).
#'
#' @return \code{NULL}, as this is just a convenient \code{print}-wrapper for
#'           the variance components returned by the \code{\link{icc}} function.
#'
#' @references Aguinis H, Gottfredson RK, Culpepper SA. 2013. Best-Practice Recommendations for Estimating Cross-Level Interaction Effects Using Multilevel Modeling. Journal of Management 39(6): 1490–1528 (\doi{10.1177/0149206313478188})
#'
#' @seealso \code{\link{icc}}
#'
#' @examples
#' library(lme4)
#' fit1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#' re_var(fit1)
#'
#' sleepstudy$mygrp <- sample(1:45, size = 180, replace = TRUE)
#' fit2 <- lmer(Reaction ~ Days + (1 | mygrp) + (Days | Subject), sleepstudy)
#' re_var(fit2)
#'
#' @importFrom stats family
#' @export
re_var <- function(x) {
  # return value
  revar_ <- icc(x)
  print(revar_, comp = "var")
}
