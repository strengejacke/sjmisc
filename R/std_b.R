#' @title Standardized Beta coefficients and CI of lm and mixed models
#' @name std_beta
#' @description Returns the standardized beta coefficients and confidence intervals
#'                of a fitted linear (mixed) models, i.e. \code{fit} must either
#'                be of class \code{lm} or \code{\link[lme4]{merMod}}.
#'
#' @param fit Fitted linear (mixed) model of class \code{\link{lm}} or
#'          \code{\link[lme4]{merMod}} (\pkg{lme4} package).
#' @param include.ci Logical, if \code{TRUE}, a data frame with confidence
#'          intervals will be returned, when \code{fit} is of class \code{lm}.
#'          If \code{fit} is a \code{lmerMod} object (\pkg{lme4} package),
#'          always returns standard error instead of confidence intervals
#'          (hence, this parameter will be ignored when \code{fit} is a
#'          \code{lmerMod} object).
#' @param type If \code{fit} is of class \code{lm}, normal standardized coefficients
#'          are computed by default. Use \code{type = "std2"} to follow
#'          \href{http://www.stat.columbia.edu/~gelman/research/published/standardizing7.pdf}{Gelman's (2008)}
#'          suggestion, rescaling the estimates by deviding them by two standard
#'          deviations, so resulting coefficients are directly comparable for
#'          untransformed binary predictors.
#' @return A vector with standardized beta coefficients of the fitted linear model, or a data frame
#'           with standardized beta coefficients and confidence intervals, if
#'           \code{include.ci = TRUE}.
#'
#' @note "Standardized coefficients refer to how many standard deviations a dependent variable will change,
#'         per standard deviation increase in the predictor variable. Standardization of the coefficient is
#'         usually done to answer the question of which of the independent variables have a greater effect
#'         on the dependent variable in a multiple regression analysis, when the variables are measured
#'         in different units of measurement (for example, income measured in dollars and family size
#'         measured in number of individuals)." (Source: Wikipedia)
#'
#' @references \itemize{
#'              \item \href{http://en.wikipedia.org/wiki/Standardized_coefficient}{Wikipedia: Standardized coefficient}
#'              \item Gelman A (2008) "Scaling regression inputs by dividing by two standard deviations." \emph{Statistics in Medicine 27: 2865–2873.} \url{http://www.stat.columbia.edu/~gelman/research/published/standardizing7.pdf}
#'              }
#'
#' @examples
#' # fit linear model
#' fit <- lm(Ozone ~ Wind + Temp + Solar.R, data = airquality)
#' # print std. beta coefficients
#' std_beta(fit)
#'
#' # print std. beta coefficients and ci
#' std_beta(fit, include.ci = TRUE)
#'
#' # print std. beta coefficients and ci, using
#' # 2 sd and center binary predictors
#' std_beta(fit, include.ci = TRUE, type = "std2")
#'
#' @export
std_beta <- function(fit,
                     include.ci = FALSE,
                     type = "std") {
  # if we have merMod object (lme4), we need
  # other function to compute std. beta
  if (any(class(fit) == "lmerMod") || any(class(fit) == "merModLmerTest")) {
    return(sjs.stdmm(fit))
  } else if (type == "std2") {
    # is package available?
    if (!requireNamespace("arm", quietly = TRUE)) {
      stop("Package 'arm' needed for computing this type of standardized estimates. Please install it.", call. = FALSE)
    }
    # get standardized model parameter
    stdbv2_all <- arm::standardize(fit)
    # get standardized estimates
    beta <- stats::coef(stdbv2_all)[-1]
    # get standardized se
    std2se <- summary(stdbv2_all)$coefficients[-1, 2]
    # check if confidence intervals should also be returned
    # if yes, create data frame with sb and ci
    if (include.ci) {
      return(data.frame(beta = beta,
                        ci.low = beta - std2se * 1.96,
                        ci.hi = beta + std2se * 1.96))
    } else {
      return(beta)
    }
  } else {
    b <- summary(fit)$coef[-1, 1]
    sx <- sapply(as.data.frame(model.matrix(fit))[-1], sd, na.rm = T)
    sy <- sapply(as.data.frame(fit$model)[1], sd, na.rm = T)
    beta <- b * sx / sy
    se <- summary(fit)$coef[-1, 2]
    beta.se <- se * sx / sy
    # check if confidence intervals should also be returned
    # if yes, create data frame with sb and ci
    if (include.ci) {
      return(data.frame(beta = beta,
                        ci.low = (beta - beta.se * 1.96),
                        ci.hi = (beta + beta.se * 1.96)))
    } else {
      return(beta)
    }
  }
}


#' @importFrom stats sd coef
sjs.stdmm <- function(fit) {
  # code from Ben Bolker, see
  # http://stackoverflow.com/a/26206119/2094622
  # ------------------------
  # check if suggested package is available
  # ------------------------
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' needed for this function to work. Please install it.", call. = FALSE)
  }
  sdy <- stats::sd(lme4::getME(fit, "y"))
  sdx <- apply(lme4::getME(fit, "X"), 2, sd)
  sc <- lme4::fixef(fit) * sdx / sdy
  se.fixef <- stats::coef(summary(fit))[, "Std. Error"]
  se <- se.fixef * sdx / sdy
  mydf <- data.frame(stdcoef = sc, stdse = se)
  rownames(mydf) <- names(lme4::fixef(fit))
  return(mydf)
}
