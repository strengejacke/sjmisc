#' @title Nagelkerke's and Cox-Snell's Pseudo R-squared
#' @name pseudo_r2
#'
#' @description This method calculates Nagelkerke's and Cox-Snell's
#'                pseudo-r-squared-values of generalized linear models
#'                for binary data.
#'
#' @param x Fitted \code{\link{glm}} model.
#'
#' @return An object of class \code{pseudo_r2} with
#'           following values:
#'           \itemize{
#'            \item \code{CoxSnell} Cox-Snell's pseudo-r-squared-value
#'            \item \code{Nagelkerke} Nagelkerke's pseudo-r-squared-value
#'           }
#'
#' @seealso \code{\link{cod}} for Tjur's Coefficient of Discrimination.
#'
#' @examples
#' data(efc)
#'
#' # Pseudo-R-squared values
#' efc$services <- dicho(efc$tot_sc_e, "v", 0, as.num = TRUE)
#' fit <- glm(services ~ neg_c_7 + c161sex + e42dep,
#'            data = efc,
#'            family = binomial(link = "logit"))
#' pseudo_r2(fit)
#'
#' @export
pseudo_r2 <- function(x) {
  .Deprecated("r2", package = "sjmisc", msg = "`pseudo_r2` will be deprecated in future versions of `sjmisc`. Please use `r2` instead.")
  n <- nrow(x$model)
  CoxSnell <- 1 - exp((x$deviance - x$null) / n)
  Nagelkerke <- CoxSnell / (1 - exp(-x$null / n))
  names(CoxSnell) <- "CoxSnell"
  names(Nagelkerke) <- "Nagelkerke"
  return(structure(class = "sjmisc_r2", list(CoxSnell = CoxSnell, Nagelkerke = Nagelkerke)))
}


#' @title Tjur's Coefficient of Discrimination
#' @name cod
#'
#' @description This method calculates the Coefficient of Discrimination \code{D}
#'                for generalized linear (mixed) models for binary data. It is
#'                an alternative to other Pseudo-R-squared values
#'                like Nakelkerke's R2 or Cox-Snell R2.
#'
#' @param x Fitted \code{\link{glm}} or \code{\link[lme4]{glmer}} model.
#'
#' @return The \code{D} Coefficient of Discrimination, also known as
#'           Tjur's R-squared value.
#'
#' @note The Coefficient of Discrimination \code{D} can be read like any
#'         other (Pseudo-)R-squared value.
#'
#' @references Tjur T (2009) Coefficients of determination in logistic regression models -
#'               a new proposal: The coefficient of discrimination. The American Statistician,
#'               63(4): 366-372
#'
#' @seealso \code{\link{pseudo_r2}} for Nagelkerke's and Cox and Snell's pseudo
#'            r-squared coefficients.
#'
#' @examples
#' data(efc)
#'
#' # Tjur's R-squared value
#' efc$services <- dicho(efc$tot_sc_e, "v", 0, as.num = TRUE)
#' fit <- glm(services ~ neg_c_7 + c161sex + e42dep,
#'            data = efc,
#'            family = binomial(link = "logit"))
#' cod(fit)
#'
#' @importFrom stats predict predict.glm residuals
#' @export
cod <- function(x) {
  # ---------------------------------------
  # check for valid object class
  # ---------------------------------------
  if (!any(class(x) == "glmerMod") && !any(class(x) == "glm")) {
    stop("'x' must be an object of class 'glm' or 'glmerMod'.", call. = F)
  }
  # ---------------------------------------
  # mixed models (lme4)
  # ---------------------------------------
  if (any(class(x) == "glmerMod")) {
    # ---------------------------------------
    # check for package availability
    # ---------------------------------------
    if (!requireNamespace("lme4", quietly = TRUE)) {
      stop("Package 'lme4' needed for this function to work. Please install it.", call. = FALSE)
    }
    y <- lme4::getME(x, "y")
    pred <- stats::predict(x, type = "response", re.form = NULL)
  } else {
    y <- x$y
    pred <- stats::predict.glm(x, type = "response")
  }
  # delete pred for cases with missing residuals
  if (anyNA(stats::residuals(x))) pred <- pred[!is.na(stats::residuals(x))]

  categories <- unique(y)
  m1 <- mean(pred[which(y == categories[1])], na.rm = T)
  m2 <- mean(pred[which(y == categories[2])], na.rm = T)

  cod = abs(m2 - m1)
  names(cod) <- "D"

  return(structure(class = "sjmisc_r2", list(cod = cod)))
}



#' @title Compute R-squared of (generalized) linear (mixed) models
#' @name r2
#'
#' @description Compute R-squared values of linear (mixed) models, or
#'                pseudo-R-squared values for generalized linear (mixed) models.
#'
#' @param x Fitted model of class \code{lm}, \code{glm}, \code{lmerMod}/\code{lme}
#'            or \code{glmerMod}.
#'
#' @return \itemize{
#'           \item For linear models, the r-squared and adjusted r-squared values.
#'           \item For linear mixed models, the r-squared and Omega-squared values.
#'           \item For \code{glm} objects, Cox & Snell's and Nagelkerke's pseudo r-squared values.
#'           \item For \code{glmerMod} objects, Tjur's coefficient of determination.
#'         }
#'
#' @note For linear models, the r-squared and adjusted r-squared value is returned,
#'         as provided by the \code{summary}-function.
#'         \cr \cr
#'         For linear mixed models, an r-squared approximation by computing the
#'         correlation between the fitted and observed values, as suggested by
#'         Byrnes (2008), is returned as well as the Omega-squared value as
#'         suggested by Xu (2003).
#'         \cr \cr
#'         For generalized linear models, Cox & Snell's and Nagelkerke's
#'         pseudo r-squared values are returned.
#'         \cr \cr
#'         For generalized linear mixed models, the coefficient of determination
#'         as suggested by Tjur (2009) (see also \code{\link{cod}}).
#'
#' @references \itemize{
#'               \item \href{http://glmm.wikidot.com/faq}{DRAFT r-sig-mixed-models FAQ}
#'               \item Byrnes, J. 2008. Re: Coefficient of determination (R^2) when using lme(). \href{http://thread.gmane.org/gmane.comp.lang.r.lme4.devel/684}{gmane.comp.lang.r.lme4.devel}
#'               \item Xu, R. 2003. Measuring explained variation in linear mixed effects models. Statist. Med. 22:3527-3541. \url{doi:10.1002/sim.1572}
#'               \item Tjur T. 2009. Coefficients of determination in logistic regression models - a new proposal: The coefficient of discrimination. The American Statistician, 63(4): 366-372
#'             }
#'
#' @examples
#' library(lme4)
#' fit <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#' r2(fit)
#'
#' data(efc)
#' fit <- lm(barthtot ~ c160age + c12hour, data = efc)
#' r2(fit)
#'
#' # Pseudo-R-squared values
#' efc$services <- dicho(efc$tot_sc_e, "v", 0, as.num = TRUE)
#' fit <- glm(services ~ neg_c_7 + c161sex + e42dep,
#'            data = efc,
#'            family = binomial(link = "logit"))
#' r2(fit)
#'
#'
#' @importFrom stats model.response model.frame fitted var residuals
#' @export
r2 <- function(x) {
  rsq <- NULL
  osq <- NULL
  adjr2 <- NULL
  # do we have a glm? if so, report pseudo_r2
  if (any(class(x) == "glm")) {
    return(pseudo_r2(x))
    # do we have a glmer?
  } else if (any(class(x) == "glmerMod")) {
    return(cod(x))
    # do we have a simple linear model?
  } else if (identical(class(x), "lm")) {
    rsq <- summary(x)$r.squared
    adjr2 <- summary(x)$adj.r.squared
    # name vectors
    names(rsq) <- "R2"
    names(adjr2) <- "adj.R2"
    # return results
    return(structure(class = "sjmisc_r2", list(r2 = rsq, adjr2 = adjr2)))
    # else do we have a mixed model?
  } else if (str_contains(class(x),
                          pattern = c("lmerMod", "lme"),
                          ignore.case = T,
                          logic = "OR")) {
    # compute "correlation"
    lmfit <-  lm(stats::model.response(stats::model.frame(x)) ~ stats::fitted(x))
    # get r-squared
    rsq <- summary(lmfit)$r.squared
    # get omega squared
    osq <- 1 - stats::var(stats::residuals(x)) / stats::var(stats::model.response(stats::model.frame(x)))
    # name vectors
    names(rsq) <- "R2"
    names(osq) <- "O2"
    # return results
    return(structure(class = "sjmisc_r2", list(r2 = rsq, o2 = osq)))
  } else {
    stop("`r2` only works on linear (mixed) models of class \"lm\", \"lme\" or \"lmerMod\".", call. = F)
    return(NULL)
  }
}
