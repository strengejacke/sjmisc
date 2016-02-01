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
  n <- nrow(x$model)
  CoxSnell <- 1 - exp((x$deviance - x$null) / n)
  Nagelkerke <- CoxSnell / (1 - exp(-x$null / n))
  pseudor2 <- list(CoxSnell, Nagelkerke)
  names(pseudor2) <- c("CoxSnell", "Nagelkerke")
  return(pseudor2)
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

  return(abs(m2 - m1))
}
