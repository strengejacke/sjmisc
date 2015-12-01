#' @title Check overdispersion of GLMM's
#' @name overdisp
#' @description This function checks generalized linear mixed models for
#'                overdispersion. Currently, only \code{\link[lme4]{merMod}} objects
#'                are supported.
#'
#' @param x Fitted mixed effects model (\code{\link[lme4]{merMod}}-class).
#'
#' @return Information on the overdispersion test. If the p-value is larger than
#'           0.05, no overdispersion is present. A p-value lower than 0.005
#'           suggests overdispersion.
#'
#' @details This function is based on the code in the
#'            \href{http://glmm.wikidot.com/faq}{DRAFT r-sig-mixed-models FAQ},
#'            section \emph{How can I deal with overdispersion in GLMMs?}.
#'            Note that this function only returns an \emph{approximate} estimate
#'            of an overdispersion parameter.
#'
#' @references \href{http://glmm.wikidot.com/faq}{DRAFT r-sig-mixed-models FAQ}
#'
#' @importFrom stats df.residual residuals pchisq
#' @export
overdisp <- function(x) {
  return(overdisp.lme4(x))
}


overdisp.lme4 <- function(x) {
  # ------------------------
  # check if suggested package is available
  # ------------------------
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' needed for this function to work. Please install it.", call. = FALSE)
  }
  # ------------------------
  # check object class
  # ------------------------
  if (any(class(x) == "glmerMod")) {
    rdf <- stats::df.residual(x)
    rp <- stats::residuals(x, type = "pearson")
    Pearson.chisq <- sum(rp ^ 2)
    prat <- Pearson.chisq / rdf
    pval <- stats::pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
    if (pval > 0.05)
      message("No overdispersion.")
    else
      message("Overdispersion.")
    c(
      chisq = Pearson.chisq,
      ratio = prat,
      rdf = rdf,
      p = pval
    )
  } else {
    warning("This method currently only supports `glmer` fitted models.", call. = F)
  }
}
