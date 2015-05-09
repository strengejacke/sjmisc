# bind global variables
if(getRversion() >= "2.15.1") utils::globalVariables(c("fit"))


#' @title Compute eta-squared of fitted anova
#' @name eta_sq
#' @description Returns the eta-squared value for one-way-anovas.
#'
#' @param ... A fitted one-way-anova model or a dependent and grouping variable (see examples).
#' @return The eta-squared value.
#'
#' @note Interpret eta-squared like r-squared or R-squared; a rule of thumb (Cohen):
#'         \itemize{
#'          \item .02 ~ small
#'          \item .13 ~ medium
#'          \item .26 ~ large
#'         }
#'
#' @references \itemize{
#'               \item \href{http://stats.stackexchange.com/questions/78808/}{How to compute eta-sq in ANOVA by hand?}
#'               \item \href{http://stats.stackexchange.com/questions/15958/}{How to interpret and report eta squared?}
#'               \item \href{http://en.wikiversity.org/wiki/Eta-squared}{Wikipedia: Eta-squared}
#'               \item Levine TR, Hullett CR (2002): Eta Squared, Partial Eta Squared, and Misreporting of Effect Size in Communication Research (\href{https://www.msu.edu/~levinet/eta\%20squared\%20hcr.pdf}{pdf})
#'             }
#'
#' @examples
#' # load sample data
#' data(efc)
#'
#' # fit linear model
#' fit <- aov(c12hour ~ as.factor(e42dep), data = efc)
#'
#' # print eta sqaured
#' eta_sq(fit)
#'
#' # grouping variable will be converted to factor autoamtically
#' eta_sq(efc$c12hour, efc$e42dep)
#'
#' @export
eta_sq <- function(...) {
  # --------------------------------------------------------
  # retrieve list of parameters
  # --------------------------------------------------------
  input_list <- list(...)
  # --------------------------------------------------------
  # check if fitted anova
  # --------------------------------------------------------
  if (length(input_list) == 1 && any(class(input_list[[1]]) == "aov")) {
    # retrieve model
    fit <- input_list[[1]]
  } else if (length(input_list) == 2) {
    # retrieve variables
    depVar <- input_list[[1]]
    grpVar <- input_list[[2]]
    # convert to factor
    if (!is.factor(grpVar)) grpVar <- as.factor(grpVar)
    # fit anova
    fit <- aov(depVar ~ grpVar)
  }
  # return eta squared
  return (summary.lm(fit)$r.squared)
  # return (1 - var(fit$residuals, na.rm = T) / var(fit$model[,1], na.rm = T))
}


#' @title Compute std. beta coefficients and ci of lm and mixed models
#' @name std_beta
#' @description Returns the standardized beta coefficients and confidence intervals
#'                of a fitted linear (mixed) models, i.e. \code{fit} must either
#'                be of class \code{lm} or \code{\link[lme4]{merMod}}.
#'
#' @param fit A fitted linear (mixed) model of class \code{\link{lm}} or \code{\link[lme4]{merMod}} (lme4-package).
#' @param include.ci logical, if \code{TRUE}, a data frame with confidence intervals will be returned,
#'          when \code{fit} is of class \code{lm}. If \code{fit} is a \code{lmerMod} object (lme4-package),
#'          always returns standard error instead of confidence intervals (hence, this paramezer will
#'          be ignored when \code{fit} is a \code{lmerMod} object).
#' @return A vector with standardiized beta coefficients of the fitted linear model, or a data frame
#'           with standardized confidence intervals, if \code{include.ci = TRUE}.
#'
#' @note "Standardized coefficients refer to how many standard deviations a dependent variable will change,
#'         per standard deviation increase in the predictor variable. Standardization of the coefficient is
#'         usually done to answer the question of which of the independent variables have a greater effect
#'         on the dependent variable in a multiple regression analysis, when the variables are measured
#'         in different units of measurement (for example, income measured in dollars and family size
#'         measured in number of individuals)." (Source: Wikipedia)
#'
#' @references \href{http://en.wikipedia.org/wiki/Standardized_coefficient}{Wikipedia: Standardized coefficient}
#'
#' @examples
#' # fit linear model
#' fit <- lm(airquality$Ozone ~ airquality$Wind + airquality$Temp + airquality$Solar.R)
#' # print std. beta coefficients
#' std_beta(fit)
#'
#' # print std. beta coefficients and ci
#' std_beta(fit, include.ci = TRUE)
#'
#' @export
std_beta <- function(fit, include.ci = FALSE) {
  # if we have merMod object (lme4), we need
  # other function to compute std. beta
  if (any(class(fit) == "lmerMod") || any(class(fit) == "merModLmerTest")) {
    return (sjs.stdmm(fit))
  } else {
    b <- summary(fit)$coef[-1, 1]
    sx <- sapply(as.data.frame(fit$model)[-1], sd, na.rm = T)
    sy <- sapply(as.data.frame(fit$model)[1], sd, na.rm = T)
    beta <- b * sx / sy
    se <- summary(fit)$coefficients[-1, 2]
    beta.se <- se * sx / sy
    # check if confidence intervals should also be returned
    # if yes, create data frame with sb and ci
    if (include.ci) {
      return (data.frame(beta = beta,
                         ci.low = (beta - beta.se * 1.96),
                         ci.hi = (beta + beta.se * 1.96)))
    } else {
      return(beta)
    }
  }
}


sjs.stdmm <- function(fit) {
  # code from Ben Bolker, see
  # http://stackoverflow.com/a/26206119/2094622
  # ------------------------
  # check if suggested package is available
  # ------------------------
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' needed for this function to work. Please install it.", call. = FALSE)
  }
  sdy <- sd(lme4::getME(fit, "y"))
  sdx <- apply(lme4::getME(fit, "X"), 2, sd)
  sc <- lme4::fixef(fit) * sdx / sdy
  se.fixef <- coef(summary(fit))[, "Std. Error"]
  se <- se.fixef * sdx / sdy
  mydf <- data.frame(stdcoef = sc, stdse = se)
  rownames(mydf) <- names(lme4::fixef(fit))
  return(mydf)
}


#' @title Performs a Mann-Whitney-U-Test
#' @name mwu
#' @description This function performs a Mann-Whitney-U-Test (or \code{Wilcoxon rank sum test},
#'                see \code{\link{wilcox.test}} and \code{\link[coin]{wilcox_test}})
#'                for the variable \code{var}, which is
#'                divided into groups indicated by \code{grp} (so the formula \code{var ~ grp}
#'                is used). If \code{grp} has more than two categories, a comparison between each
#'                two groups is performed. \cr \cr
#'                The function reports U, p and Z-values as well as effect size r
#'                and group-rank-means.
#'
#' @param var A numeric vector / variable, where the Mann-Whitney-U-Test should be applied to.
#' @param grp The grouping variable indicating the groups that should be used for comparison.
#' @param distribution indicates how the null distribution of the test statistic should be computed. Mey be one of
#'          \code{exact}, \code{approximate} or \code{asymptotic} (default).
#'          See \code{\link[coin]{wilcox_test}} for details.
#' @param weights defining integer valued weights for the observations. By default,
#'          this is \code{NULL}.
#' @return (Invisibly) returns a data frame with U, p and Z-values for each group-comparison
#'         as well as effect-size r; additionally, group-labels and groups' n's are
#'         also included.
#'
#' @note This function calls the \code{\link[coin]{wilcox_test}} with formula. If \code{grp}
#'         has more than two groups, additionally a Kruskal-Wallis-Test (see \code{\link{kruskal.test}})
#'         is performed. \cr \cr
#'         Interpretation of effect sizes, as a rule-of-thumb:
#'         \itemize{
#'          \item small effect >= 0.1
#'          \item medium effect >= 0.3
#'          \item large effect >= 0.5
#'        }
#'
#' @examples
#' data(efc)
#' # Mann-Whitney-U-Tests for elder's age by elder's dependency.
#' mwu(efc$e17age, efc$e42dep)
#'
#' @export
mwu <- function(var, grp, distribution="asymptotic", weights=NULL) {
  # ------------------------
  # check if suggested package is available
  # ------------------------
  if (!requireNamespace("coin", quietly = TRUE)) {
    stop("Package 'coin' needed for this function to work. Please install it.", call. = FALSE)
  }
  # do we have a factor? if yes, make numeric
  if (is.factor(grp)) grp <- to_value(grp)
  # group "counter" (index) should start with 1, not 0
  if (min(grp, na.rm = TRUE) == 0) grp <- grp + 1
  # retrieve unique group values. need to iterate all values
  grp_values <- sort(unique(na.omit(grp)))
  # length of value range
  cnt <- length(grp_values)
  labels <- autoSetValueLabels(grp)
  message("Performing Mann-Whitney-U-Test...")
  message("---------------------------------")
  message("showing statistics between groups (x|y)")
  df <- data.frame()
  for (i in 1:cnt) {
    for (j in i:cnt) {
      if (i != j) {
        # retrieve cases (rows) of subgroups
        xsub <- var[which(grp == grp_values[i] | grp == grp_values[j])]
        ysub <- grp[which(grp == grp_values[i] | grp == grp_values[j])]
        # only use rows with non-missings
        ysub <- ysub[which(!is.na(xsub))]
        # adjust weights, pick rows from subgroups (see above)
        if (!is.null(weights)) {
          wsub <- as.integer(na.omit(weights[which(!is.na(xsub))]))
        }
        # remove missings
        xsub <- as.numeric(na.omit(xsub))
        ysub.n <- na.omit(ysub)
        # grouping variable is a factor
        ysub <- as.factor(ysub.n)
        if (is.null(weights)) {
          wt <- coin::wilcox_test(xsub ~ ysub, distribution = distribution)
        } else {
          wt <- coin::wilcox_test(xsub ~ ysub,
                                  distribution = distribution,
                                  weights = as.formula("~wsub"))
        }
        # compute statistics
        u <- as.numeric(coin::statistic(wt, type = "linear"))
        z <- as.numeric(coin::statistic(wt, type = "standardized"))
        p <- coin::pvalue(wt)
        r <- abs(z / sqrt(length(var)))
        w <- wilcox.test(xsub, ysub.n, paired = TRUE)$statistic
        rkm.i <- mean(rank(xsub)[which(ysub.n == grp_values[i])], na.rm = TRUE)
        rkm.j <- mean(rank(xsub)[which(ysub.n == grp_values[j])], na.rm = TRUE)
        # compute n for each group
        n_grp1 <- length(xsub[which(ysub.n == grp_values[i])])
        n_grp2 <- length(xsub[which(ysub.n == grp_values[j])])
        # print to console
        if (is.null(labels)) {
          cat(sprintf("Groups (%i|%i), n = %i/%i:\n",
                      grp_values[i],
                      grp_values[j],
                      n_grp1,
                      n_grp2))
        } else {
          cat(sprintf("Groups %i = %s (n = %i) | %i = %s (n = %i):\n",
                      grp_values[i],
                      labels[i],
                      n_grp1,
                      grp_values[j],
                      labels[j],
                      n_grp2))
        }
        if (p < 0.001) {
          p <- 0.001
          p.string <- "<"
        } else {
          p.string <- "="
        }
        cat(sprintf("  U = %.3f, W = %.3f, p %s %.3f, Z = %.3f\n  effect-size r = %.3f\n  rank-mean(%i) = %.2f\n  rank-mean(%i) = %.2f\n\n", u, w, p.string, p, z, r, i, rkm.i, j, rkm.j))
        df <- rbind(df,
                    cbind(grp1 = grp_values[i],
                          grp1.label = labels[i],
                          grp1.n = n_grp1,
                          grp2 = grp_values[j],
                          grp2.label = labels[j],
                          grp2.n = n_grp2,
                          u = u,
                          w = w,
                          p = p,
                          z = z,
                          r = r,
                          rank.mean.grp1 = rkm.i,
                          rank.mean.grp2 = rkm.j))
      }
    }
  }
  # if we have more than 2 groups, also perfom kruskal-wallis-test
  if (cnt > 2) {
    message("Performing Kruskal-Wallis-Test...")
    message("---------------------------------")
    kw <- kruskal.test(var, grp)
    cat(sprintf("chi-squared = %.3f\n", kw$statistic))
    cat(sprintf("df = %i\n", kw$parameter))
    if (kw$p.value < 0.001) {
      p  <- 0.001
      p.string <- "<"
    } else {
      p <- kw$p.value
      p.string <- "="
    }
    cat(sprintf("p %s %.3f\n", p.string, p))
  }
  # prepare a data frame that can be used for 'sjt.df'.
  tab.df <- data.frame(Groups = sprintf("%s<br>%s",
                                        df$grp1.label,
                                        df$grp2.label),
                       N = sprintf("%s<br>%s",
                                   df$grp1.n,
                                   df$grp2.n),
                       'Mean Rank' = sprintf("%.2f<br>%.2f",
                                             as.numeric(as.character(df$rank.mean.grp1)),
                                             as.numeric(as.character(df$rank.mean.grp2))),
                       'Mann-Whitney-U' = df$u,
                       'Wilcoxon-W' = df$w,
                       Z = sprintf("%.3f", as.numeric(as.character(df$z))),
                       'Effect Size' = sprintf("%.3f", as.numeric(as.character(df$r))),
                       p = sprintf("%.3f", as.numeric(as.character(df$p))))
  # replace 0.001 with <0.001
  levels(tab.df$p)[which(levels(tab.df$p) == "0.001")] <- "<0.001"
  # return both data frames
  invisible(structure(class = "mwu",list(df = df, tab.df = tab.df)))
}


#' @title Performs a Chi-square goodness-of-fit-test
#' @name chisq_gof
#'
#' @description This method performs a Chi-square goodness-of-fit-test (GOF)
#'                either on a numeric vector against probabilities, or
#'                a Goodness-of-fit tests for \code{\link{glm}}s for binary data.
#'
#' @param x a numeric vector / variable, or a \code{\link{glm}}-object.
#' @param prob a vector of probabilities (indicating the population probabilities) of the same length
#'          as \code{x}'s amount of categories / factor levels. Use \code{nrow(table(x))} to
#'          determine the amount of necessary values for \code{prob}. Only used,
#'          when \code{x} is a vector, and not a \code{glm}-object.
#' @param weights a vector with weights, used to weight \code{x}.
#' @return For vectors, (insisibly) returns the object of the computed \code{\link{chisq.test}}.
#'           \cr \cr
#'           For \code{glm}-objects, an object of class \code{chisq_gof} with
#'           following values:
#'           \itemize{
#'            \item \code{p.value}	the p-value for the goodness-of-fit test
#'            \item \code{z.score} the standardized z-score for the goodness-of-fit test
#'            \item \code{RSS} the residual sums of squares term
#'            \item \code{X2} the pearson chi-squared statistic
#'           }
#'
#' @note For vectors, this function is a convenient function for the \code{\link{chisq.test}},
#'         performing goodness-of-fit test.
#'         \cr \cr
#'         For \code{glm}-objects, this function performs a goodness-of-fit test
#'         based on the \code{\link[binomTools]{X2GOFtest}} function of the
#'         \code{binomTools} package.
#'         A well-fitting model shows no significant difference between
#'         the model and the observed data, i.e. the reported p-values should be
#'         greater than 0.05.
#'
#' @examples
#' data(efc)
#' # differing from population
#' chisq_gof(efc$e42dep, c(0.3,0.2,0.22,0.28))
#' # equal to population
#' chisq_gof(efc$e42dep, prop.table(table(efc$e42dep)))
#'
#' # goodness-of-fit test for logistic regression
#' efc$services <- dicho(efc$tot_sc_e, "v", 0, asNum = TRUE)
#' fit <- glm(services ~ neg_c_7 + c161sex + e42dep,
#'            data = efc,
#'            family = binomial(link = "logit"))
#' chisq_gof(fit)
#'
#' @export
chisq_gof <- function(x, prob = NULL, weights = NULL) {
  if (any(class(x) == "glm")) {
    # ------------------------------------
    # This is an adapted version from the
    # "binomTools" package. The "X2GOFtest()"
    # function did not work when model data frame
    # had missing values.
    # ------------------------------------
    y_hat <- fitted(x)
    wt <- x$prior.weight
    vJ <- wt * y_hat * (1 - y_hat)
    cJ <- (1 - 2 * y_hat) / vJ
    X2 <- sum(resid(x, type = "pearson") ^ 2)
    form <- as.formula(x$formula)
    form[[2]] <- as.name("cJ")
    # use model matrix instead of data values,
    # because data may contain more variables
    # than needed, and due to missing may have
    # different row length
    dat <- na.omit(x$model)
    dat$cJ <- cJ
    dat$vJ <- vJ
    RSS <- sum(resid(lm(form, data = dat, weights = vJ)) ^ 2)
    A <- 2 * (length(y_hat) - sum(1 / wt))
    z <- (X2 - x$df.residual) / sqrt(A + RSS)
    p.value <- 2 * pnorm(abs(z), lower.tail = FALSE)
    chi2gof <- list(p.value = p.value,
                    z.score = z,
                    RSS = RSS,
                    X2 = X2)
    class(chi2gof) <- "chi2gof"
  } else {
    # check if we have probs
    if (is.null(prob)) {
      warning("'prob' needs to be specified.", call. = F)
      return(invisible(NULL))
    }
    # performs a Chi-square goodnes-of-fit-test
    if (!is.null(weights)) x <- weight(x, weights)
    dummy <- as.vector(table(x))
    # goodness of fit-test. x is one-dimensional and
    # y not given
    chi2gof <- chisq.test(dummy, p = prob)
  }
  return(chi2gof)
}


#' @title Performs a Hosmer-Lemeshow Goodness-of-fit-test
#' @name hoslem_gof
#'
#' @description This method performs a Hosmer-Lemeshow goodness-of-fit-test
#'                for \code{\link{glm}}s or \code{\link[lme4]{glmer}}s for
#'                binary data.
#'
#' @param x a fitted \code{\link{glm}} or \code{\link[lme4]{glmer}}.
#' @param g number of bins to divide the data Default is 10.
#'
#' @return An object of class \code{hoslem_test} with
#'           following values:
#'           \itemize{
#'            \item \code{chisq} the Hosmer-Lemeshow chi-squared statistic
#'            \item \code{df} degrees of freedom
#'            \item \code{p.value}	the p-value for the goodness-of-fit test
#'           }
#'
#' @note A well-fitting model shows no significant difference between
#'         the model and the observed data, i.e. the reported p-value should be
#'         greater than 0.05.
#'
#' @seealso \code{\link{pseudo_r2}}
#'
#' @examples
#' data(efc)
#'
#' # goodness-of-fit test for logistic regression
#' efc$services <- dicho(efc$tot_sc_e, "v", 0, asNum = TRUE)
#' fit <- glm(services ~ neg_c_7 + c161sex + e42dep,
#'            data = efc,
#'            family = binomial(link = "logit"))
#' hoslem_gof(fit)
#'
#' @export
hoslem_gof <- function(x, g = 10) {
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
    yhat <- fitted(x)
  } else {
    y <- x$y
    yhat <- fitted(x)
  }
  cutyhat <- cut(yhat,
                 breaks = quantile(yhat, probs = seq(0, 1, 1 / g)),
                 include.lowest = TRUE)
  obs <- xtabs(cbind(1 - y, y) ~ cutyhat)
  expect <- xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  chisq <- sum((obs - expect)^2 / expect)
  p.value <- 1 - pchisq(chisq, g - 2)
  hoslem <- list(chisq = chisq,
                 df = g - 2,
                 p.value = p.value)
  class(hoslem) <- "hoslem_test"
  return(hoslem)
}


#' @title Computes Nagelkerke's and Cox-Snell's Pseudo R-squared
#' @name pseudo_r2
#'
#' @description This method calculates Nagelkerke's and Cox-Snell's
#'                pseudo-r-squared-values of \code{\link{glm}}s for
#'                binary data.
#'
#' @param x a fitted \code{\link{glm}}.
#'
#' @return An object of class \code{pseudo_r2} with
#'           following values:
#'           \itemize{
#'            \item \code{CoxSnell} Cox-Snell's pseudo-r-squared-value
#'            \item \code{Nagelkerke} Nagelkerke's pseudo-r-squared-value
#'           }
#'
#' @seealso \code{\link{cod}}
#'
#' @examples
#' data(efc)
#'
#' # Pseudo-R-squared values
#' efc$services <- dicho(efc$tot_sc_e, "v", 0, asNum = TRUE)
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


#' @title Computes Tjur's Coefficient of Discrimination (Pseudo R-squared)
#' @name cod
#'
#' @description This method calculates the Coefficient of Discrimination \code{D}
#'                for \code{\link{glm}}s or \code{\link[lme4]{glmer}}s for
#'                binary data. It is an alternative to other Pseudo-R-squared values
#'                like Nakelkerke's R2 or Cox-Snell R2.
#'
#' @param x a fitted \code{\link{glm}} or \code{\link[lme4]{glmer}}.
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
#' @examples
#' data(efc)
#'
#' # Tjur's R-squared value
#' efc$services <- dicho(efc$tot_sc_e, "v", 0, asNum = TRUE)
#' fit <- glm(services ~ neg_c_7 + c161sex + e42dep,
#'            data = efc,
#'            family = binomial(link = "logit"))
#' cod(fit)
#'
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
    pred <- predict(x, type = "response", re.form = NULL)
  } else {
    y <- x$y
    pred <- predict.glm(x, type = "response")
  }
  categories <- unique(y)
  m1 <- mean(pred[which(y == categories[1])], na.rm = T)
  m2 <- mean(pred[which(y == categories[2])], na.rm = T)

  return(abs(m2 - m1))
}

#' @title Calculates Cronbach's Alpha for a matrix
#' @name cronb
#' @description This function calculates the Cronbach's alpha value for each column
#'                of a data frame or matrix.
#'
#' @seealso \code{\link{reliab_test}}
#'
#' @param df A data frame or matrix with more than 2 columns.
#' @return The Cronbach's alpha value for \code{df}.
#'
#' @note See examples from \code{\link[sjPlot]{sjp.pca}} and \code{\link[sjPlot]{sjt.pca}}.
#'
#' @export
cronb <- function(df) {
  df <- na.omit(df)
  if (is.null(ncol(df)) || ncol(df) < 2) {
    warning("Too less columns in this factor to calculate alpha value!", call. = F)
    return (NULL)
  }
  return (dim(df)[2] / (dim(df)[2] - 1) * (1 - sum(apply(df, 2, var)) / var(rowSums(df))))
}


#' @title Performs a reliability test on an item scale.
#' @name reliab_test
#' @description This function calculates the item discriminations (corrected item-total
#'                correlations for each item of \code{x} with the remaining items) and
#'                the Cronbach's alpha for each item, if it was deleted from the
#'                scale.
#'
#' @seealso \code{\link{cronb}}
#'
#' @param x A data frame with items (from a scale)
#' @param scaleItems If \code{TRUE}, the data frame's vectors will be scaled. Recommended,
#'          when the variables have different measures / scales.
#' @param digits Amount of digits for Cronbach's Alpha and correlation values in
#'          returned data frame.
#' @return A data frame with the corrected item-total correlations (item discrimination)
#'           and Cronbach's alpha (if item deleted) for each item of the scale, or
#'           \code{NULL} if data frame had too less columns.
#'
#' @note This function is similar to a basic reliability test in SPSS. The correlations in
#'         the Item-Total-Statistic are a computed correlation of each item against the sum
#'         of the remaining items (which are thus treated as one item).
#'
#' @examples
#' # -------------------------------
#' # Data from the EUROFAMCARE sample dataset
#' # -------------------------------
#' data(efc)
#'
#' # retrieve variable and value labels
#' varlabs <- get_var_labels(efc)
#'
#' # recveive first item of COPE-index scale
#' start <- which(colnames(efc) == "c82cop1")
#' # recveive last item of COPE-index scale
#' end <- which(colnames(efc) == "c90cop9")
#'
#' # create data frame with COPE-index scale
#' x <- data.frame(efc[, c(start:end)])
#' colnames(x) <- varlabs[c(start:end)]
#'
#' \dontrun{
#' sjt.df(reliab_test(x),
#'        describe = FALSE,
#'        showCommentRow = TRUE,
#'        commentString = sprintf("Cronbach's &alpha;=%.2f",
#'                                cronb(x)))}
#'
#' # ---------------------------------------
#' # Compute PCA on Cope-Index, and perform a
#' # reliability check on each extracted factor.
#' # ---------------------------------------
#' \dontrun{
#' factors <- sjt.pca(x)$factor.index
#' findex <- sort(unique(factors))
#' for (i in 1:length(findex)) {
#'  rel.df <- subset(x, select = which(factors == findex[i]))
#'  if (ncol(rel.df) >= 3) {
#'    sjt.df(reliab_test(rel.df),
#'           describe = FALSE,
#'           showCommentRow = TRUE,
#'           useViewer = FALSE,
#'           title = "Item-Total-Statistic",
#'           commentString = sprintf("Scale's overall Cronbach's &alpha;=%.2f",
#'                                   cronb(rel.df)))
#'    }
#'  }}
#'
#' @export
reliab_test <- function(x, scaleItems=FALSE, digits=3) {
  # -----------------------------------
  # remove missings, so correlation works
  # -----------------------------------
  x <- na.omit(x)
  # -----------------------------------
  # check param
  # -----------------------------------
  if (!is.matrix(x) && !is.data.frame(x)) {
    warning("'x' needs to be a data frame or matrix.", call. = F)
    return (NULL)
  }
  # -----------------------------------
  # remember item (column) names for return value
  # return value gets column names of initial data frame
  # -----------------------------------
  df.names <- colnames(x)
  # -----------------------------------
  # check for minimum amount of columns
  # can't be less than 3, because the reliability
  # test checks for Cronbach's alpha if a specific
  # item is deleted. If data frame has only two columns
  # and one is deleted, Cronbach's alpha cannot be calculated.
  # -----------------------------------
  if (ncol(x) > 2) {
    # -----------------------------------
    # Check whether items should be scaled. Needed,
    # when items have different measures / scales
    # -----------------------------------
    if (scaleItems) x <- data.frame(scale(x, center = TRUE, scale = TRUE))
    # -----------------------------------
    # init vars
    # -----------------------------------
    totalCorr <- c()
    cronbachDeleted <- c()
    # -----------------------------------
    # iterate all items
    # -----------------------------------
    for (i in 1:ncol(x)) {
      # -----------------------------------
      # create subset with all items except current one
      # (current item "deleted")
      # -----------------------------------
      sub.df <- subset(x, select = c(-i))
      # -----------------------------------
      # calculate cronbach-if-deleted
      # -----------------------------------
      cronbachDeleted <- c(cronbachDeleted, cronb(sub.df))
      # -----------------------------------
      # calculate corrected total-item correlation
      # -----------------------------------
      totalCorr <- c(totalCorr, cor(x[, i],
                                    apply(sub.df, 1, sum),
                                    use = "pairwise.complete.obs"))
    }
    # -----------------------------------
    # create return value
    # -----------------------------------
    ret.df <- data.frame(cbind(round(cronbachDeleted, digits),
                               round(totalCorr, digits)))
    # -----------------------------------
    # set names of data frame
    # -----------------------------------
    colnames(ret.df) <- c("Cronbach's &alpha; if item deleted", "Item discrimination")
    rownames(ret.df) <- df.names
  } else {
    warning("Data frame needs at least three columns for reliability-test!", call. = F)
    ret.df <- NULL
  }
  # -----------------------------------
  return(ret.df)
}


#' @title Computes a mean inter-item-correlation.
#' @name mic
#' @description This function calculates a mean inter-item-correlation, i.e.
#'                a correlation matrix of \code{data} will be computed (unless
#'                \code{data} is already a \code{\link{cor}}-object) and the mean
#'                of all added item's correlation values is returned.
#'                Requires either a data frame or a computed \code{\link{cor}}-object.
#'
#' @param data A correlation object (see \code{\link{cor}}-function), or a data frame
#'          which correlations should be calculated.
#' @param corMethod Indicates the correlation computation method. May be one of
#'          \code{"spearman"} (default), \code{"pearson"} or \code{"kendall"}.
#'          You may use initial letter only.
#' @return The value of the computed mean inter-item-correlation.
#'
#' @examples
#' # -------------------------------
#' # Data from the EUROFAMCARE sample dataset
#' # -------------------------------
#' data(efc)
#' # recveive first item of COPE-index scale
#' start <- which(colnames(efc)=="c82cop1")
#' # recveive last item of COPE-index scale
#' end <- which(colnames(efc)=="c90cop9")
#' # create data frame with COPE-index scale
#' df <- as.data.frame(efc[,c(start:end)])
#'
#' mic(df)
#'
#' @export
mic <- function(data, corMethod="pearson") {
  # -----------------------------------
  # Check parameter
  # -----------------------------------
  if (corMethod == "s") corMethod <- "spearman"
  if (corMethod == "p") corMethod <- "pearson"
  if (corMethod == "k") corMethod <- "kendall"
  # -----------------------------------
  # Mean-interitem-corelation
  # -----------------------------------
  if (class(data) == "matrix") {
    corr <- data
  } else {
    data <- na.omit(data)
    corr <- cor(data, method = corMethod)
  }
  # -----------------------------------
  # Sum up all correlation values
  # -----------------------------------
  meanic <- c()
  for (j in 1:(ncol(corr) - 1)) {
    # first correlation is always "1" (self-correlation)
    for (i in (j + 1):nrow(corr)) {
      # check four valid bound
      if (i <= nrow(corr) && j <= ncol(corr)) {
        # add up all subsequent values
        meanic <- c(meanic, corr[i, j])
      } else {
        meanic <- c(meanic, "NA")
      }
    }
  }
  return (mean(meanic))
}


#' @title Compute expected and relative table values
#' @name table_values
#' @description This function calculates a table's cell, row and column percentages as
#'                well as expected values and returns all results as lists of tables.
#'
#' @param tab A simple \code{\link{table}} or \code{\link{ftable}} of which cell, row and column percentages
#'          as well as expected values are calculated. Tables of class \code{\link{xtabs}} and other will
#'          be coerced to \code{\link{ftable}} objects.
#' @param digits The amount of digits for the table percentage values.
#' @return (invisibly) returns a list with four tables:
#'         \enumerate{
#'          \item \code{cell} a table with cell percentages of \code{tab}
#'          \item \code{row} a table with row percentages of \code{tab}
#'          \item \code{col} a table with column percentages of \code{tab}
#'          \item \code{expected} a table with expected values of \code{tab}
#'         }
#'
#' @examples
#' tab <- table(sample(1:2, 30, TRUE), sample(1:3, 30, TRUE))
#' # show expected values
#' table_values(tab)$expected
#' # show cell percentages
#' table_values(tab)$cell
#'
#' @export
table_values <- function(tab, digits=2) {
  # convert to ftable object
  if (all(class(tab) != "ftable")) tab <- ftable(tab)
  tab.cell <- round(100 * prop.table(tab), digits)
  tab.row <- round(100 * prop.table(tab, 1), digits)
  tab.col <- round(100 * prop.table(tab, 2), digits)
  tab.expected <- as.table(round(as.array(margin.table(tab, 1)) %*% t(as.array(margin.table(tab, 2))) / margin.table(tab)))
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible (structure(class = "sjutablevalues",
                       list(cell = tab.cell,
                            row = tab.row,
                            col = tab.col,
                            expected = tab.expected)))
}


#' @title Phi value for a contingency table
#' @name phi
#' @description Compute Phi value for a contingency table.
#'
#' @seealso \code{\link{cramer}}

#' @param tab A simple \code{\link{table}} or \code{\link{ftable}}. Tables of class
#'          \code{\link{xtabs}} and other will be coerced to \code{\link{ftable}} objects.
#' @return The table's Phi value.
#'
#' @examples
#' tab <- table(sample(1:2, 30, TRUE), sample(1:2, 30, TRUE))
#' phi(tab)
#'
#' @importFrom MASS loglm
#' @export
phi <- function(tab) {
  # convert to flat table
  if (all(class(tab) != "ftable")) tab <- ftable(tab)
  tb <- summary(MASS::loglm(~1 + 2, tab))$tests
  phi_val <- sqrt(tb[2, 1] / sum(tab))
  return(phi_val)
}


#' @title Cramer's V for a contingency table
#' @name cramer
#' @description Compute Cramer's V for a table with more than 2x2 fields.
#'
#' @seealso \code{\link{phi}}
#'
#' @param tab A simple \code{\link{table}} or \code{\link{ftable}}. Tables of class
#'          \code{\link{xtabs}} and other will be coerced to \code{\link{ftable}} objects.
#' @return The table's Cramer's V.
#'
#' @examples
#' tab <- table(sample(1:2, 30, TRUE), sample(1:3, 30, TRUE))
#' cramer(tab)
#'
#' @export
cramer <- function(tab) {
  if (all(class(tab) != "ftable")) tab <- ftable(tab)
  phi_val <- phi(tab)
  cramer <- sqrt(phi_val^2 / min(dim(tab) - 1))
  return(cramer)
}


#' @title Compute standard error for variables
#' @name std_e
#' @description Compute standard error for variables
#'
#' @param x a (numeric) vector / variable.
#' @return The standard error of variable \code{x}.
#'
#' @examples
#' std_e(rnorm(n = 100, mean = 3))
#'
#' @export
std_e <- function(x) sqrt(var(x, na.rm = TRUE) / length(na.omit(x)))


#' @title Compute coefficient of variation
#' @name cv
#' @description Compute coefficient of variation for single variables
#'                (standard deviation divided by mean) or for fitted
#'                linear (mixed effects) models (root mean squared error
#'                (RMSE) divided by mean of dependent variable).
#'
#' @param x a (numeric) vector / variable or a fitted linear model of class
#'          \code{\link{lm}}, \code{\link[lme4]{merMod}} (lme4) or
#'          \code{\link[nlme]{lme}} (nlme).
#' @return The coefficient of variation of \code{x}.
#'
#' @details The advantage of the cv is that it is unitless. This allows
#'            coefficient of variation to be compared to each other in ways
#'            that other measures, like standard deviations or root mean
#'            squared residuals, cannot be \href{http://www.ats.ucla.edu/stat/mult_pkg/faq/general/coefficient_of_variation.htm}{source: UCLA-FAQ}.
#'
#' @seealso \code{\link{rmse}}
#'
#' @references \href{http://www.ats.ucla.edu/stat/mult_pkg/faq/general/coefficient_of_variation.htm}{UCLA-FAQ: What is the coefficient of variation?}
#'
#' @examples
#' data(efc)
#' cv(efc$e17age)
#'
#' @export
cv <- function(x) {
  # check if we have a fitted linear model
  if (class(x) == "lm" || any(class(x) == "lmerMod") || any(class(x) == "lme") || any(class(x) == "merModLmerTest")) {
    if (class(x) == "lm") {
      # dependent variable in lm
      dv <- x$model[[1]]
    } else if (any(class(x) == "lmerMod") || any(class(x) == "merModLmerTest")) {
      # dependent variable in lmerMod
      dv <- x@frame[[1]]
    } else if (any(class(x) == "lme")) {
      # dependent variable in lme
      dv <- x$data[[1]]
    }
    # compute mean of dependent variable
    mw <- mean(dv, na.rm = TRUE)
    # check if mean is zero?
    if (mw != 0) {
      # cv = root mean squared error (RMSE) divided by mean of dep. var.
      rmse(x) / mw
    } else {
      warning("Mean of dependent variable is zero. Cannot compute model's coefficient of variation.", call. = F)
    }
  } else {
    # compute mean of variable
    mw <- mean(x, na.rm = TRUE)
    # check if mean is zero?
    if (mw != 0) {
      #  we assume a simple vector
      sd(x, na.rm = TRUE) / mw
    } else {
      warning("Mean of 'x' is zero. Cannot compute coefficient of variation.", call. = F)
    }
  }
}


#' @title Compute root mean squared error (RMSE)
#' @name rmse
#' @description Compute root mean squared error  of fitted linear (mixed effects) models.
#'
#' @param fit a fitted linear model of class \code{\link{lm}},
#'          \code{\link[lme4]{merMod}} (lme4) or \code{\link[nlme]{lme}} (nlme).
#' @return The root mean squared error of \code{fit}.
#'
#' @seealso \code{\link{cv}}
#'
#' @examples
#' data(efc)
#' fit <- lm(barthtot ~ c160age + c12hour, data=efc)
#' rmse(fit)
#'
#' @export
rmse <- function(fit) sqrt(mean(residuals(fit)^2, na.rm = TRUE))


#' @title Plot Levene-Test for One-Way-Anova
#' @name levene_test
#'
#' @description Plot results of Levene's Test for Equality of Variances for One-Way-Anova.
#'
#' @param depVar The dependent variable. Will be used with following formular:
#'          \code{aov(depVar ~ grpVar)}
#' @param grpVar The grouping variable, as unordered factor. Will be used with following formular:
#'          \code{aov(depVar ~ grpVar)}
#'
#' @examples
#' data(efc)
#' levene_test(efc$c12hour, efc$e42dep)
#'
#' @export
levene_test <- function(depVar, grpVar) {
  # check if grpVar is factor
  if (!is.factor(grpVar)) grpVar <- factor(grpVar)
  # remove missings
  df <- na.omit(data.frame(depVar, grpVar))
  # calculate means
  means <- tapply(df$depVar, df$grpVar, mean)
  depVarNew <- abs(df$depVar - means[df$grpVar])
  message("\nLevene's Test for Homogeneity of Variances\n------------------------------------------")
  fit <- aov(depVarNew ~ df$grpVar)
  print(summary(fit))
  pval <- summary(fit)[[1]]['Pr(>F)'][1,1]
  # print "summary" of test
  message("\nConclusion:")
  if (pval > 0.05) {
    message("Groups are homogeneous. Everything's fine.\n")
  } else {
    message("Groups are not homogeneous!\n")
  }
}


#' @title Check whether two factors are crossed
#' @name is_crossed
#' @description This function checks whether two factors are crossed,
#'                i.e. if each level of one factor occurs in combination
#'                with each level of the other factor.
#'
#' @param f1 a numeric vector or \code{\link{factor}}.
#' @param f2 a numeric vector or \code{\link{factor}}.
#' @return Logical, \code{TRUE} if factors are crossed, \code{FALSE} otherwise.
#'
#' @seealso \code{\link{is_nested}}
#'
#' @references Grace, K. The Difference Between Crossed and Nested Factors. \href{http://www.theanalysisfactor.com/the-difference-between-crossed-and-nested-factors/}{(web)}
#'
#' @examples
#' # crossed factors, each category of
#' # x appears in each category of y
#' x <- c(1,4,3,2,3,2,1,4)
#' y <- c(1,1,1,2,2,1,2,2)
#' # show distribution
#' table(x, y)
#' # check if crossed
#' is_crossed(x, y)
#'
#' # not crossed factors
#' x <- c(1,4,3,2,3,2,1,4)
#' y <- c(1,1,1,2,1,1,2,2)
#' # show distribution
#' table(x, y)
#' # check if crossed
#' is_crossed(x, y)
#'
#' @export
is_crossed <- function(f1, f2) {
  tab <- table(f1, f2)
  # for crossed factors, we should have no zeros in any rows
  # (i.e. each level of f1 also contains any level of f2)
  return (!any(apply(tab, 1, function(x) any(x==0)) == TRUE))
}


#' @title Check whether two factors are nested
#' @name is_nested
#' @description This function checks whether two factors are nested,
#'                i.e. if each category of the first factor co-occurs
#'                with only one category of the other.
#'
#' @param f1 a numeric vector or \code{\link{factor}}.
#' @param f2 a numeric vector or \code{\link{factor}}.
#' @return Logical, \code{TRUE} if factors are nested, \code{FALSE} otherwise.
#'
#' @note If factors are nested, a message is displayed to tell whether \code{f1}
#'         is nested within \code{f2} or vice versa.
#'
#' @seealso \code{\link{is_crossed}}
#'
#' @references Grace, K. The Difference Between Crossed and Nested Factors. \href{http://www.theanalysisfactor.com/the-difference-between-crossed-and-nested-factors/}{(web)}
#'
#' @examples
#' # nested factors, each category of
#' # x appears in one category of y
#' x <- c(1,2,3,4,5,6,7,8,9)
#' y <- c(1,1,1,2,2,2,3,3,3)
#' # show distribution
#' table(x, y)
#' # check if nested
#' is_nested(x, y)
#' is_nested(y, x)
#'
#' # not nested factors
#' x <- c(1,2,3,4,5,6,7,8,9,1,2)
#' y <- c(1,1,1,2,2,2,3,3,3,2,3)
#' # show distribution
#' table(x, y)
#' # check if nested
#' is_nested(x, y)
#' is_nested(y, x)
#'
#' @export
is_nested <- function(f1, f2) {
  tab <- table(f1, f2)
  # cross tabulation of nested factors should have only 1 value per row
  # (or column) that is not zero. If we found more, factors are not nested
  # or rows and columns have to be swapped.
  # check if f1 is nested within f2
  nested <- !any(apply(tab, 1, function(x) sum(x!=0) > 1))
  if (nested) message("'f1' is nested within 'f2'")
  # swap rows and columns to check whether factors are nested
  # check whether f2 is nested within f1
  if (!nested) {
    nested <- !any(apply(tab, 2, function(x) sum(x!=0) > 1))
    if (nested) message("'f2' is nested within 'f1'")
  }
  return (nested)
}


#' @title Compute Intra-Class-Correlation
#' @name icc
#' @description This function calculates the intraclass-correlation
#'                (icc) for random intercepts of mixed effects models.
#'                Currently, only \code{\link[lme4]{merMod}} objects
#'                are supported.
#'
#' @param x a fitted mixed effects model (\code{\link[lme4]{merMod}}-class).
#'
#' @return A numeric vector with all random intercept intraclass-correlation-coefficients.
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
#' fit <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#' icc(fit)
#'
#' sleepstudy$mygrp <- sample(1:45, size = 180, replace = T)
#' fit <- lmer(Reaction ~ Days + (1|mygrp) + (Days | Subject), sleepstudy)
#' icc(fit)}
#'
#' @export
icc <- function(x) {
  return(icc.lme4(x))
}


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
    # random effects variances
    # ------------------------
    reva <- summary(fit)$varcor
    # retrieve only intercepts
    vars <- lapply(reva, function(x) x[[1]])
    # residual variances
    if (any(class(fit) == "glmerMod")) {
      # for logistic models, we use pi / 3
      resid_var <- (pi^2) / 3
    } else {
      # for linear models, we have a clear
      # residual variance
      resid_var <- attr(reva, "sc")^2
    }
    # total variance
    total_var <- sum(sapply(vars, sum), resid_var)
    # random intercept icc
    ri.icc <- sapply(vars, function(x) x[1]) / total_var
    # name values
    names(ri.icc) <- names(reva)
    # icc standard errors
    # ri.icc.se <- unlist(lapply(reva, function(x) attr(x, "stddev")[1]))
    # names(ri.icc.se) <- paste0(names(reva), " (S.E.)")
    # return(list(icc = ri.icc, se = ri.icc.se))
    return(ri.icc)
  } else {
    warning("Function 'icc' currently only supports 'merMod' objects (package 'lme4').", call. = F)
  }
}

# retrieve variance of random intercepts
# and residuals
lmer_var <- function(fit) {
  reva <- summary(fit)$varcor
  # retrieve only intercepts
  vars <- unlist(lapply(reva, function(x) x[[1]]))
  names(vars) <- names(reva)
  # residual variances
  if (any(class(fit) == "glmerMod")) {
    # for logistic models, we use pi / 3
    resid_var <- (pi^2) / 3
  } else {
    # for linear models, we have a clear
    # residual variance
    resid_var <- attr(reva, "sc")^2
  }
  return(list('Between group variance' = vars,
              'Within group variance' = resid_var))
}


lm_pval_fstat <- function(x) {
  if (class(x) != "lm") stop("Not an object of class 'lm'.", call. = F)
  f <- summary(x)$fstatistic
  p <- pf(f[1], f[2], f[3], lower.tail = F)
  return(as.vector(p))
}