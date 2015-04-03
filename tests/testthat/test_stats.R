test_that("cv", {
  skip_on_cran()
  data(efc)
  cv(efc$e42dep)

  fit <- lm(neg_c_7 ~ quol_5, data=efc)
  cv(fit)

  library(lme4)
  fit <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
  cv(fit)

  library(nlme)
  fit <- lme(distance ~ age, data = Orthodont) # random is ~ age
  cv(fit)
})

test_that("cramer", {
  skip_on_cran()
  tab <- table(sample(1:2, 30, TRUE), sample(1:3, 30, TRUE))
  cramer(tab)

  data(efc)
  tab <- ftable(efc$e42dep, efc$e16sex)
  cramer(tab)

  tab <- xtabs(e42dep ~ e16sex, data = efc)
  cramer(tab)
})

test_that("phi", {
  skip_on_cran()
  tab <- table(sample(1:2, 30, TRUE), sample(1:3, 30, TRUE))
  phi(tab)

  data(efc)
  tab <- ftable(efc$e42dep, efc$e16sex)
  phi(tab)

  tab <- xtabs(e42dep ~ e16sex, data = efc)
  phi(tab)
})

test_that("cronb und reliab_test", {
  skip_on_cran()
  data(efc)

  expect_warning(cronb(efc$e42dep))
  expect_warning(reliab_test(efc$e42dep))

  # recveive first item of COPE-index scale
  start <- which(colnames(efc) == "c82cop1")
  # recveive last item of COPE-index scale
  end <- which(colnames(efc) == "c90cop9")

  cronb(efc[, start:end])
  reliab_test(efc[, start:end])
})


test_that("chisq_gof", {
  skip_on_cran()
  data(efc)

  chisq_gof(efc$e42dep, c(0.3,0.2,0.22,0.28))

  chisq_gof(efc$e42dep, prop.table(table(efc$e42dep)))
})

