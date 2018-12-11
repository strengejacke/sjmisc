context("sjmisc, frq-weights")

library(sjmisc)
data(efc)
efc$weights <- abs(rnorm(n = nrow(efc), mean = 1, sd = .5))
efc$w <- abs(rnorm(n = nrow(efc), mean = 1, sd = .5))

test_that("frq-weights", {
  frq(efc, c172code, weights = weights)
  frq(efc, c172code, weights = "weights")
  frq(efc, c172code, weights = efc$weights)
  frq(efc$c172code, weights = efc$weights)
})

test_that("frq-weights", {
  frq(efc, e16sex)
  frq(efc$e16sex)
  frq(efc, e16sex, weights = w)
  frq(efc, e16sex, weights = "w")
  frq(efc, e16sex, weights = efc$w)
  frq(efc$e16sex, weights = efc$w)
})
