context("sjmisc, zap_inf")

library(sjmisc)

test_that("zap_inf", {
  x <- c(1, 2, NA, 3, NaN, 4, NA, 5, Inf, -Inf, 6, 7)
  tmp <- zap_inf(x)
  expect_equal(any(is.infinite(tmp)), FALSE)
})
