context("sjmisc, prcn")

library(sjmisc)

test_that("prcn", {
  expect_equal(prcn(0.2389), "23.89%")
})

test_that("prcn", {
  expect_equal(prcn(c(0.2143887, 0.55443)), c("21.44%", "55.44%"))
})
