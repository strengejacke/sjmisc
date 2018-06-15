context("sjmisc, all_na")

library(sjmisc)

x <- c(NA, NA, NA)
y <- c(1, NA, NA)

test_that("all_na", {
  expect_true(all_na(x))
})

test_that("all_na", {
  expect_false(all_na(y))
})

test_that("all_na, data.frame", {
  expect_is(all_na(data.frame(x, y)), "data.frame")
})
