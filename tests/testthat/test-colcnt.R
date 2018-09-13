context("sjmisc, col_count")

library(sjmisc)
dat <- data.frame(
  c1 = c(1, 2, 3, 1, 3, NA),
  c2 = c(3, 2, 1, 2, NA, 3),
  c3 = c(1, 1, 2, 1, 3, NA),
  c4 = c(1, 1, 3, 2, 1, 2)
)

test_that("col_count", {
  expect_equal(sum(col_count(dat, count = 1, append = FALSE)), 9)
})

test_that("col_count", {
  expect_equal(sum(col_count(dat, count = NA, append = FALSE)), 3)
})

test_that("col_count", {
  expect_equal(sum(col_count(dat, c2:c4, count = 2, append = FALSE)), 5)
})
