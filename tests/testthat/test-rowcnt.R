context("sjmisc, row_count")

library(sjmisc)

dat <- data.frame(
  c1 = c(1, 2, 3, 1, 3, NA),
  c2 = c(3, 2, 1, 2, NA, 3),
  c3 = c(1, 1, 2, 1, 3, NA),
  c4 = c(1, 1, 3, 2, 1, 2)
)

test_that("row_count", {
  expect_equal(sum(row_count(dat, count = 1, append = FALSE)), 9)
})

test_that("row_count", {
  expect_equal(sum(row_count(dat, count = NA, append = FALSE)), 3)
})

test_that("row_count", {
  expect_equal(sum(row_count(dat, c1:c3, count = 2, append = FALSE)), 4)
})
