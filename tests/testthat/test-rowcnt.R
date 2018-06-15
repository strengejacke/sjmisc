context("sjmisc, row_count")

library(sjmisc)
library(tibble)
dat <- tribble(
  ~c1, ~c2, ~c3, ~c4,
  1,   3,   1,   1,
  2,   2,   1,   1,
  3,   1,   2,   3,
  1,   2,   1,   2,
  3,  NA,   3,   1,
  NA,   3,  NA,   2
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
