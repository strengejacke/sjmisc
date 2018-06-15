context("sjmisc, split_var")

library(sjmisc)

dat <- data.frame(
  c1 = c(1,2,NA,4),
  c2 = c(NA,2,NA,5),
  c3 = c(NA,4,NA,NA),
  c4 = c(2,3,7,8),
  c5 = c(1,7,5,3)
)

test_that("std, split_var", {
  tmp <- row_means(dat, n = 4, append = FALSE)
  expect_equal(sum(is.na(tmp[[1]])), 2)

  tmp <- row_means(dat, n = .4, append = FALSE)
  expect_equal(sum(is.na(tmp[[1]])), 0)

  # this one is R-behaviour, because round(2.5) = 2
  tmp <- row_means(dat, n = .5, append = FALSE)
  expect_equal(sum(is.na(tmp[[1]])), 0)

  tmp <- row_means(dat, n = .51, append = FALSE)
  expect_equal(sum(is.na(tmp[[1]])), 1)
})
