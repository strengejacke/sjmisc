context("sjmisc, row_sums")

library(sjmisc)

dat <- data.frame(
  c1 = c(1,2,NA,4),
  c2 = c(NA,2,NA,5),
  c3 = c(NA,4,NA,NA),
  c4 = c(2,3,7,8),
  c5 = c(1,7,5,3)
)

test_that("row_sums", {
  tmp <- row_sums(dat, n = 4, append = FALSE)
  expect_equal(sum(is.na(tmp[[1]])), 2)
  expect_equal(sum(tmp[[1]], na.rm = TRUE), 38)

  tmp <- row_sums(dat, n = .4, append = FALSE)
  expect_equal(sum(is.na(tmp[[1]])), 0)
  expect_equal(sum(tmp[[1]], na.rm = TRUE), 54)

  # this one is R-behaviour, because round(2.5) = 2
  tmp <- row_sums(dat, n = .5, append = FALSE)
  expect_equal(sum(is.na(tmp[[1]])), 0)
  expect_equal(sum(tmp[[1]], na.rm = TRUE), 54)

  tmp <- row_sums(dat, n = .51, append = FALSE)
  expect_equal(sum(is.na(tmp[[1]])), 1)
  expect_equal(sum(tmp[[1]], na.rm = TRUE), 42)

  tmp <- row_sums(dat, n = 3, append = FALSE)
  expect_equal(sum(is.na(tmp[[1]])), 1)
  expect_equal(sum(tmp[[1]], na.rm = TRUE), 42)

  expect_message(row_sums(dat[, 1, drop = FALSE], n = 0))
})
