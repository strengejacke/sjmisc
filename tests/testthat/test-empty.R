context("sjmisc, empty_cols")

library(sjmisc)
tmp <- data.frame(a = c(1, 2, 3, NA, 5),
                  b = c(1, NA, 3, NA , 5),
                  c = c(NA, NA, NA, NA, NA),
                  d = c(1, NA, 3, NA, 5))

test_that("empty_cols", {
  expect_equal(unname(empty_cols(tmp)), 3)
})

test_that("empty_rows", {
  expect_equal(empty_rows(tmp), 4)
})
