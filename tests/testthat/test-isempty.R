context("sjmisc, is_empty")

library(sjmisc)

test_that("is_empty", {
  skip_on_cran()

  expect_true(is_empty(NULL))
  expect_true(is_empty(NA))
  expect_true(is_empty(""))
  expect_false(is_empty(" "))
  x <- 1
  x <- x[-1]
  expect_true(is_empty(x))

  expect_true(is_empty(c("", " "), first.only = TRUE))
  expect_equal(is_empty(c("", " "), first.only = FALSE), c(TRUE, FALSE))

  expect_true(is_empty(data.frame()))
  expect_true(is_empty(list(NULL)))
})

