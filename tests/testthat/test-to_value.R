context("sjmisc, to_value")

library(sjmisc)

test_that("to_value", {
  expect_equal(to_value(factor(c(0,1,2)), keep.labels = FALSE), c(0,1,2))
  expect_equal(to_value(factor(c(2,3,4)), keep.labels = FALSE), c(2,3,4))
  expect_equal(to_value(factor(c("a", "b", "c")), keep.labels = FALSE), c(1,2,3))
  expect_equal(to_value(factor(c("d", "e", "f")), keep.labels = FALSE), c(1,2,3))
})

test_that("to_value", {
  expect_equal(to_value(factor(c(0,1,2)), start.at = 4, keep.labels = FALSE), c(4,5,6))
  expect_equal(to_value(factor(c(2,3,4)), start.at = 4, keep.labels = FALSE), c(4,5,6))
  expect_equal(to_value(factor(c("a", "b", "c")), start.at = 4, keep.labels = FALSE), c(4,5,6))
  expect_equal(to_value(factor(c("d", "e", "f")), start.at = 4, keep.labels = FALSE), c(4,5,6))
})

