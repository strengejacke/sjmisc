context("sjmisc, tidy_values")

library(sjmisc)
f1 <- "A B C"
f2 <- "A.BC"
f3 <- "A/B"
f4 <- "A_B"
f5 <- "AB"

test_that("tidy_values", {
  expect_equal(tidy_values(f1), "A_B_C")
})

test_that("tidy_values", {
  expect_equal(tidy_values(f2), "A_BC")
})

test_that("tidy_values", {
  expect_equal(tidy_values(f3), "A_B")
})

test_that("tidy_values", {
  expect_equal(tidy_values(f4), "A_B")
})

test_that("tidy_values", {
  expect_equal(tidy_values(f5), "AB")
})
