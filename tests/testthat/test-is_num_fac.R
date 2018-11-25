context("sjmisc, to_dummy")

library(sjmisc)

test_that("is_num_fac", {
  expect_true(is_num_fac(factor(c(NA, 1, 3, NA, 2, 4))))
})

test_that("is_num_fac", {
  expect_false(is_num_fac(c(NA, 1, 3, NA, 2, 4)))
})

test_that("is_num_fac", {
  expect_false(is_num_fac(factor(c(NA, "C", 1, 3, "A", NA, 2, 4))))
})

test_that("is_num_fac", {
  expect_false(is_num_fac(factor(c("Justus", "Bob", "Peter"))))
})

test_that("is_num_chr", {
  expect_false(is_num_chr(c("a", "1")))
})

test_that("is_num_chr", {
  expect_false(is_num_chr(c("a", NA, "1")))
})

test_that("is_num_chr", {
  expect_true(is_num_chr(c("2", "1")))
})

test_that("is_num_chr", {
  expect_true(is_num_chr(c("2", NA_character_, "1")))
})

test_that("is_num_chr", {
  expect_true(is_num_chr(c("2", NA, "1")))
})

test_that("is_num_chr", {
  expect_false(is_num_chr(c(2, NA, 1)))
})
