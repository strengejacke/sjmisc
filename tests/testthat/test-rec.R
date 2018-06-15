context("sjmisc, empty_cols")

library(sjmisc)
data(iris)
data(efc)

test_that("empty_cols", {
  expect_equal(length(unique(rec(iris$Sepal.Length, rec = "lo:5=1;5.01:6.5=2;6.501:max=3", append = TRUE))), 3)
})

test_that("empty_cols", {
  expect_equal(length(unique(rec(efc$c12hour, rec = "5=10;else=2"))), 3)
  expect_equal(length(unique(rec(efc$c12hour, rec = "5=10;else=2;NA=2"))), 2)
  expect_equal(length(unique(rec(efc$c82cop1, rec = "1,2=1; NA=9; else=copy"))), 4)
})

test_that("empty_cols", {
  efc$c172code <- as.factor(efc$c172code)
  expect_is(rec(efc$c12hour, rec = "5=10;else=2", as.num = FALSE), "factor")
  expect_is(rec(efc$c172code, rec = "rev", as.num = TRUE), "numeric")
})
