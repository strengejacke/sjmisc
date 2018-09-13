context("sjmisc, frq")

library(sjmisc)
library(dplyr)

data(efc)

test_that("frq", {
  expect_is(frq(efc$e42dep), class = "list")
})

test_that("frq", {
  expect_is(frq(efc$e42dep, sort.frq = "asc"), class = "list")
})

test_that("frq", {
  expect_is(frq(efc, e42dep, e16sex), class = "list")
})

test_that("frq", {
  expect_is(frq(efc, e42dep, e16sex, sort.frq = "asc"), class = "list")
})

test_that("frq", {
  expect_is(frq(efc$c12hour, auto.grp = 5), class = "list")
})

test_that("frq", {
  expect_is(frq(efc, c12hour, e17age, auto.grp = 5), class = "list")
})

test_that("frq", {
  expect_is(frq(efc, c12hour, e17age, auto.grp = 5, sort.frq = "desc"), class = "list")
})

test_that("frq", {
  frq(efc$e42dep, title = "test")

  efc %>%
    dplyr::group_by(c172code) %>%
    frq(e16sex, title = c("1", "2", "3"))
})

test_that("frq", {
  efc$weights <- abs(rnorm(n = nrow(efc), mean = 1, sd = .5))
  frq(efc, c160age, auto.grp = 5, weights = weights)
  frq(efc, e42dep, weights = weights)

  test.weight <- function(x, y, w) {
    frq(x, y, weights = w)
  }
  test.weight(efc, "neg_c_7", "weights")
})
