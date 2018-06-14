context("sjmisc, frq")

library(sjmisc)
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
