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


v1 <- c(1, 2, 1, 2, 1, 1)
v2 <- c(1, 2, 1, NA, 1, 1)

test_that("frq, show.na", {
  expect_equal(nrow(frq(v1, show.na = TRUE)[[1]]), 3)
  expect_equal(nrow(frq(v1, show.na = FALSE)[[1]]), 2)
  expect_equal(nrow(frq(v1, show.na = "auto")[[1]]), 2)

  expect_equal(nrow(frq(v2, show.na = TRUE)[[1]]), 3)
  expect_equal(nrow(frq(v2, show.na = FALSE)[[1]]), 2)
  expect_equal(nrow(frq(v2, show.na = "auto")[[1]]), 3)
})


test_that("frq", {
  data(efc)
  efc$e15relat <- to_label(efc$e15relat)
  levels(efc$e15relat) <- c("Hello", "Helo", "Hole", "Apple", "Ape", "System", "Systemic", "new")
  efc$e15relat <- to_character(efc$e15relat)

  frq(efc$e15relat)
  frq(efc, e15relat)
  frq(efc$e15relat, grp.strings = 2)
  frq(efc, e15relat, grp.strings = 2)
  x <- efc$e15relat
  frq(x)
  frq(x, grp.strings = 2)

  efc %>% dplyr::group_by(c172code) %>% frq(c161sex, e15relat, grp.strings = 2)
})
