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
  frq(efc, e42dep, weights = "weights")
  frq(efc, e42dep, weights = efc$weights)

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

#' # with grouped data frames for which some groups are completely missing,
#' # and also choosing minimum frequencies
#' efc %>%
#'   slice(1:4) %>%
#'   group_by(c12hour) %>%
#'   frq(nur_pst)
#'
#' efc %>%
#'   slice(1:4) %>%
#'   group_by(c12hour) %>%
#'   frq(nur_pst, min.frq = 1)
#'



test_that("frq, string", {
  dat <- data.frame(
    x = c("", "", "a", "a", "b"),
    stringsAsFactors = FALSE
  )

  expect_equal(
    frq(dat$x),
    structure(list(structure(list(
      val = structure(c(1L, 2L, 3L, NA), .Label = c("", "a", "b"), class = "factor"),
      label = c("<none>", "<none>", "<none>", NA),
      frq = c(2L, 2L, 1L, 0L),
      raw.prc = c(40, 40, 20, 0),
      valid.prc = c(40, 40, 20, NA),
      cum.prc = c(40, 80, 100, NA)
    ),
    row.names = c(NA, -4L),
    class = "data.frame",
    label = "x",
    vartype = "character",
    mean = 1.8,
    sd = 0.836660026534076,
    ci = structure(
      list(
        lower = c(-0.147032972460588, -0.147032972460588, -0.753045081153163, 0),
        upper = c(4.14703297246059, 4.14703297246059, 2.75304508115316, 0)
      ),
      class = "data.frame",
      row.names = c(NA, -4L)
    ),
    relative.ci = structure(
      list(
      lower = c(-0.0294065944921176, -0.0294065944921176, -0.150609016230633, 0),
      upper = c(0.829406594492118, 0.829406594492118, 0.550609016230633, 0)
    ),
    class = "data.frame",
    row.names = c(NA, -4L)))),
    class = c("sjmisc_frq", "list"),
    print = "txt",
    encoding = "UTF-8"
    ),
    tolerance = 1e-4
  )

  expect_equal(
    frq(dat[["x"]]),
    structure(list(structure(list(
      val = structure(c(1L, 2L, 3L, NA), .Label = c("", "a", "b"), class = "factor"),
      label = c("<none>", "<none>", "<none>", NA),
      frq = c(2L, 2L, 1L, 0L),
      raw.prc = c(40, 40, 20, 0),
      valid.prc = c(40, 40, 20, NA),
      cum.prc = c(40, 80, 100, NA)
    ),
    row.names = c(NA, -4L),
    class = "data.frame",
    label = "x",
    vartype = "character",
    mean = 1.8,
    sd = 0.836660026534076,
    ci = structure(
      list(
        lower = c(-0.147032972460588, -0.147032972460588, -0.753045081153163, 0),
        upper = c(4.14703297246059, 4.14703297246059, 2.75304508115316, 0)
      ),
      class = "data.frame",
      row.names = c(NA, -4L)
    ),
    relative.ci = structure(
      list(
        lower = c(-0.0294065944921176, -0.0294065944921176, -0.150609016230633, 0),
        upper = c(0.829406594492118, 0.829406594492118, 0.550609016230633, 0)
      ),
      class = "data.frame",
      row.names = c(NA, -4L)))),
    class = c("sjmisc_frq", "list"),
    print = "txt",
    encoding = "UTF-8"
    ),
    tolerance = 1e-4
  )

  expect_equal(
    frq(dat["x"]),
    structure(list(structure(list(
      val = structure(c(1L, 2L, 3L, NA), .Label = c("", "a", "b"), class = "factor"),
      label = c("<none>", "<none>", "<none>", NA),
      frq = c(2L, 2L, 1L, 0L),
      raw.prc = c(40, 40, 20, 0),
      valid.prc = c(40, 40, 20, NA),
      cum.prc = c(40, 80, 100, NA)
    ),
    row.names = c(NA, -4L),
    class = "data.frame",
    label = "x",
    vartype = "character",
    mean = 1.8,
    sd = 0.836660026534076,
    ci = structure(
      list(
        lower = c(-0.147032972460588, -0.147032972460588, -0.753045081153163, 0),
        upper = c(4.14703297246059, 4.14703297246059, 2.75304508115316, 0)
      ),
      class = "data.frame",
      row.names = c(NA, -4L)
    ),
    relative.ci = structure(
      list(
        lower = c(-0.0294065944921176, -0.0294065944921176, -0.150609016230633, 0),
        upper = c(0.829406594492118, 0.829406594492118, 0.550609016230633, 0)
      ),
      class = "data.frame",
      row.names = c(NA, -4L)))),
    class = c("sjmisc_frq", "list"),
    print = "txt",
    encoding = "UTF-8"
    ),
    tolerance = 1e-4
  )
})


