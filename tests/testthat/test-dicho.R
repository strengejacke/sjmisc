context("sjmisc, dicho")

library(sjmisc)
data(efc)


test_that("dicho", {
  tmp <- dicho(efc$c12hour)
  expect_equal(sum(tmp == 1, na.rm = T), 438)
})

test_that("dicho", {
  tmp <- dicho(efc, c12hour, c160age, append = FALSE)
  expect_equal(sum(diag(table(tmp))), 574)
})

test_that("dicho", {
  tmp <- dicho(efc, c12hour, c160age, dich.by = "mean", append = FALSE, as.num = TRUE)
  expect_equal(sum(diag(table(tmp))), 556)
})

test_that("dicho", {
  tmp <- dicho(
    efc,
    c12hour,
    c160age,
    dich.by = "mean",
    append = FALSE,
    as.num = TRUE,
    var.label = "variable labels",
    val.labels = c("zero", "one")
  )

  expect_equal(names(lapply(tmp, attributes)[[2]]$labels), c("zero", "one"))
})

test_that("dicho overwrites columns in place and keeps groupwise cutoffs", {
  data <- dplyr::as_tibble(data.frame(
    first = c(1, 4, 8, 10), key = c(1, 1, 2, 2), last = c(100, 200, 1, 3)
  ))
  result <- dicho(data, last, first, as.num = TRUE, suffix = "")

  expect_identical(names(result), names(data))
  expect_equal(as.numeric(result$first), c(0, 0, 1, 1))
  expect_equal(as.numeric(result$last), c(1, 1, 0, 0))

  data <- dplyr::group_by(data, key)
  result <- dicho(data, last, key, first, as.num = TRUE, suffix = "")
  expect_identical(names(result), names(data))
  expect_identical(result$key, data$key)
  expect_equal(as.numeric(result$first), c(0, 1, 0, 1))
  expect_equal(as.numeric(result$last), c(0, 1, 0, 1))
  expect_equal(dplyr::group_data(result), dplyr::group_data(data))
})
