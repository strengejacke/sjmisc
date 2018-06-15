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
