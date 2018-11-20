context("sjmisc, to_dummy")

library(sjmisc)

x <- to_dummy(data.frame(x = c("yes", "no", "yes", "maybe"), stringsAsFactors = FALSE), suffix = "label")
y <- to_dummy(data.frame(x = c("yes", "no", "yes", "maybe"), stringsAsFactors = TRUE), suffix = "label")

test_that("to_dummy", {
  expect_identical(x, y)
})
