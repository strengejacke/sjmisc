context("sjmisc, count_na")

library(sjmisc)

x <- to_dummy(data.frame(x = c("yes", "no", "yes", "maybe"), stringsAsFactors = F), suffix = "label")
y <- to_dummy(data.frame(x = c("yes", "no", "yes", "maybe")), suffix = "label")

test_that("to_dummy", {
  expect_identical(x, y)
})
