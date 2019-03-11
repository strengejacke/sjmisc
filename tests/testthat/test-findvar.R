context("sjmisc, find_var")

library(sjmisc)
data(efc)

test_that("find_var", {
  x <- find_var(efc, pattern = "cop", search = "label_value")
  expect_equal(nrow(x), 1)

  x <- find_var(efc, pattern = "cop", search = "name_label")
  expect_equal(nrow(x), 9)

  x <- find_var(efc, pattern = "cop", search = "all", fuzzy = TRUE)
  expect_equal(nrow(x), 10)
})

