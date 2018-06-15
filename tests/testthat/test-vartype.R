context("sjmisc, var_type")

library(sjmisc)
data(efc)

test_that("var_type", {
  expect_equal(var_type(1, abbr = FALSE), "numeric")
  expect_equal(var_type(1, abbr = TRUE), "dbl")

  expect_equal(var_type(1L, abbr = FALSE), "integer")
  expect_equal(var_type(1L, abbr = TRUE), "int")

  expect_equal(var_type("1", abbr = FALSE), "character")
  expect_equal(var_type("1", abbr = TRUE), "chr")
})

