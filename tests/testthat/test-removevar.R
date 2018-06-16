context("sjmisc, remove_var")

library(sjmisc)
data(efc)

test_that("remove_var", {
  x <- remove_var(efc, 1:3)
  expect_equal(ncol(x), 23)

  x <- remove_var(efc, c82cop1:c90cop9)
  expect_equal(ncol(x), 17)
})
