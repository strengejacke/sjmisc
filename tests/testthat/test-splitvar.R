context("sjmisc, split_var")

library(sjmisc)
library(dplyr)
data(mtcars)

test_that("std, split_var", {
  tmp <- split_var(mtcars, disp, n = 3, append = FALSE)
  expect_equal(sum(tmp$disp_g == 1), 11)


  tmp <- mtcars %>%
    dplyr::group_by(cyl) %>%
    split_var(disp, n = 3, append = FALSE)

  expect_equal(sum(tmp$disp_g == 1), 10)
})
