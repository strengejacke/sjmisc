context("sjmisc, replace_na")

library(sjmisc)
data(efc)

library(haven)
x <- labelled(
  c(1:3, tagged_na("a", "z"), 4:1),
  c("Agreement" = 1, "Disagreement" = 4, "First" = tagged_na("c"), "Not home" = tagged_na("z"))
)

test_that("replace_na", {
  expect_true(sum(is.na(replace_na(efc$e42dep, value = 99))) == 0)
})


test_that("replace_na", {
  expect_true(sum(is.na(replace_na(x, value = 99))) == 0)
  expect_true(sum(is.na(replace_na(x, value = 99, tagged.na = "a"))) == 1)
  expect_equal(names(attr(replace_na(x, value = 99, tagged.na = "a", na.label = "test"), "labels") == 99)[3], "test")
})
