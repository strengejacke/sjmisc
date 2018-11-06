context("sjmisc, add_case")

library(sjmisc)

d <- data.frame(
  a = c(1, 2, 3),
  b = c("a", "b", "c"),
  c = c(10, 20, 30),
  stringsAsFactors = FALSE
)

attr(d, "test") <- "abc"

test_that("add_case", {
  add_case(d, b = "d")
  add_case(d, b = "d", .after = -1)
  add_case(d, b = "d", a = 5, .before = 1)
  add_case(d, b = "d", a = 5, .after = Inf)
  add_case(d, b = "d", .after = 2)
  add_case(d, b = "d", .after = 5)
  add_case(d, b = "d", a = 5, .after = 2, .before = 2)
})

test_that("add_variable", {
  add_variables(d, new = 5)
  add_variables(d, new = 5, .after = 3)
  add_variables(d, new = 5, .after = Inf)
  add_variables(d, new = c(4, 4, 4), new2 = c(5, 5, 5), .after = "b")
  add_variables(d, new = c(4, 4, 4), new2 = c(5, 5, 5), .after = Inf)
  add_variables(d, new = c(4, 4, 4), new2 = c(5, 5, 5), .after = -1)
})

test_that("add_variable", {
  x <- add_variables(d, new = 5)
  expect_equal(attr(x, "test", exact = TRUE), "abc")
})
