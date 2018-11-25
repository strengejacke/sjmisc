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
  expect_equal(nrow(add_case(d, b = "d")), 4)
  expect_equal(add_case(d, b = "d", .after = -1)[1, 1], as.numeric(NA))
  expect_equal(add_case(d, b = "d", a = 5, .before = 1)[1, 1], 5)
  expect_equal(add_case(d, b = "d", a = 5, .after = Inf)[4, 1], 5)
  expect_equal(add_case(d, b = "d", .after = 2)[3, 1], as.numeric(NA))
  expect_equal(add_case(d, b = "d", .after = 5)[4, 1], as.numeric(NA))
  expect_equal(add_case(d, b = "d", a = 5, .after = 2, .before = 2)[2, 1], 5)
})

test_that("add_variable", {
  expect_equal(ncol(add_variables(d, new = 5)), 4)
  expect_equal(colnames(add_variables(d, new = 5, .after = 3))[4], "new")
  expect_equal(colnames(add_variables(d, new = 5, .after = Inf))[4], "new")
  expect_equal(colnames(add_variables(d, new = c(4, 4, 4), new2 = c(5, 5, 5), .after = "b"))[3:4], c("new", "new2"))
  add_variables(d, new = c(4, 4, 4), new2 = c(5, 5, 5), .after = Inf)
  add_variables(d, new = c(4, 4, 4), new2 = c(5, 5, 5), .after = -1)
})

test_that("add_variable", {
  x <- add_variables(d, new = 5)
  expect_equal(attr(x, "test", exact = TRUE), "abc")
})
