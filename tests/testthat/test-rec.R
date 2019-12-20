context("sjmisc, rec")

library(sjmisc)
data(iris)
data(efc)

test_that("rec", {
  expect_equal(length(unique(rec(iris$Sepal.Length, rec = "lo:5=1;5.01:6.5=2;6.501:max=3", append = TRUE))), 3)
})

test_that("rec", {
  expect_equal(length(unique(rec(efc$c12hour, rec = "5=10;else=2"))), 3)
  expect_equal(length(unique(rec(efc$c12hour, rec = "5=10;else=2;NA=2"))), 2)
  expect_equal(length(unique(rec(efc$c82cop1, rec = "1,2=1; NA=9; else=copy"))), 4)
})

test_that("rec", {
  efc$c172code <- as.factor(efc$c172code)
  expect_is(rec(efc$c12hour, rec = "5=10;else=2", as.num = FALSE), "factor")
  expect_is(rec(efc$c172code, rec = "rev", as.num = TRUE), "numeric")
})

test_that("rec", {
  x <- c(1,2,3,NA)
  expect_equal(rec(x, rec = "1:3=NA;NA=1;else=2"), c(NA, NA, NA, 1))
  expect_equal(rec(x, rec = "1=NA;NA=1;else=copy"), c(NA, 2, 3, 1))
  expect_equal(rec(x, rec = "min=10;max=5;NA=9;else=copy"), c(10, 2, 5, 9))
})

test_that("rec", {
  skip_on_cran()
  expect_equal(
    unique(rec(iris, Petal.Length, rec = "lo:3=1;3.01:4.5=2;4.501:max=3", append = T, suffix = "")$Petal.Length),
    c(1, 3, 2)
  )
})


test_that("rec rev", {
  x <- c(1, 2, 3, 4, 1, 2, 3, 4)
  x <- sjlabelled::set_labels(x, labels = c("1" = "a", "2" = "b", "3" = "c"))
  expect_equal(
    rec(x, rec = "rev"),
    structure(c(4, 3, 2, 1, 4, 3, 2, 1), labels = c(c = 2, b = 3, a = 4))
  )


  x <- c(1, 2, 3, 4, 1, 2, 3, 4)
  x <- sjlabelled::set_labels(x, labels = c("1" = "a", "2" = "b", "3" = "c", "4" = "d"))
  expect_equal(
    rec(x, rec = "rev"),
    structure(c(4, 3, 2, 1, 4, 3, 2, 1), labels = c(d = 1, c = 2, b = 3, a = 4))
  )

  x <- c(1, 2, 3, 4, 1, 2, 3, 4)
  x <- sjlabelled::set_labels(x, labels = c("1" = "a", "2" = "b", "3" = "c", "4" = "d", "5" = "e"))
  expect_equal(
    rec(x, rec = "rev"),
    structure(c(5, 4, 3, 2, 5, 4, 3, 2), labels = c(e = 1, d = 2, c = 3, b = 4, a = 5))
  )
}
)
