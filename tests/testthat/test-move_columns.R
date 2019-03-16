context("sjmisc, to_dummy")

library(sjmisc)
data(iris)

test_that("move_columns", {
  skip_on_cran()

  expect_equal(
    colnames(move_columns(iris, Sepal.Width, .after = "Species")),
    c("Sepal.Length", "Petal.Length", "Petal.Width", "Species", "Sepal.Width")
  )

  expect_equal(
    colnames(move_columns(iris, Species, .before = 1)),
    c("Species", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  )

  expect_equal(
    colnames(move_columns(iris, Sepal.Width, .before = Sepal.Length)),
    c("Sepal.Width", "Sepal.Length", "Petal.Length", "Petal.Width", "Species")
  )

  expect_equal(
    colnames(move_columns(iris, "Species", "Petal.Length", .after = 1)),
    c("Sepal.Length", "Species", "Petal.Length", "Sepal.Width", "Petal.Width")
  )

})
