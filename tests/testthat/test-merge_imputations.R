if (require("testthat") && require("sjmisc") && require("mice")) {
  data(iris)
  iris$Species[round(runif(5, 1, 150))] <- NA
  imp <- mice(iris)

  test_that("merge_imputations", {
    merge_imputations(iris, imp)
  })
}
