if (requireNamespace("haven", quietly = TRUE) && requireNamespace("sjlabelled", quietly = TRUE)) {
  library(sjmisc)
  data(iris)
  data(efc)

  test_that("ref_lvl", {
    x <- to_factor(efc$e42dep)
    tmp <- ref_lvl(x, lvl = 2)
    expect_match(names(attr(tmp, "labels"))[1], "slightly dependent", fixed = T)
  })

  test_that("ref_lvl", {
    tmp <- levels(ref_lvl(iris$Species, lvl = 3))
    expect_match(tmp[1], levels(iris$Species)[3], fixed = T)

    tmp <- levels(ref_lvl(iris$Species, lvl = "versicolor"))
    expect_match(tmp[1], levels(iris$Species)[2], fixed = T)
  })
}
