if (require("testthat") && require("sjmisc") && require("dplyr")) {
  data(efc)
  data(mtcars)
  efc$c172code <- as.factor(efc$c172code)
  efc$e42dep <- as.factor(efc$e42dep)

  test_that("std, vector", {
    expect_is(std(efc$c12hour), "numeric")
  })

  test_that("std, data.frame", {
    expect_is(std(efc, c12hour), "data.frame")
  })

  test_that("std, robust", {
    expect_is(std(efc, c12hour, c160age, robust = "2sd"), "data.frame")
  })

  test_that("std, robust", {
    expect_is(std(efc, c12hour, c160age, robust = "gmd", append = FALSE), "data.frame")
  })

  test_that("std, factors", {
    tmp <- std(efc, append = FALSE)
    expect_is(tmp$c172code_z, "factor")

    tmp <- std(efc, append = FALSE, include.fac = TRUE)
    expect_is(tmp$c172code_z, "numeric")
  })

  test_that("std, factors", {
    mtcars %>%
      dplyr::group_by(cyl) %>%
      std(disp)
  })
}
