library(testthat)
library(sjmisc)

x.char <- c("Category 1", "Category 2", "Category 3", NA)  # with spaces
y.char <- c("Category1", "Category2", "Category3", NA)     # w/o spaces

dat <- data.frame(x.char, y.char, stringsAsFactors = FALSE)  # make a data frame


x <- sjmisc::rec(
  dat, y.char, suffix = "_r", as.num = TRUE,
  rec = "Category1 = 1 [Label 1]; Category2 = 2 [Label 2]; Category3 = 3 [Label 3];"
)

expect_equal(as.vector(x$y.char_r), c(1, 2, 3, NA))
expect_equal(attributes(x$y.char_r)$labels, c(`Label 1` = 1, `Label 2` = 2, `Label 3` = 3))



x <- sjmisc::rec(
  dat, x.char, suffix = "_r", as.num = TRUE,
  rec = "Category 1 = 1 [Label 1]; Category 2 = 2 [Label 2]; Category 3 = 3 [Label 3];"
)

expect_equal(as.vector(x$x.char_r), c(1, 2, 3, NA))
expect_equal(attributes(x$x.char_r)$labels, c(`Label 1` = 1, `Label 2` = 2, `Label 3` = 3))
