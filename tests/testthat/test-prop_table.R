test_that("flat_table uses fractional weights for percentages", {
  d <- data.frame(
    group = factor(c("a", "a", "b", "b")),
    response = factor(c("x", "y", "x", "y")),
    w = c(0.1, 0.25, 0.3, 0.55)
  )
  counts <- stats::xtabs(w ~ group + response, data = d)
  margins <- list(cell = NULL, row = 1L, col = 2L)

  for (margin in names(margins)) {
    expected <- round(stats::ftable(prop.table(counts, margins[[margin]])) * 100, 9)
    expect_equal(
      flat_table(d, group, response, weights = w, margin = margin, digits = 9),
      expected
    )
  }

  expect_equal(flat_table(d, group, response, weights = w), stats::ftable(round(counts)))
  expect_equal(
    flat_table(d, group, response, margin = "col"),
    stats::ftable(prop.table(stats::xtabs(~ group + response, data = d), 2L)) * 100
  )
})

test_that("weighted percentages retain the levels of a single factor", {
  d <- data.frame(
    group = factor(c("a", "a", "b", "b"), levels = c("a", "b", "unused")),
    w = c(0.2, 0.3, 0.7, 0)
  )
  expected <- stats::ftable(d["group"])
  expected[] <- c(41.67, 58.33, 0)

  expect_equal(flat_table(d, group, weights = w, margin = "cell"), expected)
  expect_equal(flat_table(d, group, weights = w, margin = "row"), expected)

  expected[] <- c(100, 100, NaN)
  expect_equal(flat_table(d, group, weights = w, margin = "col"), expected)
})
