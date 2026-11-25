test_that("grouped weighted descriptions use available statistics", {
  dat <- data.frame(
    group = rep(c("A", "B"), each = 4),
    value = c(1:4, 11:14),
    wt = c(1, 2, 1, 3, 2, 4, 1, 1)
  )
  grouped <- dplyr::group_by(dat, group)
  result <- descr(grouped, weights = wt)

  expect_s3_class(result, "sjmisc_grpdescr")
  expect_equal(length(result), 2L)
  expect_identical(names(result[[1]])[1:3], c("var", "type", "label"))
  expect_identical(names(result[[1]]), names(descr(dat, weights = wt)))
  expected <- vapply(split(dat, dat$group), function(x) {
    stats::weighted.mean(x$value, x$wt)
  }, numeric(1))
  expect_equal(vapply(result, function(x) x$mean, numeric(1)), unname(expected))

  selected <- descr(grouped, weights = wt, show = c("mean", "md", "n", "mean"))
  expect_identical(
    lapply(selected, names),
    rep(list(c("var", "mean", "n", "mean.1")), 2)
  )
  expect_equal(vapply(selected, function(x) x$mean, numeric(1)), unname(expected))

  omitted <- descr(grouped, weights = wt, show = "md")
  expect_identical(lapply(omitted, names), list("var", "var"))
})
