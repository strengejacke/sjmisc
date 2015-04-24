test_that("to_value", {
  skip_on_cran()

  dummy <- c(TRUE, TRUE, FALSE, TRUE)
  to_value(dummy)
  to_value(dummy, FALSE)
  to_value(dummy, startAt = 5)

  dummy <- c(TRUE, TRUE, FALSE, TRUE)
  to_fac(dummy)
  table(to_fac(dummy))

  dummy <- c(TRUE, TRUE, FALSE, TRUE)
  to_label(dummy)

  dummy <- c(TRUE, TRUE, FALSE, TRUE)
  set_na(dummy, FALSE)
  set_na(dummy, TRUE)
  dummy <- set_val_labels(dummy, c("falsch", "wahr"))
  set_na(dummy, FALSE)
})
