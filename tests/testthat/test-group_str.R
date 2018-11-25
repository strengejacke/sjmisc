context("sjmisc, group_str")

library(sjmisc)
library(dplyr)

data(efc)

test_that("group_str", {
  data(efc)
  efc$e15relat <- to_label(efc$e15relat)
  levels(efc$e15relat) <- c("Hello", "Helo", "Hole", "Apple", "Ape", "System", "Systemic", "new")
  efc$e15relat <- to_character(efc$e15relat)

  group_str(efc$e15relat)
  x <- efc$e15relat
  group_str(x)

  oldstring <- c("Hello", "Helo", "Hole", "Apple", "Ape", "New", "Old", "System", "Systemic")
  group_str(oldstring)

  oldstring <- c("Hello", "Helo", "Hole", NA_character_, "Apple", "Ape", "New", "Old", "System", "Systemic", NA_character_)
  expect_equal(group_str(oldstring)[4], NA_character_)
  expect_equal(group_str(oldstring)[11], NA_character_)
})
