context("sjmisc, count_na")

library(sjmisc)
library(haven)
library(tibble)

x <- labelled(
  x = c(1:3, tagged_na("a", "c", "z"), 4:1, tagged_na("a", "a", "c"), 1:3, tagged_na("z", "c", "c"), 1:4, tagged_na("a", "c", "z")),
  labels = c("Agreement" = 1, "Disagreement" = 4, "First" = tagged_na("c"), "Refused" = tagged_na("a"), "Not home" = tagged_na("z"))
)

y <- labelled(
  x = c(1:3, tagged_na("e", "d", "f"), 4:1, tagged_na("f", "f", "d"), 1:3, tagged_na("f", "d", "d"), 1:4, tagged_na("f", "d", "f")),
  labels = c("Agreement" = 1, "Disagreement" = 4, "An E" = tagged_na("e"), "A D" = tagged_na("d"), "The eff" = tagged_na("f"))
)

test_that("count_na, general", {
  dat <- tibble(x, y)

  # possible count()-function calls
  count_na(dat)
  count_na(dat$x)
  count_na(dat, x)
  count_na(dat, x, y)
})

test_that("count_na, labels", {
  tmp <- count_na(x)
  tmp$label <- as.character(tmp$label)

  expect_match(tmp$label, "First", fixed = T, all = F)
  expect_match(tmp$label, "Refused", fixed = T, all = F)
  expect_match(tmp$label, "Not home", fixed = T, all = F)
})
