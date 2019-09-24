context("sjmisc, word_wrap")

library(sjmisc)

test_that("word_wrap", {
  x     <- c("my friend likes me", "your friend likes you")
  x_5   <- c("my\nfriend\nlikes\nme", "your\nfriend\nlikes\nyou")
  x_20  <- c("my friend likes me", "your friend likes\nyou")
  expect_equal(word_wrap(x, 5), x_5)
  expect_equal(word_wrap(x, 20), x_20)
  expect_equal(word_wrap(x, 0), x)
  expect_equal(word_wrap(x, Inf), x)

  x <- c(expression(paste(italic("Index"), ""^"®™")))
  expect_warning(word_wrap(x, 10))
})
