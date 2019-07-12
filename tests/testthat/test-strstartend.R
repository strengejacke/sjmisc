context("sjmisc, str_start")

library(sjmisc)

test_that("str_start", {
  x <- c("my_friend_likes me", "your_friend likes_you")
  expect_equal(str_start(x, "_"), list(c(3, 10), c(5, 18)))
  expect_equal(str_start(x, "likes"), list(c(11), c(13)))

  x <- c("my_friend_likes me", "your_friend likes_you")
  expect_equal(str_start(x, "ho"), list(-1, -1))
})

test_that("str_end", {
  x <- c("my_friend_likes me", "your_friend likes_you")
  expect_equal(str_end(x, "_"), list(c(3, 10), c(5, 18)))
  expect_equal(str_end(x, "likes"), list(c(15), c(17)))

  x <- c("my_friend_likes me", "your_friend likes_you")
  expect_equal(str_end(x, "ho"), list(-1, -1))
})
