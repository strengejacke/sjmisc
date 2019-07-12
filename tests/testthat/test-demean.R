context("sjmisc, de_mean")

library(sjmisc)

data(efc)
efc$ID <- sample(1:4, nrow(efc), replace = TRUE) # fake-ID



test_that("de_mean", {
  de_mean(efc, c12hour, barthtot, grp = ID)
  de_mean(efc, c12hour, barthtot, grp = ID, append = FALSE)
  de_mean(efc, c12hour, barthtot, grp = ID, append = FALSE, suffix.dm = "dm", suffix.gm = "gm")
  de_mean(efc, c12hour, barthtot, grp = ID, suffix.dm = "dm", suffix.gm = "gm")
  de_mean(efc, c12hour, barthtot, grp = "ID")
})
