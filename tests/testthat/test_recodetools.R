test_that("rec", {
  skip_on_cran()

  data(efc)
  table(efc$e42dep, exclude = NULL)

  # replace NA with 5
  table(rec(efc$e42dep, "1=1;2=2;3=3;4=4;NA=5"), exclude = NULL)

  # recode 1 to 2 into 1 and 3 to 4 into 2
  table(rec(efc$e42dep, "1,2=1; 3,4=2"), exclude = NULL)

  # recode 1 to 3 into 4 into 2
  table(rec(efc$e42dep, "min:3=1; 4=2"), exclude = NULL)

  # recode 2 to 1 and all others into 2
  table(rec(efc$e42dep, "2=1; else=2"), exclude = NULL)

  # reverse value order
  table(rec(efc$e42dep, "rev"), exclude = NULL)

  head(efc[, 6:9])
  head(rec(efc[, 6:9], "1=10;2=20;3=30;4=40"))
})
