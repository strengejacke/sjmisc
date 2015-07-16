## ----collapse=TRUE-------------------------------------------------------
library(haven)
x <- labelled(c(1, 2, 1, 8, 9),
              c(Male = 1, Female = 2, 
                Refused = 8, "Not applicable" = 9),
              c(FALSE, FALSE, TRUE, TRUE))

print(x)

## ----message=FALSE, collapse=TRUE----------------------------------------
library(Hmisc)
x <- c(1, 2, 3, 4)
label(x) <- "Variable label"
units(x) <- "cm"
str(x)

## ----eval=FALSE, collapse=TRUE-------------------------------------------
#  library(foreign)
#  efc <- read.spss("sample_dataset.sav",
#                   to.data.frame = TRUE,
#                   use.value.labels = FALSE,
#                   reencode = "UTF-8")
#  str(efc$16sex)
#  
#  > $e16sex  : atomic  2 2 2 2 2 2 1 2 2 2 ...
#  >  ..- attr(*, "value.labels")= Named chr  "2" "1"
#  >  .. ..- attr(*, "names")= chr  "female" "male"
#  
#  attr(efc, "variable.labels")['e16sex']
#  
#  >            e16sex
#  >  "elder's gender"

## ----message=FALSE, collapse=TRUE----------------------------------------
library(sjmisc)
# sjmisc-sample data
data(efc)
str(efc$e16sex)

## ----collapse=TRUE-------------------------------------------------------
get_labels(efc$e42dep)

## ----collapse=TRUE-------------------------------------------------------
get_labels(efc$e42dep, include.values = "p")

## ----collapse=TRUE-------------------------------------------------------
x <- factor(c("low", "mid", "low", "hi", "mid", "low"))
get_labels(x)

## ----collapse=TRUE-------------------------------------------------------
x <- factor(c("low", "mid", "low", "hi", "mid", "low"))
get_labels(x, attr.only = TRUE)

## ----collapse=TRUE-------------------------------------------------------
x <- labelled(c(1, 2, 1, 3, 4, 1, NA, 5),
              c(Male = 1, Female = 2, Refused = 5),
              c(FALSE, FALSE, TRUE))
get_labels(x)

## ----collapse=TRUE-------------------------------------------------------
get_labels(x, include.non.labelled = TRUE)

## ----collapse=TRUE-------------------------------------------------------
print(x)
get_values(x)

## ----collapse=TRUE-------------------------------------------------------
get_values(x, drop.na = TRUE)

## ----collapse=TRUE-------------------------------------------------------
x <- sample(1:4, 20, replace = TRUE)
x <- set_labels(x, c("very low", "low", "mid", "hi"))
x

## ----collapse=TRUE-------------------------------------------------------
x <- c(2, 2, 3, 3, 2)
x <- set_labels(x, c("a", "b", "c"))
x

## ----collapse=TRUE-------------------------------------------------------
x <- c(2, 2, 3, 3, 2)
x <- set_labels(x, c("a", "b", "c"), 
                force.labels = TRUE)
x

## ----collapse=TRUE-------------------------------------------------------
x <- c(1, 2, 3, 2, 4, NA)
x <- set_labels(x, c("yes", "maybe", "no"))
x

## ----collapse=TRUE-------------------------------------------------------
x <- c(1, 2, 3, 2, 4, NA)
x <- set_labels(x, c("yes", "maybe", "no"),
                force.values = FALSE)
x

## ----collapse=TRUE-------------------------------------------------------
x <- c(1, 2, 3, 2, 4, 5)
x <- set_labels(x, c(`1` = "strongly agree", 
                     `4` = "totally disagree", 
                     `5` = "refused",
                     `9` = "missing"))
x

