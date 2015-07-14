## ------------------------------------------------------------------------
library(haven)
x <- labelled(c(1, 2, 1, 8, 9),
              c(Male = 1, Female = 2, 
                Refused = 8, "Not applicable" = 9),
              c(FALSE, FALSE, TRUE, TRUE))

print(x)

## ---- message=FALSE------------------------------------------------------
library(Hmisc)
x <- c(1, 2, 3, 4)
label(x) <- "Variable label"
units(x) <- "cm"
str(x)

## ---- eval=FALSE---------------------------------------------------------
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

