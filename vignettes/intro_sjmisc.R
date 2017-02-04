## ----echo = FALSE--------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
library(haven)
x <- labelled(c(1:3, tagged_na("a", "c", "z"), 4:1),
              c("Agreement" = 1, "Disagreement" = 4, "First" = tagged_na("c"),
                "Refused" = tagged_na("a"), "Not home" = tagged_na("z")))

print(x)

## ------------------------------------------------------------------------
is.na(x)

as_factor(x)

is.na(as_factor(x))

## ----message=FALSE-------------------------------------------------------
library(Hmisc)
x <- c(1, 2, 3, 4)
label(x) <- "Variable label"
units(x) <- "cm"
str(x)

## ----eval=FALSE----------------------------------------------------------
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

## ----message=FALSE-------------------------------------------------------
library(sjmisc)
# sjmisc-sample data, an atomic vector with label attributes
data(efc)
str(efc$e16sex)

## ------------------------------------------------------------------------
get_labels(efc$e42dep)

## ------------------------------------------------------------------------
get_labels(efc$e42dep, include.values = "p")

## ------------------------------------------------------------------------
x <- factor(c("low", "mid", "low", "hi", "mid", "low"))
get_labels(x)

## ------------------------------------------------------------------------
x <- factor(c("low", "mid", "low", "hi", "mid", "low"))
get_labels(x, attr.only = TRUE)

## ------------------------------------------------------------------------
# get labels, including tagged NA values
x <- labelled(
  c(1:3, tagged_na("a", "c", "z"), 4:1),
  c("Agreement" = 1, "Disagreement" = 4, "First" = tagged_na("c"),
    "Refused" = tagged_na("a"), "Not home" = tagged_na("z"))
)
get_labels(x)

## ------------------------------------------------------------------------
get_labels(x, include.non.labelled = TRUE)

## ------------------------------------------------------------------------
get_labels(x, include.values = "n", drop.na = FALSE)

## ------------------------------------------------------------------------
print(x)

get_values(x)

## ------------------------------------------------------------------------
get_values(x, drop.na = TRUE)

## ------------------------------------------------------------------------
x <- sample(1:4, 20, replace = TRUE)

# return new labelled vector
x <- set_labels(x, labels = c("very low", "low", "mid", "hi"))
x

## ------------------------------------------------------------------------
x <- c(2, 2, 3, 3, 2)
x <- set_labels(x, labels = c("a", "b", "c"))
x

## ------------------------------------------------------------------------
x <- c(2, 2, 3, 3, 2)
x <- set_labels(
  x, 
  labels = c("a", "b", "c"), 
  force.labels = TRUE
)
x

## ------------------------------------------------------------------------
x <- c(1, 2, 3, 2, 4, NA)
x <- set_labels(x, labels = c("yes", "maybe", "no"))
x

## ------------------------------------------------------------------------
x <- c(1, 2, 3, 2, 4, NA)
x <- set_labels(
  x, 
  labels = c("yes", "maybe", "no"),
  force.values = FALSE
)
x

## ------------------------------------------------------------------------
x <- c(1, 2, 3, 2, 4, 5)
x <- set_labels(
  x, 
  labels = c("strongly agree" = 1, 
             "totally disagree" = 4, 
             "refused" = 5,
             "missing" = 9)
)
x

## ----echo=FALSE, message=FALSE-------------------------------------------
library(dplyr)

## ------------------------------------------------------------------------
data(efc)

efc %>% 
  select(c82cop1, c83cop2, c84cop3) %>% 
  set_labels(labels = c("not often" = 1, "very often" = 4)) %>% 
  frq()

## ------------------------------------------------------------------------
get_label(efc$e42dep)

get_label(efc, e42dep, e16sex, e15relat)

## ------------------------------------------------------------------------
dummy <- c(1, 2, 3)
testit <- function(x) get_label(x, def.value = deparse(substitute(x)))
# returns name of vector, if it has no variable label
testit(dummy)

## ------------------------------------------------------------------------
x <- sample(1:4, 10, replace = TRUE)

# return new vector
x <- set_label(x, lab = "Dummy-variable")
str(x)

# label existing vector
set_label(x) <- "Another Dummy-variable"
str(x)

## ------------------------------------------------------------------------
x <- data.frame(
  a = sample(1:4, 10, replace = TRUE),
  b = sample(1:4, 10, replace = TRUE),
  c = sample(1:4, 10, replace = TRUE)
)
x <- set_label(x, lab = c("Variable A",
                          "Variable B",
                          "Variable C"))

str(x)

get_label(x)

## ------------------------------------------------------------------------
x <- data.frame(
  a = sample(1:4, 10, replace = TRUE),
  b = sample(1:4, 10, replace = TRUE),
  c = sample(1:4, 10, replace = TRUE)
)

x %>% 
  var_labels(
    a = "Variable A",
    b = "Variable B",
    c = "Variable C"
  ) %>% 
  str()

## ------------------------------------------------------------------------
x <- sample(1:8, 100, replace = TRUE)
# show value distribution
table(x)

# set value 1 and 8 as tagged missings
x <- set_na(x, value = c(1, 8), as.tag = TRUE)
x

# show value distribution, including missings
table(x, useNA = "always")

# now let's see, which NA's were "1" and which were "8"
print_tagged_na(x)

x <- factor(c("a", "b", "c"))
x

# set NA into existing vector
x <- set_na(x, value = "b", as.tag = TRUE)
x

## ------------------------------------------------------------------------
get_na(x)

## ------------------------------------------------------------------------
get_na(x, as.tag = TRUE)

## ------------------------------------------------------------------------
str(efc$c84cop3)

efc$c84cop3 <- set_na(efc$c84cop3, value = c(2, 3), as.tag = TRUE)
get_na(efc$c84cop3)

get_na(replace_na(efc$c84cop3, value = 2, na.label = "restored NA", tagged.na = "2"))

## ------------------------------------------------------------------------
str(efc$c82cop1)

efc$c82cop1 <- set_na(efc$c82cop1, value = c(2, 3), as.tag = TRUE)
get_na(efc$c82cop1, as.tag = TRUE)

efc$c82cop1 <- replace_labels(efc$c82cop1, value = c("new NA label" = tagged_na("2")))

get_na(efc$c82cop1, as.tag = TRUE)

## ------------------------------------------------------------------------
head(to_label(efc$c87cop6))

## ------------------------------------------------------------------------
efc$c83cop2 <- set_na(efc$c83cop2, value = 3, as.tag = TRUE)

# "Often" is now a tagged NA
get_na(efc$c83cop2, as.tag = T)

head(to_label(efc$c83cop2))

table(to_label(efc$c83cop2))

# "Often" is a tagged NA. This NA value will be used 
# as factor level, when drop.na is set to FALSE
head(to_label(efc$c83cop2, drop.na = FALSE))

table(to_label(efc$c83cop2, drop.na = FALSE))

## ------------------------------------------------------------------------
x <- c(1, 2, 3, 2, 4, NA)
x <- set_labels(
  x, 
  labels = c(`1` = "always",
             `4` = "never")
)
str(x)

to_label(x)

to_label(x, add.non.labelled = T)

## ------------------------------------------------------------------------
str(to_factor(efc$e42dep))

str(as.factor(efc$e42dep))

## ------------------------------------------------------------------------
get_labels(as.factor(efc$e42dep), include.values = "p")

get_labels(to_factor(efc$e42dep), include.values = "p")

get_labels(to_factor(efc$e42dep, ref.lvl = 3), include.values = "p")

