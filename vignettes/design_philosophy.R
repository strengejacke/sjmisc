## ----collapse=TRUE-------------------------------------------------------
library(sjmisc)
data(efc)

# returns a vector
x <- rec(efc$e42dep, recodes = "1,2=1; 3,4=2")
str(x)

# returns a data frame (a tibble, to be exactly)
rec(efc, e42dep, recodes = "1,2=1; 3,4=2")

## ----collapse=TRUE, echo=FALSE, message=FALSE----------------------------
library(dplyr)

## ----collapse=TRUE-------------------------------------------------------
# select all variables with "cop" in their names, and also
# the range from c161sex to c175empl
rec(efc, ~contains("cop"), c161sex:c175empl, recodes = "0,1=0; else=1")

# center all variables with "age" in name, variable c12hour
# and all variables from column 19 to 21
center(efc, c12hour, ~contains("age"), 19:21)

## ----collapse=TRUE-------------------------------------------------------
to_factor(efc, e42dep, e16sex)

## ----collapse=TRUE-------------------------------------------------------
rec(efc, c82cop1, c83cop2, recodes = "1,2=0; 3:4=2")

## ----collapse=TRUE-------------------------------------------------------
efc %>% 
  rec(c82cop1, c83cop2, recodes = "1,2=0; 3:4=2") %>% 
  add_columns(efc)

## ----collapse=TRUE-------------------------------------------------------
efc %>% 
  select(c82cop1, c83cop2) %>% 
  rec(recodes = "1,2=0; 3:4=2")

efc %>% 
  select(c82cop1, c83cop2) %>% 
  mutate(
    c82cop1_dicho = rec(c82cop1, recodes = "1,2=0; 3:4=2"),
    c83cop2_dicho = rec(c83cop2, recodes = "1,2=0; 3:4=2")
  ) %>% 
  head()

