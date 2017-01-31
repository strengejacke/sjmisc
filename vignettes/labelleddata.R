## ------------------------------------------------------------------------
factor(c("low", "high", "mid", "high", "low"))

## ------------------------------------------------------------------------
library(sjmisc)
data(efc)
str(efc$e42dep)

## ----warning=FALSE, fig.height=6, fig.width=7----------------------------
library(sjmisc)
data(efc)
barplot(
  table(efc$e42dep, efc$e16sex), 
  beside = T, 
  legend.text = T
)

## ----warning=FALSE, fig.height=6, fig.width=7----------------------------
barplot(
  table(to_label(efc$e42dep),
        to_label(efc$e16sex)), 
  beside = T, 
  legend.text = T
)

## ----warning=FALSE, fig.height=6, fig.width=7----------------------------
barplot(
  table(to_label(efc$e42dep),
        to_label(efc$e16sex)), 
  beside = T, 
  legend.text = T,
  main = get_label(efc$e42dep)
)

## ----collapse=TRUE-------------------------------------------------------
library(sjPlot)
data(efc)
# make education categorical
efc$c172code <- to_factor(efc$c172code)
fit <- lm(barthtot ~ c160age + c12hour + c172code + c161sex, data = efc)

## ----eval=FALSE, collapse=TRUE-------------------------------------------
#  sjt.lm(fit, group.pred = TRUE)

## ------------------------------------------------------------------------
efc.sub <- subset(efc, subset = e16sex == 1, select = c(4:8))
str(efc.sub)

## ---- message=FALSE------------------------------------------------------
efc.sub <- copy_labels(efc.sub, efc)
str(efc.sub)

