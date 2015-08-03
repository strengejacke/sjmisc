## ----collapse=TRUE-------------------------------------------------------
factor(c("low", "high", "mid", "high", "low"))

## ----collapse=TRUE-------------------------------------------------------
library(sjmisc)
data(efc)
str(efc$e42dep)

## ----fig.width=6, fig.height=4, dpi=72, dev="png", warning=FALSE---------
library(sjmisc)
data(efc)
barplot(table(efc$e42dep, efc$e16sex), 
        beside = T, 
        legend.text = T)

## ----fig.width=6, fig.height=4, dpi=72, dev="png", warning=FALSE---------
barplot(table(to_label(efc$e42dep),
              to_label(efc$e16sex)), 
        beside = T, 
        legend.text = T)

## ----fig.width=6, fig.height=4, dpi=72, dev="png", warning=FALSE---------
barplot(table(to_label(efc$e42dep),
              to_label(efc$e16sex)), 
        beside = T, 
        legend.text = T,
        main = get_label(efc$e42dep))

## ----collapse=TRUE-------------------------------------------------------
library(sjPlot)
data(efc)
# make education categorical
efc$c172code <- to_factor(efc$c172code)
fit <- lm(barthtot ~ c160age + c12hour + c172code + c161sex, 
          data = efc)

## ----eval=FALSE, collapse=TRUE-------------------------------------------
#  sjt.lm(fit, group.pred = TRUE)

## ---- collapse=TRUE, message=FALSE---------------------------------------
efc.sub <- subset(efc, 
                  subset = e16sex == 1, 
                  select = c(4:8))
str(efc.sub)

## ---- collapse=TRUE------------------------------------------------------
efc.sub <- copy_labels(efc.sub, efc)
str(efc.sub)

