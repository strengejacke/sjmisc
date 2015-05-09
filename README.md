sjmisc - Miscellaneous Data Management Tools
------------------------------------------------------------------------------
This package contains some tools that are useful when carrying out data analysis or interpreting data (especially intended for people coming from SPSS and/or who are new to R). These tool functions support reading and writing data (SPSS, SAS and STATA), variable recoding and weighting, statistical tests, reliability tests and much more.


### Installation

#### Latest development build

To install the latest development snapshot (see latest changes below), type following commands into the R console:

```r
library(devtools)
devtools::install_github("sjPlot/sjmisc")
```

#### Officiale, stable release
To install the latest stable release from CRAN, type following command into the R console:

```r
install.packages("sjmisc")
```

### References, documentation and examples

- [Documentation and examples](http://www.strengejacke.de/sjPlot/)


### Citation

In case you want / have to cite my package, please use `citation('sjmisc')` for citation information. 


### Changelog of current development build 1.0.2

#### New functions
* `icc` to compute intraclass-correlations for random-intercepts of mixed models.
* `hoslem_gof` to perform a Hosmer-Lemeshow-Goodness-of-Fit-test for logistic regression models.
* `cod` to compute the Coefficient of Discrimination, aka Tjur's Pseudo-R2, for logistic regression models.
* `pseudo_r2` to compute the Nagelkerke's and Cox-Snell's Pseudo-R2 for logistic regression models.

#### Changes to functions
* Functions `std_beta` and `cv` now support `merModLmerTest` objects (fitted by `lmerTest` package).
* `mean_n` has a `digit` parameter to round returned mean values.
* `rec` and `recode_to` now also accept data frames as parameter.
* `chisq_gof` now accepts `glm`-objects, however, computing the Chi-squared-Goodness-of-Fit-test for logistic regression models sometime may fail.

#### Bug fixes
* `set_na` did not work with logical vectors - fixed.
* `recode_to` did not work when `var` had value-label-attributes - fixed.