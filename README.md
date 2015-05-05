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

#### Changes to functions
* Functions `std_beta` and `cv` now support `merModLmerTest` objects (fitted by `lmerTest` package).
* `mean_n` has a `digit` parameter to round returned mean values.

#### Bug fixes
* `set_na` did not work with logical vectors - fixed.
* `recode_to` did not work when `var` had value-label-attributes - fixed.