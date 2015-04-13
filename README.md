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


### Changelog of current development build 1.0.0-4

#### New functions
* `is_crossed` to check whether two factors are crossed.
* `is_nested` to check whether two factors are nested.

#### Changes to functions
* `std_beta` now accepts `plm`-objects.
* `to_value` now auto-detects lowest minimum value of numeric factors, instead of always setting minimum value to 1.

#### Bug fixes
* `std_beta` did not work in some cases - fixed.
* `mwu` did not accept factors as grouping levels - fixed.
* `mwu` did not work when value range of `grp` vector was not continuously - fixed.
* `cv` did not work with `lme`-objects (from `nlme`-package) - fixed.
* `cramer`, `phi` and `table_values` did not work with tables of class `xtabs` - fixed.
* `to_value` (resp. `set_val_labels`) did not work in certain situations when factor had not continuously numeric levels - fixed.
