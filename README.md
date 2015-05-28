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
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/sjmisc)](http://cran.r-project.org/web/packages/sjmisc)
&#160;&#160;
[![downloads](http://cranlogs.r-pkg.org/badges/sjmisc)](http://cranlogs.r-pkg.org/)

To install the latest stable release from CRAN, type following command into the R console:

```r
install.packages("sjmisc")
```

### References, documentation and examples

- [Documentation and examples](http://www.strengejacke.de/sjPlot/)


### Citation

In case you want / have to cite my package, please use `citation('sjmisc')` for citation information. 


### Changelog of current development build 1.0.2-6

#### New function
* `add_labels` to set back labels from subsetted data frame, or remove any label attributes from data frames.
* `remove_labels` to remove any label attributes from data frames or vectors.
* `get_values` to return values associated with value labels from labelled vectors.

#### Changes to functions
* `dicho` now also dichotomizes non-numeric values.
* `get_val_labels` gains a `include.values` parameter to also return values associated with the value labels.

#### Bug fixes
* `get_var_labels` returned `NULL` if first variable in `data.frame` had no variable label (but other has) - fixed.
* Fixed code with non-exact matching of `haven`-attributes `label` and `labels`, which in certain situation may return wrong vector attributes.
