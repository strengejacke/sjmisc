sjmisc - Miscellaneous Data Management Tools
------------------------------------------------------------------------------
This package contains utility functions that are useful when carrying out data analysis or basic statistical tests, performing common recode and data transformation tasks or working with labelled data (especially intended for people coming from SPSS and/or who are new to R).

Basically, this package covers four domains of functionality:
* reading and writing data between other statistical packages (like SPSS) and R, based on the haven and foreign packages
* hence, this package also includes functions to make working with labelled data easier
* frequently used statistical tests and computation of statistical coefficients, or at least convenient wrappers for such test functions
* frequently applied recoding and variable transformation tasks


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


### Changelog of current development build 1.0.2-8

#### New function
* `add_labels` to set back labels from subsetted data frame, or remove any label attributes from data frames.
* `remove_labels` to remove any label attributes from data frames or vectors.
* `get_values` to return values associated with value labels from labelled vectors.
* `replace_na` to replace `NA`'s with specific value (counterpart to `set_na`).
* `is_even` and `is_odd` to check whether values are odd or even.

#### Changes to functions
* `dicho` now also dichotomizes non-numeric values.
* `rec` now can keep (copy) not yet recoded values with `else=keep`.
* `get_val_labels` gains a `include.values` parameter to also return values associated with the value labels.
* `get_val_labels`, `get_var_labels`, `set_val_labels` and `set_var_labels` now also accept `list`-objects with variables.
* `dicho`, `rec`, `set_na` and `recode_to` now also accept `list`-objects with variables.

#### Bug fixes
* `get_var_labels` returned `NULL` if first variable in `data.frame` had no variable label (but other variables had) - fixed.
* Fixed code with non-exact matching of `haven`-attributes `label` and `labels`, which in certain situation may return wrong vector attributes.
* `rec` did not recode `NA`s into values, if followed by `else`-token.
