# sjmisc 2.0.0

## General

* **sjmisc** now supports _tagged_ `NA` values, a new structure for labelled missing values introduced by the [haven-package](https://cran.r-project.org/package=haven). This means that functions or arguments that are no longer useful, have been removed while other functions dealing with NA values have been largely revised.
* All statistical functions have been removed and are now in a separate package, [sjstats](https://cran.r-project.org/package=sjstats).
* Removed some S3-methods for `labelled`-class, as these are now provided by the haven-package.
* Functions no longer check input for type `matrix`, to avoid conflicts with scaled vectors (that were recognized as matrix and hence treated as data frame).
* `table(*, exclude = NULL)` was changed to `table(*, useNA = "always")`, because of planned changes in upcoming R version 3.4.

## New functions

* `zap_na_tags` to turn tagged `NA` values into regular `NA` values.
* `spread_coef` to spread coefficients of multiple fitted models in nested data frames into columns.
* `merge_imputations` to find the most likely imputed value for a missing value.
* Added `to_character` method.

## Changes to functions

* `str_contains` gets a `switch` argument to switch the role of `x` and `pattern`.
* `word_wrap` coerces vectors to character if necessary.
* `to_label` gets a `var.label` and `drop.levels` argument, and now preserves variable labels by default.

## Bug fixes

* `rec` did not recode values, when these were the first element of a multi-line string of the `recodes` argument.
* `is_empty` returned `NA` instead of `TRUE` for empty character vectors.
* Fixed bug with erroneous assignment of value labels to subset data when using `copy_labels` ([#20](https://github.com/sjPlot/sjmisc/issues/20))
