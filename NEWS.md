# sjmisc 1.8.0-3

## General

* All statistical functions have been removed and are now in a separate package, `sjstats` (https://cran.r-project.org/package=sjstats).
* Removed some S3-methods for `labelled`-class, as these are now provided by the haven-package.

## New functions

* Added `to_character` method.

## Bug fixes

* `is_empty` returned `NA` instead of `TRUE` for empty character vectors.
* Fixed bug with erroneous assignment of value labels to subset data when using `copy_labels` ([#20](https://github.com/sjPlot/sjmisc/issues/20))
