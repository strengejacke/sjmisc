# sjmisc 1.8.0-3

## General

* All statistical functions have been removed and are now in a separate package, `sjstats` (https://cran.r-project.org/package=sjstats).

## New functions
* Added `to_character` method.

## Bug fixes

* `is_empty` returned `NA` instead of `TRUE` for empty character vectors.
