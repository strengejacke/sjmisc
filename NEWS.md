# sjmisc 1.6

## New functions

* `rec_pattern` to generate recode patterns for the `rec` function.
* `drop_labels` to drop labels from values with zero-counts.
* `str_contains` to check whether a string contains another string pattern.
* `r2` to compute R-squared values for linear (mixed) models.
* `lbl_df` to create a labelled data frame, and related S3-generic print method for `lbl_df` objects.

## Changes to functions

* `cv` now accepts multiple arguments.
* `icc` now accepts multiple arguments.
* `weight` now also weights character vectors.
* `overdisp` now wraps `AER::dispersiontest` to also support simple glm's.
* Removed deprecated functions.

## Bug fixes

* Fixed bug in `ref_lvl`, where value labels were not correctly re-ordered for factors that had a `0` as level.
* Fixed bug in `rec`, where value labels were not automatically re-ordered when `x` was a numeric factor.
