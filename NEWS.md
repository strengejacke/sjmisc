# sjmisc 1.5.1-3

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