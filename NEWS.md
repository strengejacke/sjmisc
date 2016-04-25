# sjmisc 1.7-4

## Changes to functions

* `to_long` can now also gather columns according to their column numbers.
* `merge_df` now optionally merges more than two data frames at once.
* `frq` and `get_frq` now also return frequencies (counts) of character vectors.
* `rec` now also works for character vectors and non-numeric factors.
* `set_labels` now also works for character vectors.
* `drop_labels` now also works for character vectors.
* `to_value` now keeps labels of character vectors.
* `to_label` now also works for character vectors and non-numeric factors.
* `mwu` now also works when `grp` is a character vector.
* Generally, a better support for character vectors in label functions.
* Argument `enc` now also applies to `read_spss` for haven-option.

## Bug fixes

* `merge_df` did not copy all variable and value labels from second data frame.
* `merge_df` did not work when data frames had no matching columns.
* `std_beta` did not work when fitted model had no intercept.
* `set_labels` now also works correctly for ordered factors.
