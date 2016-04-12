# sjmisc 1.7-3

## Changes to functions

* `to_long` can now also gather columns according to their column indices.
* `merge_df` now optionally merges more than two data frames at once.
* `frq` and `get_frq` now also return frequencies (counts) of character vectors.
* `set_labels` now also works for character vectors.
* `drop_labels` now also works for character vectors.

## Bug fixes

* `merge_df` did not copy all variable and value labels from second data frame.
* `merge_df` did not work when data frames had no matching columns.
* `std_beta` did not work when fitted model had no intercept.
