# sjmisc 1.5

## New functions

* `to_long` to gather (transform from wide to long format) multiple column groups at once.
* S3-method `model.matrix` for `nlme::gls` class.


## Changes to functions

* `std_beta` now supports `nlme::gls` models.


## Bug fixes

* `word_wrap` now removes `NA` values from `labels` before wrapping strings.
* `set_na` now drops unused factor levels that have been replaced with `NA` values ([#15](https://github.com/sjPlot/sjmisc/issues/15)).
