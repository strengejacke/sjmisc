# sjmisc 1.6-5

## New functions

* `merge_df` to fully join labelled data frame and preserve value and variable labels.
* `wtd_sd` to compute weighted standard deviations.
* `wtd_se` to compute weighted standard errors.
* `get_note` and `set_note` to annotate vectors.
* `re_var` to print random effect variances of `merMod`-objects. This function is just a convenient wrapper for `print.icc.lme4` with `comp`-argument.
* `print.labelled` generic method for printing labelled class vectors. Unlike 'haven's print-method, this method also prints variable labels and, if available, vector annotations and missing value attributes.
* S3-method `model.frame.gls`.

## Changes to functions

* `icc` now also returns variance parameters of random effects as attributes.
* `print.icc.lme4` gets a `comp`-argument to also print variance parameters (see `?icc` for details).
* `r2` also computes pseudo-R2 based on random effect variances.
* S3-method `mean.labelled` only prints a message instead of warning, when `x` has labelled missing values.
