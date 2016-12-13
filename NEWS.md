# sjmisc 2.2.0

## New functions

* `zap_inf()` to "clean" vectors from `NaN` and infinite values.
* `descr()` to provide basic descriptive statistics (similar to `describe()` in the psych-package), but including variable labels and usable in pipe-workflows.

## Changes to functions

* The `as.tag`-argument in `set_na()` now defaults to `FALSE`.
* `rec()`, `split_var()` and `dicho()` get an argument `suffix`, to append a suffix to variable (column) names, if applied on a data frame.
* `find_var()` gets a `as.df`-argument, to return a data frame with matching variables, instead of their column indices only.
* `find_var()` gets a `as.varlab`-argument, to return a "summary" data frame with column number, variable name and variable label.
* `flat_table()` gets a `show.values`-argument, to add values to associated labels in output.
* `frq()` gets a `weight.by`-argument to weight frequencies.
* Value labels in `rec()` can now directly be assigned inside the `recodes`-syntax (see 'Details' in `rec()`).
* `get_labels()` now always returns labels in sorted order of the associated values.
* For a named vector as `labels`-argument, `set_labels()` now always sorts labels in sorted order of the associated values.

## Bug fixes

* `set_na()` did not work on vectors of class `Date` when argument `as.tag = TRUE`.
* `flat_table()` did not show values that had no value labels. Now all categories are shown in the frequency table.
* `rec()` did not properly copy labels of tagged NA values when not all recoded values appeared in the vector.
* `frq()` did not show correct values, when value labels of a vector were not sorted according their values.
* `set_labels()` did not set labels properly for ordered factors.

# sjmisc 2.1.0

## New functions

* `find_var()` to find variables in data frames by name or label.
* `var_labels()` as "tidyversed" alternative to `set_label()` to set variable labels.
* `var_rename()` to rename variables.

## Changes to functions

* Following functions now get an ellipses-argument `...`, to apply function only to selected variables, but return the complete data frame (thus, overwriting existing variables in a data frame, if requested): `to_factor()`, `to_value()`, `to_label()`, `to_character()`, `to_dummy()`, `zap_labels()`, `zap_unlabelled()`, `zap_na_tags()`.

## Bug fixes

* Fixed bug with copying attributes from tibbles for `merge_df()`.
* Fixed wrong argument-description in docs of `frq()`.

# sjmisc 2.0.1

## General

* Removed package `coin` from Imports.

## New functions

* `count_na()` to print a frequency table of tagged NA values.

## Changes to functions

* `set_na()` gets a `drop.levels` argument to keep or drop factor levels of values that have been replaced with NA.
* `set_na()` gets a `as.tag` argument to set NA values as regular or tagged NA.


# sjmisc 2.0.0

## General

* **sjmisc** now supports _tagged_ `NA` values, a new structure for labelled missing values introduced by the [haven-package](https://cran.r-project.org/package=haven). This means that functions or arguments that are no longer useful, have been removed while other functions dealing with NA values have been largely revised.
* All statistical functions have been removed and are now in a separate package, [sjstats](https://cran.r-project.org/package=sjstats).
* Removed some S3-methods for `labelled`-class, as these are now provided by the haven-package.
* Functions no longer check input for type `matrix`, to avoid conflicts with scaled vectors (that were recognized as matrix and hence treated as data frame).
* `table(*, exclude = NULL)` was changed to `table(*, useNA = "always")`, because of planned changes in upcoming R version 3.4.
* More functions (like `trim()` or `frq()`) now also have data frame- or list-methods.

## New functions

* `zap_na_tags()` to turn tagged NA values into regular NA values.
* `spread_coef()` to spread coefficients of multiple fitted models in nested data frames into columns.
* `merge_imputations()` to find the most likely imputed value for a missing value.
* `flat_table()` to print flat (proportional) tables of labelled variables.
* Added `to_character()` method.
* `big_mark()` to format large numbers with big marks.
* `empty_cols()` and `empty_rows()` to find variables or observations with exclusively NA values in a data frame.
* `remove_empty_cols()` and `remove_empty_rows()` to remove variables or observations with exclusively NA values from a data frame.

## Changes to functions
* `str_contains()` gets a `switch` argument to switch the role of `x` and `pattern`.
* `word_wrap()` coerces vectors to character if necessary.
* `to_label()` gets a `var.label` and `drop.levels` argument, and now preserves variable labels by default.
* Argument `def.value` in `get_label()` now also applies to data frame arguments.
* If factor levels are numeric and factor has value labels, these are used in `to_value()` by default.
* `to_factor()` no longer generates `NA` or `NaN`-levels when converting input into factors.

## Bug fixes
* `rec()` did not recode values, when these were the first element of a multi-line string of the `recodes` argument.
* `is_empty()` returned `NA` instead of `TRUE` for empty character vectors.
* Fixed bug with erroneous assignment of value labels to subset data when using `copy_labels()` ([#20](https://github.com/sjPlot/sjmisc/issues/20))
