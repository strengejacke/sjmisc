# sjmisc 2.3.1.9000

## General

* Argument `value` in `set_na()` is deprecated. Please use `na` instead.
* Argument `recodes` in `rec()` is deprecated. Please use `rec` instead.
* Argument `lab` in `set_label()` is deprecated. Please use `label` instead.
* Argument `value` in `add_labels()` and `replace_labels()` is deprecated. Please use `labels` instead.
* Argument `value` in `ref_lvl()` is deprecated. Please use `lvl` instead.

## New functions

* `row_sums()` as wrapper of `rowSums()` to compute row sums, but works within pipe-workflow and with select-helpers for variables.
* `row_means()` as wrapper of `sjstats::mean_n()` to compute row means, but works within pipe-workflow and with select-helpers for variables.
* `%nin%` as complement to `%in%`.

## Bug fixes

* `remove_empty_cols()` returned an empty data frame, when input data frame had no empty columns.
* `remove_empty_rows()` returned an empty data frame, when input data frame had no empty rows.
* `add_columns()` and `repair_columns()` in some cases coerced data frames of class `data.frame` with only one column into a vector, which gave an error when binding columns.

# sjmisc 2.3.1

## General

* Re-exports `magrittr::%>%` (Bob Rudis said more packages should do so).

## New functions

* `replace_columns()` to replace variables in one data frame with variables from other data frames.

## Changes to functions

* `descr()` gets a `max.length`-argument to shorten variable labels in the output to a specific number of chars.
* `descr()` now also reports the percentage of missing values.
* `set_na()` no longer gives a warning when trying to replace values with `NA` for vectors that completely contained `NA`s.

## Bug fixes

* `descr()` now also works on single vectors as data argument.
* Fixed bugs with `write_*()`-functions.

# sjmisc 2.3.0

## General

* Added package-vignettes.
* Functions were largely revised to work seamlessly within the tidyverse. This may break existing code, but in the long run, consistent api-design makes working with the package more intuitive. See `vignette("design_philosophy", "sjmisc")` for more details.
* `as_labelled()` only converts vectors into `labelled`-class if vector has label attributes. This ensures that data can be properly saved into other formats, e.g. with `write_spss()`.
* The `write_*()`-functions have been revised and should now save data frame with any common classes of vectors (labelled, factor, numeric, atomic...).

## New functions

* `center()` and `std()` are moving from package `sjstats` to `sjmisc`.
* `add_columns()` to bind columns of first data frame at the end of all data frames.

## Changes to functions

* `frq()` now has the same argument-structure as `flat_table()`.
* Following functions now follow a consistent tidyverse-approach, with the data being the first argument, followed by variable names: `add_labels()`, `replace_labels()`, `remove_labels()`, `count_na()`, `rec()`, `dicho()`, `split_var()`, `drop_labels()`, `fill_labels()`, `group_var()`, `group_labels()`, `ref_lvl()`, `recode_to()`, `replace_na()`, `set_na()` and `set_labels()`.
* `get_values()` now sorts returned values by default, to be consistent with `get_labels()`.
* `spread_coef()` gets arguments `se` and `p.val`, to define whether standard errors and p-values should be included in the return value or not.

## Bug fixes

* `merge_df()` did not copy label attributes for data frame with identical variables (that were row-bound).
* `to_value()` did not work for character vectors of class labelled, with empty values (which typically have no value label).

# sjmisc 2.2.1

## Bug fixes

* The `sort.frq` did not work `frq()`.

# sjmisc 2.2.0

## New functions

* `zap_inf()` to "clean" vectors from `NaN` and infinite values.
* `descr()` to provide basic descriptive statistics (similar to `describe()` in the psych-package), but including variable labels and usable in pipe-workflows. Also works with grouped data frames.

## Changes to functions

* `rec()`, `split_var()` and `dicho()` get an argument `suffix`, to append a suffix to variable (column) names, if applied on a data frame.
* Value labels in `rec()` can now directly be assigned inside the `recodes`-syntax (see 'Details' in `?rec`).
* `find_var()` gets a `as.df`-argument, to return a data frame with matching variables, instead of their column indices only.
* `find_var()` gets a `as.varlab`-argument, to return a "summary" data frame with column number, variable name and variable label.
* `flat_table()` now also accepts grouped data frames.
* `flat_table()` gets a `show.values`-argument, to add values to associated labels in output.
* `frq()` now also accepts grouped data frames.
* `frq()` gets a `weight.by`-argument to weight frequencies.
* `set_na()` can now also find values by their value labels and replace them with NA.
* `set_na()` now removes unused value labels from values that have been replaced with NA.
* The `as.tag`-argument in `set_na()` now defaults to `FALSE`.
* `get_labels()` now always returns labels in sorted order of the associated values.
* `get_labels()` gets a `drop.unused`-argument, to automatically drop labels from values that don't occur in the vector.
* For a named vector as `labels`-argument, `set_labels()` now always sorts labels in sorted order of the associated values.
* `is_empty()` gets a `first.only`-argument, to evaluate either first or all elements of a character vector.

## Bug fixes

* `set_na()` did not work on vectors of class `Date` when argument `as.tag = TRUE`.
* `flat_table()` did not show values that had no value labels. Now all categories are shown in the frequency table.
* `rec()` did not properly copy labels of tagged NA values when not all recoded values appeared in the vector.
* `frq()` did not show correct values, when value labels of a vector were not sorted according their values.
* `set_labels()` did not set labels properly for ordered factors.
* `remove_labels()` returned NA-values for value labels (instead of no value labels) when the last value label of a vector was removed.


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
* Fixed bug with erroneous assignment of value labels to subset data when using `copy_labels()` ([#20](https://github.com/strengejacke/sjmisc/issues/20))
