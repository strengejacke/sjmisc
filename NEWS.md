# sjmisc 2.8.3

## New functions

* Added `as.data.frame()` for `frq()`.

## Changes to functions

* `typical_value()` now returns the median for integer-values (instead of mean), to preserve the integer-type of a variable.
* The recode-pattern in `rec()` now also works for character variables with whitespaces.
* `rec()` now warns explicetly for possible non-intended multiple assignment of identical new recode-values.
* Improved printing for `frq()`.
* `merge_imputations()` now returns the plot-object as well.
* `to_numeric()` as alias for `to_value()`.

## Bug fixes

* Fixed warning from CRAN checks.
* Fixed errors from CRAN checks.

# sjmisc 2.8.2

## General

* Alias `find_variables()` (alias for `find_var()`) was renamed to `find_in_data()`, to avoid conflicts with package *insight*.
* `rename_variables()` and `rename_columns()` are aliases for `var_rename()`.

## Changes to functions

* `frq()` now also prints frequencies of logical conditions, e.g. how many values are lower or greater than a certain threshold.
* `frq()` gets a `min.frq`-argument, indicating the minimum frequency for which a value will be shown in the output.
* `descr()` gets a `show` argument to show selected columns only.
* `descr()` gets a `file`-argument to write the output as HTML file.
* `var_rename()` now also accepts a named vector with multiple elements as ellipses-argument.

## Bug fixes

* Fixed erroneously warning in `de_mean()`.
* `merge_df()` now removes columns with identical column names inside a data frame before merging, to avoid errors.
* Fixed issue when printing character vectors in `frq()`, where first element was empty, and vectors were not provided as data frame argument.
* Fixed issue in `word_wrap()` when processing expressions.
* Fixed issue in `rec()` with token `rec = "rev"`, when reversing labelled vectors with more value labels than values.

# sjmisc 2.8.1

## General

* `find_variables()` as alias for `find_var()`.
* Revised docs.

## Bug fixes

* Fixed issue with forthcoming update of the **rlang** package.

# sjmisc 2.8.0

## General

* Some print-methods, especially for grouped data frames, are now more compact.

## New functions

* `reshape_longer()`, as alternative to `to_long()`, probably easier to remember (function and argument-names).

## Bug fixes

* `frq()` displayed labels as `NA` in some situations for grouped data frames with more than one group, when data were not labelled.

# sjmisc 2.7.8

## General

* Reduce package dependencies.
* `str_pos()` was renamed into `str_find()`.
* New package-vignette **Recoding Variables**.

## New functions

* `typical_value()`, which was formerly located in package _sjstats_.

## Changes to functions

* `is_whole()` now automatically removes missing values from vectors.
* `is_empty()` now also checks lists with only `NULL`-elements.

## Bug fixes

* Better handling of factors in `merge_imputations()`, which previously could result in `NA`-values when merging imputed values into one variable.
* Fix issue in `is_empty()` in case the vector had non-missing values, but first element of vector was `NA`.
* Fixed bug in `frq()` for grouped data frame, when grouping variable was a character vector. In this case, group titles were mixed up.
* Fix encoding issues in help-files.

# sjmisc 2.7.7

## New functions

* `tidy_values()` to "clean" values (i.e. remove special chars) of character vectors or levels of factors.
* `add_id()` to quickly add an ID variable to (grouped) data frames.

## Changes to functions

* `frq()` gets a `show.na`-argument, to (automatically) show or hide the information for `NA`-values from the output.
* The `weights`-argument in `frq()` now also accepts vectors, and is not limited to variable names. Note that these vectors must be part of a data frame.
* For recode-functions (like `rec()`, `dicho()`, ...), if `suffix = ""` and `append = TRUE`, existing variables will be replaced by the new, recoded variables.
* Improved performance for `group_str()`.
* `var_rename()` now supports quasi-quotation (see Examples).
* `row_sums()` and `row_means()` now return the input data frame when this data frame only had one column and no row means or sums were calculated. The returned data frame still gets the new variable name defined in `var`.

## Bug fixes

* `complete_cases()` returned an empty vector instead of all indexes if all cases (rows) of a data frame were complete.
* Fix issue with `to_dummy()` for character-vector input.
* Fix issue with missing values in `group_str()`.
* Fix issue with grouped data frames in `frq()` when `grp.strings = TRUE`.

# sjmisc 2.7.6

## Changes to functions

* `frq()` gets a `file` and `encoding` argument, to save the HTML output as file.
* `add_variables()` and `move_columns()` now preserve the attributes of a data frame.

# sjmisc 2.7.5

## General

* Reduce package dependencies.

## New functions

* `de_mean()` to compute group-meaned and de-meaned variables.
* `add_variables()` and `add_case()` to add columns or rows in a convenient way to a data frame.
* `move_columns()` to move one or more columns to another position in a data frame.
* `is_num_chr()` to check whether a character vector has only numeric strings.
* `seq_col()` and `seq_row()` as convenient wrapper to create a regular sequence for column or row numbers.

## Changes to functions

* `descr()` gets a `weights`-argument, to print weighted descriptive statistics.
* The `n`-argument in `row_means()` and `row_sums()` now also may be `Inf`, to compute means or sums only if all values in a row are valid (i.e. non-missing).
* Argument `weight.by` in `frq()` was renamed into `weights`.
* `frq()` gets a `title`-argument, to specify an alternative title to the variable label.

## Bug fixes

* `round_num()` preserves data frame attributes.
* `frq()` printed frequencies of grouping-variable for grouped data frames, when `weights` was not `NULL`.
* Fixed issue with wrong title in `frq()` for grouped data frames, when grouping variable was an unlabelled factor.

# sjmisc 2.7.4

## New functions

* `has_na()` to check if variables or observations in a data frame contain `NA`, `NaN` or `Inf` values. Convenient shortcuts for this function are `complete_cases()`, `incomplete_cases()`, `complete_vars()` and `incomplete_vars()`.
* `total_mean()` to compute the overall mean of all values from all columns in a data frame.
* `prcn()` to convert numeric scalars between 0 and 1 into a character-percentage value.
* `numeric_to_factor()` to convert numeric variables into factors, using associated value labels as factor levels.

## Changes to functions

* `set_na()` now also replaces different values per variable into `NA`.
* Changed behaviour of `row_sums()` and missing values. `row_sums()` gets a `n`-argument and now computes row sums if a row has at least `n` non-missing values.

# sjmisc 2.7.3

## General

* A test-suite was added to the package.
* Updated reference in `CITATION` to the publication in the Journal of Open Source Software.

## New functions

* `is_cross_classified()` to check whether two factors are partially crossed.

## Changes to functions

* `ref_lvl()` now also accepts value labels as value for the `lvl`-argument. Additionally, `ref_lvl()` now also works for factor with non-numeric factor levels and simply returns `relevel(x, ref = lvl)` in such cases.

## Bug fixes

* Fixed encoding issues in `rec()` with direct labelling for certain locales.
* Fixed issue in `count_na()`, which did not print labels of tagged `NA` values since the last revision of `frq()`.
* Fixed issue in `merge_imputation()` for cases where original data frame had less columns than imputed data frames.
* Fixed issue in `find_var()` for fuzzy-matching in all elements (i.e. when `fuzzy = TRUE` and `search = "all"`).

# sjmisc 2.7.2

## New functions

* `round_num()` to round only numeric values in a data frame.

## General

* Improved performance for `merge_df()`. Furthermore, `add_rows()` was added as alias for `merge_df()`.
* `merge_df()` resp. `add_rows()` now create a unique `id`-name instead of dropping the ID-variable, in case `id` has the same name of any existing variables in the provided data frames.
* Improved performance for `descr()` and minor changes to the output.

## Support for `mids`-objects (package _mice_)

Following functions now also work on `mids`-objects, as returned by the `mice()`-function:
* `row_count()`, `row_sums()`, `row_means()`, `rec()`, `dicho()`, `center()`, `std()`, `recode_to()` and `to_long()`.

## Changes to functions

* The `weight.by`-argument in `frq()` now should be a variable name from a variable in `x`, and no longer a separate vector.

## Bug fixes

* `descr()` does not work with character vectors, so these are being removed now.

# sjmisc 2.7.1

## General

* Fix typos and revise outdated paragraphs in vignettes.

## New functions

The recoding and transformation functions get scoped variants, allowing to select variables based on logical conditions described in a function:

* `rec_if()` as scoped variant of `rec()`.
* `dicho_if()` as scoped variant of `dicho()`.
* `center_if()` as scoped variant of `center()`.
* `std_if()` as scoped variant of `std()`.
* `split_var_if()` as scoped variant of `split_var()`.
* `group_var_if()` and `group_label_if()` as scoped variant of `group_var()` and `group_label()`.
* `recode_to_if()` as scoped variant of `recode_to()`.
* `set_na_if()` as scoped variant of `set_na()`.

## Changes to functions

* New function `remove_cols()` as alias for `remove_var()`.
* `std()` gets a new robust-option, `robust = "2sd"`, which divides the centered variables by two standard deviations.
* Slightly improved performance for `set_na()`.

## Bug fixes

* `frq()` now removes empty columns before computing frequencies, because applying `frq()` on empty vectors caused an error.
* `empty_cols()` and `empty_rows()` (and hence, `remove_empty_cols()` and `remove_empty_rows()`) caused an error for data frames with only one column resp. row, or if `x` was a vector and no data frame.
* `frq()` now removes missing values from input when weights are applied, to ensure that input and weights have same length.

# sjmisc 2.7.0

## General

* *Breaking changes*: The `append`-argument in recode and transformation functions like `rec()`, `dicho()`, `split_var()`, `group_var()`, `center()`, `std()`, `recode_to()`, `row_sums()`, `row_count()`, `col_count()` and `row_means()` now defaults to `TRUE`.
* The `print()`-method for `descr()` now accepts a `digits`-argument, to specify the rounding of the output.
* Cross refences from `dplyr::select_helpers` were updated to `tidyselect::select_helpers`.

## New functions

* `is_whole()` as counterpart to `is_float()`.

## Changes to functions

* `frq()` now prints variable names for non-labelled data, adds variable names in braces for labelled data and omits the _label_ column for non-labelled data.
* `frq()` now prints mean and standard deviation in the header line of the output.
* `frq()` now gets a `auto.grp`-argument to automatically group variables with many unique values.
* `frq()` now gets a `show.strings`-argument to omit string variables (character vectors) from being printed as frequency table.
* `frq()` now gets a `grp.strings`-argument to group similar string values in the frequency table.
* `frq()` gets an `out`-argument, to print output to console, or as HTML table in the viewer or web browser.
* `descr()` gets an `out`-argument, to print output to console, or as HTML table in the viewer or web browser.

## Bug fixes

* `is_empty()` returned `TRUE` for single vectors with `NA` being the first element.
* Fix issue where due to a bug during code cleanup, `remove_empty_rows()` did no longer remove empty rows, but columns.

# sjmisc 2.6.3

## General

* Revised examples that used removed methods from other packages.
* Use select-helpers from package *tidyselect*, instead of *dplyr*.
* Beautiful colored output for `frq()`, `descr()` and `flat_table()`.

## Changes to functions

* `rec()` now also recodes doubles with floating points, if a range of values is specified.
* `std()` and `center()` now use `include.fac = FALSE` as default option.
* `std()` gets a `robust`-argument, to divide variables either by standard deviation, or - in case of asymmetrically distributed variables - median absolute deviation or Gini's mean difference.
* `frq()` now shows total and valid N in output.

## Bug fixes

* `center()`, `std()`, `dicho()`, `split_var()` and `group_var()` did not work correctly for grouped data frames.
* `frq()` did not print multiple variables when applied on grouped data frames.

# sjmisc 2.6.2

## Changes to functions

* Arguments `as.df` and `as.varlab` in function `find_var()` are now deprecated. Please use `out` instead.
* `rotate_df()` preserves attributes.
* `is_float()` is now exported as function.

## Bug fixes

* Fixed bug for `to_label()`, when `x` was a character vector and argument `drop.levels` was `TRUE`.

# sjmisc 2.6.1

## General

* Fixed issue with latest tidyr-update on CRAN.

## Bug fixes

* `frq()` did not correctly calculate valid and cumulative percentages when using weights.

# sjmisc 2.6.0

## General
* All labelled-data functions were removed and are now in package *sjlabelled*.

## New functions

* `remove_var()` as pipe-friendly function to remove variables from data frames.
* `var_type()` as pipe-friendly function to determine the type of variables.
* `all_na()` to check whether a vector only consists of NA values.
* `rotate_df()` to rotate data frames (switch columns and rows).
* `shorten_string()`, to shorten strings to a certain maxium number of chars.

## Changes to functions

* Following functions now also work on grouped data frames: `dicho()`, `split_var()`, `group_var()`, `std()` and `center()`.
* Argument `groupcount` in `split_var()`, `group_var()` and `group_labels()` is now named `n`.
* Argument `groupsize` in `group_var()` and `group_labels()` is now named `size`.
* `frq()` gets a revised print-method, which does not print the result to console when captured in an object (i.e., `x <- frq(x)` no longer prints the result).
* `frq()` no longer prints (redundant) labels for factors w/o value label attributes.
* `frq()` adds information about the variable type in the table caption (only for variables with variable labels).
* `frq()` adds information about groups when printing grouped, non-labelled variables.
* `descr()` now also prints information about the variable type.
* `to_character()` now preserves variable labels.

# sjmisc 2.5.0

## General

* **sjmisc** now uses dplyr's [tidyeval-approach](http://dplyr.tidyverse.org/articles/programming.html) to evaluate arguments. This means that the select-helper-functions (like `one_of()` or `contains()`) no longer need to be prefixed with a `~` when used as argument within **sjmisc**-functions.
* All labelled-data functions are now deprecated and will become defunct in future package versions. The labelled-data functions have been moved into a separate package, *sjlabelled*.

## New functions

* `row_count()` to count specific values in a data frame per observation.
* `col_count()` to count specific values in a data frame per variable.
* `str_start()` and `str_end()` to find starting and end indices of patterns inside strings.

## Changes to functions

* The output for `frq()` now always includes a `NA`-row, but no longer prints a value for the `NA`-row.
* `merge_imputations()` gets a `summary`-argument to plot a graphical summary of the quality of the merging process.

## Bug fixes

* `add_columns()` and `replace_columns()` crashed R when no data frame was specified in `...`-ellipses argument.
* `descr()` and `frq()` used wrong variable labels when processing grouped data frames for specific situations, where the grouping variable had no sequences values.
* `descr()` did not work for large data frames, because internally, because `psych::describe()` switched to fast mode by default then (removing columns from the output).
