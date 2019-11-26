#' @title Add or replace data frame columns
#' @name add_columns
#'
#' @description \code{add_columns()} combines two or more data frames, but unlike
#'   \code{\link{cbind}} or \code{\link[dplyr:bind]{dplyr::bind_cols()}}, this function
#'   binds \code{data} as last columns of a data frame (i.e., behind columns
#'   specified in \code{...}). This can be useful in a "pipe"-workflow, where
#'   a data frame returned by a previous function should be appended
#'   \emph{at the end} of another data frame that is processed in
#'   \code{add_colums()}.
#'   \cr \cr
#'   \code{replace_columns()} replaces all columns in \code{data} with
#'   identically named columns in \code{...}, and adds remaining (non-duplicated)
#'   columns from \code{...} to \code{data}.
#'   \cr \cr
#'   \code{add_id()} simply adds an ID-column to the data frame, with values
#'   from 1 to \code{nrow(data)}, respectively for grouped data frames, values
#'   from 1 to group size. See 'Examples'.
#'
#' @param data A data frame. For \code{add_columns()}, will be bound after data
#'   frames specified in \code{...}. For \code{replace_columns()}, duplicated
#'   columns in \code{data} will be replaced by columns in \code{...}.
#' @param ... More data frames to combine, resp. more data frames with columns
#'   that should replace columns in \code{data}.
#' @param replace Logical, if \code{TRUE} (default), columns in \code{...} with
#'   identical names in \code{data} will replace the columns in \code{data}.
#'   The order of columns after replacing is preserved.
#' @param add.unique Logical, if \code{TRUE} (default), remaining columns in
#'   \code{...} that did not replace any column in \code{data}, are appended
#'   as new columns to \code{data}.
#' @param var Name of new the ID-variable.
#'
#' @return For \code{add_columns()}, a data frame, where columns of \code{data}
#'   are appended after columns of \code{...}.
#'   \cr \cr
#'   For \code{replace_columns()}, a data frame where columns in \code{data}
#'   will be replaced by identically named columns in \code{...}, and remaining
#'   columns from \code{...} will be appended to \code{data} (if
#'   \code{add.unique = TRUE}).
#'   \cr \cr
#'   For \code{add_id()}, a new column with ID numbers. This column is always
#'   the first column in the returned data frame.
#'
#' @note For \code{add_columns()}, by default, columns in \code{data} with
#'   identical names like columns in one of the data frames in \code{...}
#'   will be dropped (i.e. variables with identical names in \code{...} will
#'   replace existing variables in \code{data}). Use \code{replace = FALSE} to
#'   keep all columns. Identical column names will then be renamed, to ensure
#'   unique column names (which happens by default when using
#'   \code{\link[dplyr:bind]{dplyr::bind_cols()}}). When replacing columns, replaced columns
#'   are not added to the end of the data frame. Rather, the original order of
#'   columns will be preserved.
#'
#' @examples
#' data(efc)
#' d1 <- efc[, 1:3]
#' d2 <- efc[, 4:6]
#'
#' library(dplyr)
#' head(bind_cols(d1, d2))
#' add_columns(d1, d2) %>% head()
#'
#' d1 <- efc[, 1:3]
#' d2 <- efc[, 2:6]
#'
#' add_columns(d1, d2, replace = TRUE) %>% head()
#' add_columns(d1, d2, replace = FALSE) %>% head()
#'
#' # use case: we take the original data frame, select specific
#' # variables and do some transformations or recodings
#' # (standardization in this example) and add the new, transformed
#' # variables *to the end* of the original data frame
#' efc %>%
#'   select(e17age, c160age) %>%
#'   std() %>%
#'   add_columns(efc) %>%
#'   head()
#'
#' # new variables with same name will overwrite old variables
#' # in "efc". order of columns is not changed.
#' efc %>%
#'   select(e16sex, e42dep) %>%
#'   to_factor() %>%
#'   add_columns(efc) %>%
#'   head()
#'
#' # keep both old and new variables, automatically
#' # rename variables with identical name
#' efc %>%
#'   select(e16sex, e42dep) %>%
#'   to_factor() %>%
#'   add_columns(efc, replace = FALSE) %>%
#'   head()
#'
#' # create sample data frames
#' d1 <- efc[, 1:10]
#' d2 <- efc[, 2:3]
#' d3 <- efc[, 7:8]
#' d4 <- efc[, 10:12]
#'
#' # show original
#' head(d1)
#'
#' library(sjlabelled)
#' # slightly change variables, to see effect
#' d2 <- as_label(d2)
#' d3 <- as_label(d3)
#'
#' # replace duplicated columns, append remaining
#' replace_columns(d1, d2, d3, d4) %>% head()
#'
#' # replace duplicated columns, omit remaining
#' replace_columns(d1, d2, d3, d4, add.unique = FALSE) %>% head()
#'
#' # add ID to dataset
#' library(dplyr)
#' data(mtcars)
#' add_id(mtcars)
#'
#' mtcars %>%
#'   group_by(gear) %>%
#'   add_id() %>%
#'   arrange(gear, ID) %>%
#'   print(n = 100)
#' @importFrom dplyr bind_cols
#' @export
add_columns <- function(data, ..., replace = TRUE) {
  # evaluate dots
  .dots <- match.call(expand.dots = FALSE)$`...`

  # if add_columns had no arguments, .dots are NULL
  # this crashes R when calling bind_cols
  if (is.null(.dots)) {
    stop("You must specify at least one more data frame with columns to add.", call. = F)
  }

  # check for identical column names
  tmp <- dplyr::bind_cols(...)
  doubles <- colnames(tmp) %in% colnames(data)

  # keep order?
  reihenfolge <- c(which(!doubles), which(doubles))

  # remove duplicate column names, if requested
  if (replace && any(doubles)) tmp <- tmp[, !doubles, drop = FALSE]

  # bind all data
  x <- dplyr::bind_cols(tmp, data)

  # restore order
  if (replace) {
    # check for correct length. if "data" had duplicated variables,
    # but not all variable are duplicates, add indices of regular values
    if (ncol(x) > length(reihenfolge)) {
      # get remaining indices
      xl <- seq_len(ncol(x))[-seq_len(length(reihenfolge))]
      # add to "reihefolge"
      reihenfolge <- c(reihenfolge, xl)
    }
    # sort data frame
    x <- x[, order(reihenfolge)]
  }

  x
}


#' @rdname add_columns
#' @export
replace_columns <- function(data, ..., add.unique = TRUE) {
  # evaluate dots
  .dots <- match.call(expand.dots = FALSE)$`...`

  # if add_columns had no arguments, .dots are NULL
  # this crashes R when calling bind_cols
  if (is.null(.dots)) {
    stop("You must specify at least one more data frame with columns to add.", call. = F)
  }

  # bind all data frames to one
  tmp <- dplyr::bind_cols(...)

  # check for identical column names
  data.doubles <- colnames(data) %in% colnames(tmp)
  tmp.doubles <- colnames(tmp) %in% colnames(data)

  # replace duplicate variables in "data" with duplicates from "..."
  data[, data.doubles] <- tmp[, tmp.doubles, drop = FALSE]

  # add remaining columns that were not duplicates
  if (add.unique)
    x <- dplyr::bind_cols(data, tmp[, !tmp.doubles, drop = FALSE])
  else
    x <- data

  x
}


#' @importFrom dplyr bind_cols row_number mutate
#' @rdname add_columns
#' @export
add_id <- function(data, var = "ID") {
  if (!is.data.frame(data))
    stop("`data` must be a data frame.", call. = FALSE)

  x <- dplyr::mutate(data, id = dplyr::row_number())
  colnames(x)[ncol(x)] <- var
  dplyr::bind_cols(x[, ncol(x), drop = FALSE], data)
}
