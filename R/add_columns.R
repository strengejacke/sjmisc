#' @title Add columns to a data frame
#' @name add_columns
#'
#' @description This function combines two or more data frames, but unlike
#'              \code{\link{cbind}} or \code{\link[dplyr]{bind_cols}}, this function
#'              binds \code{data} as last columns of a data frame.
#'
#' @param data A data frame. Will be bound after data frames specified in \code{...}.
#' @param ... More data frames to combine.
#' @param replace Logical, if \code{TRUE} (default), columns in \code{...} with
#'        identical names in \code{data} will replace the columns in \code{data}.
#'        The order of columns after replacing is preserved.
#'
#' @return A data frame, where columns of \code{data} are appended after columns
#'         of \code{...}.
#'
#' @note By default, columns in \code{data} with identical names like columns in one of
#'       the data frames in \code{...} will be dropped (i.e. variables with identical
#'       names in \code{...} will replace existing variables in \code{data}).
#'       Use \code{replace = FALSE} to keep all columns. Identical column names
#'       will then be repaired (see \code{\link[tibble]{repair_names}}). When
#'       replacing columns, replaced columns are not added to the end of the
#'       data frame. Rather, the original order of columns will be preserved.

#'
#' @examples
#' data(efc)
#' d1 <- efc[, 1:3]
#' d2 <- efc[, 4:6]
#'
#' library(dplyr)
#' head(bind_cols(d1, d2))
#' add_columns(d1, d2)
#'
#' # use case: we take the original data frame, select specific
#' # variables and do some transformations or recodings
#' # (standardization in this example) and add the new, transformed
#' # variables *to the end* of the original data frame
#' efc %>%
#'   select(e17age, c160age) %>%
#'   std() %>%
#'   add_columns(efc)
#'
#' # new variables with same name will overwrite old variables
#' # in "efc". order of columns is not changed.
#' efc %>%
#'   select(e16sex, e42dep) %>%
#'   to_factor() %>%
#'   add_columns(efc)
#'
#' # keep both old and new variables, automatically
#' # rename variables with identical name
#' efc %>%
#'   select(e16sex, e42dep) %>%
#'   to_factor() %>%
#'   add_columns(efc, replace = FALSE)
#'
#' @importFrom dplyr bind_cols
#' @importFrom tibble as_tibble repair_names
#' @export
add_columns <- function(data, ..., replace = TRUE) {
  # check for identical column names
  tmp <- dplyr::bind_cols(...)
  doubles <- colnames(tmp) %in% colnames(data)

  # keep order?
  reihenfolge <- c(which(!doubles), which(doubles))

  # remove double column names, if requested
  if (replace && any(doubles)) tmp <- tmp[, !doubles]

  # bind all data
  x <- dplyr::bind_cols(tmp, data)

  # restore order
  if (replace) x <- x[, order(reihenfolge)]

  # repair names - might be necessary when not replacing variables
  if (!replace) x <- tibble::repair_names(x)

  # return a tibble
  tibble::as_tibble(x)
}
