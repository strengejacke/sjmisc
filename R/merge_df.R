#' @title Merge labelled data frames
#' @name add_rows
#'
#' @description Merges (full join) data frames and preserve value and variable labels.
#'
#' @param ... Two or more data frames to be merged.
#' @param id Optional name for ID column that will be created to indicate the
#'          source data frames for appended rows.
#'
#' @return A full joined data frame.
#'
#' @details This function works like \code{\link[dplyr:bind]{dplyr::bind_rows()}}, but preserves
#'   variable and value label attributes. \code{add_rows()} row-binds all data
#'   frames in \code{...}, even if these have different numbers of columns.
#'   Non-matching columns will be column-bound and filled with \code{NA}-values
#'   for rows in those data frames that do not have this column.
#'   \cr \cr
#'   Value and variable labels are preserved. If matching columns have
#'   different value label attributes, attributes from first data frame
#'   will be used.
#'   \cr \cr
#'   \code{merge_df()} is an alias for \code{add_rows()}.
#'
#' @examples
#' library(dplyr)
#' data(efc)
#' x1 <- efc %>% select(1:5) %>% slice(1:10)
#' x2 <- efc %>% select(3:7) %>% slice(11:20)
#'
#' mydf <- add_rows(x1, x2)
#' mydf
#' str(mydf)
#'
#' \dontrun{
#' library(sjPlot)
#' view_df(mydf)}
#'
#' x3 <- efc %>% select(5:9) %>% slice(21:30)
#' x4 <- efc %>% select(11:14) %>% slice(31:40)
#'
#' mydf <- add_rows(x1, x2, x3, x4, id = "subsets")
#' mydf
#' str(mydf)
#'
#' @importFrom dplyr bind_rows
#' @importFrom purrr map flatten compact flatten_chr
#' @export
add_rows <- function(..., id = NULL) {

  # get column names of all data frames and make sure that ID
  # variable has unique column name
  cnames <- purrr::map(list(...), ~ colnames(.x)) %>%
    purrr::flatten_chr()

  if (!is.null(id) && id %in% cnames) {
    id <- make.unique(c(cnames, id))[length(cnames) + 1]
    warning(sprintf("Value of `id` already exists as column name. ID column was renamed to `%s`.", id), call. = F)
  }

  # remove variables with duplicated names

  dat <- lapply(list(...), function(d) {
    d[, unique(names(d)), drop = FALSE]
  })


  # bind all data frames

  x <- dplyr::bind_rows(dat, .id = id)


  # get attributes from all variables of original data frame
  # and restore these attributes to the final merged data frame
  # (bind_rows() currently drops attributes)

  at <- purrr::map(list(...), function(x) {
    purrr::map(x, ~ attributes(.x))
  }) %>%
    purrr::flatten() %>%
    purrr::compact()


  if (!sjmisc::is_empty(at)) {

    # make sure attributes from duplicated variables
    # are removed

    at <- at[!duplicated(at)]

    for (i in names(at)) {
      attr(x[[i]], "labels") <- at[[i]][["labels"]]
      attr(x[[i]], "label") <- at[[i]][["label"]]
      attr(x[[i]], "na_values") <- at[[i]][["na_values"]]
      attr(x[[i]], "na.values") <- at[[i]][["na.values"]]
      attr(x[[i]], "na_range") <- at[[i]][["na_range"]]
      attr(x[[i]], "na.range") <- at[[i]][["na.range"]]
    }
  }

  x
}


#' @rdname add_rows
#' @export
merge_df <- function(..., id = NULL) {
  add_rows(..., id = id)
}
