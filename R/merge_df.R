#' @title Merge labelled data frames
#' @name merge_df
#'
#' @description Merges (full join) two (or more) data frames and preserve value and variable labels.
#'
#' @param x1 First data frame to be merged.
#' @param x2 Second data frame to be merged.
#' @param ... More data frames to be merged.
#' @param id Optional name for ID column that will be created to indicate the
#'          source data frames for appended rows.
#'
#' @return A full joined data frame.
#'
#' @details This function merges two data frames, where equal named columns
#'            will be joined together. Value and variable labels are
#'            preserved. If matching columns have different value
#'            label attributes, attributes from first data frame will be
#'            used.
#'
#' @examples
#' library(dplyr)
#' data(efc)
#' x1 <- efc %>% select(1:5) %>% slice(1:10)
#' x2 <- efc %>% select(3:7) %>% slice(1:10)
#'
#' mydf <- merge_df(x1, x2)
#' mydf
#' str(mydf)
#'
#' \dontrun{
#' library(sjPlot)
#' view_df(mydf)}
#'
#' x3 <- efc %>% select(5:9) %>% slice(1:10)
#' x4 <- efc %>% select(11:14) %>% slice(1:10)
#'
#' mydf <- merge_df(x1, x2, x3, x4, id = "subsets")
#' mydf
#' str(mydf)
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows
#' @export
merge_df <- function(x1, x2, ..., id = NULL) {
  # retrieve list of parameters
  more_dfs <- list(...)

  # first step, initial merge
  x_final <- merge_df_helper(x1, x2)

  # merge remaining df's if we have more data frames
  if (!is.null(more_dfs) && length(more_dfs) > 0) {
    # iterate all remaining data frames
    for (i in seq_len(length(more_dfs))) {
      # create ID vector
      x_final <- merge_df_helper(x_final, more_dfs[[i]])
    }
  }

  # create ID column?
  if (!is.null(id)) {
    # check whether column name already exists
    if (id %in% colnames(x_final)) {
      warning("Value of `id` already exists as column name. ID column was not created.", call. = F)
    } else {
      # create ID vector
      id_col <- c(rep(deparse(substitute(x1)), times = nrow(x1)),
                  rep(deparse(substitute(x2)), times = nrow(x2)))
      # do we have more data frames?
      if (!is.null(more_dfs) && length(more_dfs) > 0) {
        # get names of data frames for ID column
        more_df_names <- match.call(expand.dots = FALSE)$`...`

        # iterate all remaining data frames
        for (i in seq_len(length(more_dfs))) {
          # create ID vector
          id_col <- c(id_col, rep(as.character(more_df_names[[i]]), times = nrow(more_dfs[[i]])))
        }
      }
      # bind id column
      x_final <- cbind(x_final, id_col)
      # name column
      colnames(x_final)[ncol(x_final)] <- id
    }
  }
  # return merged df
  tibble::as_tibble(x_final)
}


merge_df_helper <- function(x1, x2) {
  # check if both data frames have same column names
  # in case, someone forgets that rbind exists...
  if (isTRUE(all.equal(sort(colnames(x1)), sort(colnames(x2))))) {
    # bind rows
    tmp <- dplyr::bind_rows(x1, x2)
    # copy attributes
    at <- lapply(x1, attributes)
    # set back attributes
    for (i in seq_len(length(at))) attributes(tmp[[i]]) <- at[[i]]
    # return data
    return(tmp)
  }

  # find matching columns in both data frames
  x2_match <- match(colnames(x1), colnames(x2))
  x1_match <- which(!is.na(x2_match))
  # clean up NA
  x2_match <- x2_match[!is.na(x2_match)]

  # now we have the matching columns of x1 in x1_match
  # and of x2 in x2_match. Next, create empty data frame with
  # correct dimension to append rows of matching columns from x2
  # to x1
  tmp <- as.data.frame(matrix(nrow = nrow(x2), ncol = ncol(x1)))
  colnames(tmp) <- colnames(x1)
  tmp[, x1_match] <- x2[, x2_match]

  # x1_new has now all variables from x1, plus all variables
  # of x2 that also appear in x1
  x1_new <- rbind(x1, tmp)

  # which columns are still in x2 and have not been merged yet?
  # in certain cases, e.g. when we have no matching columns at all,
  # x2_match is of length 0. in this case, all columns are still remaining,
  # so we need to check this here
  if (sjmisc::is_empty(x2_match))
    x2_remain <- seq_len(ncol(x2))
  else
    x2_remain <- seq_len(ncol(x2))[-x2_match]

  # create dummy df for remaining data of x2
  tmp <- as.data.frame(matrix(nrow = nrow(x1), ncol = length(x2_remain)))
  colnames(tmp) <- colnames(x2)[x2_remain]

  # append rows of x2. now we have a data frame of same length as merged
  # x1 and x2
  tmp <- rbind(tmp, x2[, x2_remain])
  # copy attributes
  for (i in seq_len(length(x2_remain))) {
    attributes(tmp[[i]]) <- attributes(x2[[x2_remain[i]]])
  }

  # final merge
  x_final <- cbind(x1_new, tmp)
  # return merged df
  x_final
}