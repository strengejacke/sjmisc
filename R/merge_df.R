#' @title Merge labelled data frames
#' @name merge_df
#'
#' @description Merges (full join) two data frames and preserve value and variable labels.
#'
#' @param x1 First data frame to be merged.
#' @param x2 Second data frame to be merged.
#' @param id Optional name for ID column that will be created to indicate the
#'          source data frames for appended rows.
#'
#' @return A full joined data frame.
#'
#' @details This function merges two data frames, where equal named columns
#'            will be joined together. This function is a convenient wrapper for
#'            \code{merge(x1, x2, all = TRUE)}, however, unlike base
#'            \code{\link{merge}}, this function preserves value and
#'            variable labels. If matching columns have different value
#'            label attributes, attributes from first data frame will be
#'            used. For more details on the join operation, see
#'            'Details' in \code{\link{merge}} on \code{all}-argument.
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
#' @export
merge_df <- function(x1, x2, id = NULL) {
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
  x2_remain <- seq_len(ncol(x2))[-x2_match]
  # create dummy df for remaining data of x2
  tmp <- as.data.frame(matrix(nrow = nrow(x1), ncol = length(x2_remain)))
  colnames(tmp) <- colnames(x2)[x2_remain]
  # append rows of x2. now we have a data frame of same length as merged
  # x1 and x2
  tmp <- rbind(tmp, x2[, x2_remain])
  # final merge
  x_final <- cbind(x1_new, tmp)
  # create ID column?
  if (!is.null(id)) {
    # check whether column name already exists
    if (id %in% colnames(x_final)) {
      warning("Value of `id` already exists as column name. ID column was not created.", call. = F)
    } else {
      # create ID vector
      id_col <- c(rep(deparse(substitute(x1)), times = nrow(x1)),
                  rep(deparse(substitute(x2)), times = nrow(x2)))
      # bind id column
      x_final <- cbind(x_final, id_col)
      # name column
      colnames(x_final)[ncol(x_final)] <- id
    }
  }
  x_final
}