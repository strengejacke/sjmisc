#' @title Convert wide data to long format
#' @name to_long
#' @description This function converts wide data into long format. It allows
#'                to transform multiple key-value pairs to be transformed
#'                from wide to long format in one single step.
#'
#' @param data A \code{data.frame} that should be tansformed from wide to
#'          long format.
#' @param keys Character vector with name(s) of key column(s) to create in output.
#'          Either one key value per column group that should be gathered, or
#'          a single string. In the latter case, this name will be used as
#'          key column, and only one key column is created. See 'Examples'.
#' @param values Character vector with names of value columns (variable names)
#'          to create in output. Must be of same length as number of column
#'          groups that should be gathered. See 'Examples'.
#' @param ... Specification of columns that should be gathered. Must be one
#'          character vector with variable names per column group.
#' @param labels Character vector of same length as \code{values} with variable
#'          labels for the new variables created from gathered columns.
#'          See 'Examples' and 'Details'.
#' @param recode.key Logical, if \code{TRUE}, the values of the \code{key}
#'          column will be recoded to numeric values, in sequential ascending
#'          order.
#'
#' @details This function enhances \pkg{tidyr}'s \code{\link[tidyr]{gather}}
#'            function that you can gather multiple column groups at once.
#'            Value and variable labels for non-gathered variables are preserved.
#'            However, gathered variables may have different variable label
#'            attributes. In this case, \code{\link[tidyr]{gather}} will drop
#'            these attributes. Hence, the new created variables from gathered
#'            columns don't have any variable label attributes. In such cases,
#'            use \code{labels} argument to set variable label attributes.
#'
#' @examples
#' # create sample
#' mydat <- data.frame(age = c(20, 30, 40),
#'                     sex = c("Female", "Male", "Male"),
#'                     score_t1 = c(30, 35, 32),
#'                     score_t2 = c(33, 34, 37),
#'                     score_t3 = c(36, 35, 38),
#'                     speed_t1 = c(2, 3, 1),
#'                     speed_t2 = c(3, 4, 5),
#'                     speed_t3 = c(1, 8, 6))
#'
#' # check tidyr. score is gathered, however, speed is not
#' tidyr::gather(mydat, "time", "score", score_t1, score_t2, score_t3)
#'
#' # gather multiple columns. both time and speed are gathered.
#' to_long(mydat, "time", c("score", "speed"),
#'         c("score_t1", "score_t2", "score_t3"),
#'         c("speed_t1", "speed_t2", "speed_t3"))
#'
#' # gather multiple columns, use numeric key-value
#' to_long(mydat, "time", c("score", "speed"),
#'         c("score_t1", "score_t2", "score_t3"),
#'         c("speed_t1", "speed_t2", "speed_t3"),
#'         recode.key = TRUE)
#'
#' # gather multiple columns, use separate key-column for each value-vector
#' to_long(mydat, c("time_score", "time_speed"), c("score", "speed"),
#'         c("score_t1", "score_t2", "score_t3"),
#'         c("speed_t1", "speed_t2", "speed_t3"))
#'
#' # gather multiple columns, label columns
#' mydat <- to_long(mydat, "time", c("score", "speed"),
#'                  c("score_t1", "score_t2", "score_t3"),
#'                  c("speed_t1", "speed_t2", "speed_t3"),
#'                  labels = c("Test Score", "Time needed to finish"))
#'
#' str(mydat$score)
#' get_label(mydat$speed)
#' lbl_df(mydat)
#'
#' @importFrom tidyr gather_
#' @importFrom dplyr bind_cols
#' @export
to_long <- function(data, keys, values, ..., labels = NULL, recode.key = FALSE) {
  # get variable names for gather columns
  data_cols <- eval(substitute(list(...)))
  # init output
  dummy <- list()
  # if we have just one key value, repeat it to required length
  if (length(keys) < length(data_cols))
    keys <- rep(keys, times = length(data_cols))
  # check for correct length
  if (length(values) < length(data_cols)) {
    stop("`values` must be of same length as column groups to gather.", call. = F)
  }
  # check for correct length
  if (!is.null(labels) && length(labels) < length(data_cols)) {
    warning("`labels` must be of same length as `values`. Dropping variable labels for gathered columns.")
    labels <- NULL
  }
  # get all columns that should be gathered
  all_data_cols <- unlist(data_cols)
  # iterate each column group
  for (i in 1:length(data_cols)) {
    # which of all column groups should be gathered in this step,
    # which not?
    remove_cols <- all_data_cols[!all_data_cols %in% data_cols[[i]]]
    # remove those columns that should not be gathered
    tmp <- data[, -match(remove_cols, colnames(data))]
    # gather data frame
     tmp <- suppressWarnings(
       tidyr::gather_(tmp,
                      keys[i],
                      values[i],
                      data_cols[[i]])
       )
     # need to recode key-value?
     if (recode.key)
       tmp[[keys[i]]] <- to_value(tmp[[keys[i]]], keep.labels = FALSE)
     # set variable label
     if (!is.null(labels))
       set_label(tmp[[values[i]]]) <- labels[i]
     # add output to list
     dummy[[length(dummy) + 1]] <- tmp
  }
  # we have at least one gathered data frame
  mydat <- dummy[[1]]
  # if we have multiple column groups to gather, go on here
  if (length(dummy) > 1) {
    # iterate remaining groups
    for (i in 2:length(dummy)) {
      # find gathered columns that do not already exist in our
      # output data frame
      add_cols <- dummy[[i]][!colnames(dummy[[i]]) %in% colnames(mydat)]
      # and bind them to the output
      mydat <- dplyr::bind_cols(mydat, add_cols)
    }
  }
  # return results
  return(mydat)
}