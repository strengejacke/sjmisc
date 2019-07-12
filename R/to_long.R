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
#'          character vector with variable names per column group, or a numeric
#'          vector with column indices indicating those columns that should be
#'          gathered. See 'Examples'.
#' @param labels Character vector of same length as \code{values} with variable
#'          labels for the new variables created from gathered columns.
#'          See 'Examples' and 'Details'.
#' @param recode.key Logical, if \code{TRUE}, the values of the \code{key}
#'          column will be recoded to numeric values, in sequential ascending
#'          order.
#'
#' @seealso \code{\link{reshape_longer}}
#'
#' @details This function reshapes data from wide to long format, however,
#'   you can gather multiple column groups at once. Value and variable labels
#'   for non-gathered variables are preserved. Attributes from gathered variables,
#'   such as information about the variable labels, are lost during reshaping.
#'   Hence, the new created variables from gathered columns don't have any
#'   variable label attributes. In such cases, use \code{labels} argument to set
#'   back variable label attributes.
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
#' # gather multiple columns. both time and speed are gathered.
#' to_long(
#'   data = mydat,
#'   keys = "time",
#'   values = c("score", "speed"),
#'   c("score_t1", "score_t2", "score_t3"),
#'   c("speed_t1", "speed_t2", "speed_t3")
#' )
#'
#' # alternative syntax, using "reshape_longer()"
#' reshape_longer(
#'   mydat,
#'   columns = list(
#'     c("score_t1", "score_t2", "score_t3"),
#'     c("speed_t1", "speed_t2", "speed_t3")
#'   ),
#'   names.to = "time",
#'   values.to = c("score", "speed")
#' )
#'
#' # or ...
#' reshape_longer(
#'   mydat,
#'   list(3:5, 6:8),
#'   names.to = "time",
#'   values.to = c("score", "speed")
#' )
#'
#' # gather multiple columns, use numeric key-value
#' to_long(
#'   data = mydat,
#'   keys = "time",
#'   values = c("score", "speed"),
#'   c("score_t1", "score_t2", "score_t3"),
#'   c("speed_t1", "speed_t2", "speed_t3"),
#'   recode.key = TRUE
#' )
#'
#' # gather multiple columns by colum names and colum indices
#' to_long(
#'   data = mydat,
#'   keys = "time",
#'   values = c("score", "speed"),
#'   c("score_t1", "score_t2", "score_t3"),
#'   6:8,
#'   recode.key = TRUE
#' )
#'
#' # gather multiple columns, use separate key-columns
#' # for each value-vector
#' to_long(
#'   data = mydat,
#'   keys = c("time_score", "time_speed"),
#'   values = c("score", "speed"),
#'   c("score_t1", "score_t2", "score_t3"),
#'   c("speed_t1", "speed_t2", "speed_t3")
#' )
#'
#' # gather multiple columns, label columns
#' mydat <- to_long(
#'   data = mydat,
#'   keys = "time",
#'   values = c("score", "speed"),
#'   c("score_t1", "score_t2", "score_t3"),
#'   c("speed_t1", "speed_t2", "speed_t3"),
#'   labels = c("Test Score", "Time needed to finish")
#' )
#'
#' library(sjlabelled)
#' str(mydat$score)
#' get_label(mydat$speed)
#'
#' @export
to_long <- function(data, keys, values, ..., labels = NULL, recode.key = FALSE) {
  UseMethod("to_long")
}


#' @export
to_long.default <- function(data, keys, values, ..., labels = NULL, recode.key = FALSE) {
  to_long_helper(
    data = data,
    keys = keys,
    values = values,
    ...,
    labels = labels,
    recode.key = recode.key
  )
}


#' @export
to_long.mids <- function(data, keys, values, ..., labels = NULL, recode.key = FALSE) {
  ndf <- prepare_mids_recode(data)

  # select variable and compute rowsums. add this variable
  # to each imputed

  ndf$data <- purrr::map(
    ndf$data,
    function(.x) {
      dat <- to_long_helper(
        data = .x,
        keys = keys,
        values = values,
        ...,
        labels = labels,
        recode.key = recode.key
      )

      dat$.id <- 1:nrow(dat)
      dat
    }
  )

  final_mids_recode(ndf)
}


#' @importFrom dplyr bind_cols
#' @importFrom purrr map
#' @importFrom sjlabelled as_numeric set_label
to_long_helper <- function(data, keys, values, ..., labels, recode.key) {
  # get variable names for gather columns
  data_cols <- list(...)

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

  # check for numeric indices, and get column names then
  for (i in seq_len(length(data_cols))) {
    # check if all values are numeric
    if (all(is.numeric(data_cols[[i]]))) {
      # get column names instead
      data_cols[[i]] <- colnames(data)[data_cols[[i]]]
    }
  }

  # get all columns that should be gathered
  all_data_cols <- unlist(data_cols)


  # iterate each column group
  dummy <- purrr::map(seq_len(length(data_cols)), function(i) {
    # which of all column groups should be gathered in this step,
    # which not?
    remove_cols <- all_data_cols[!all_data_cols %in% data_cols[[i]]]

    # remove those columns that should not be gathered
    tmp <- data[, -match(remove_cols, colnames(data))]
    # gather data frame
    tmp <- suppressWarnings(.gather(tmp, keys[i], values[i], data_cols[[i]]))

    # need to recode key-value?
    if (recode.key)
      tmp[[keys[i]]] <- sort(sjlabelled::as_numeric(tmp[[keys[i]]], keep.labels = FALSE))

    # set variable label
    if (!is.null(labels))
      sjlabelled::set_label(tmp[[values[i]]]) <- labels[i]

    tmp
  })


  # we have at least one gathered data frame
  mydat <- dummy[[1]]

  # if we have multiple column groups to gather, go on here
  if (length(dummy) > 1) {
    # iterate remaining groups
    for (i in 2:length(dummy)) {
      # find gathered columns that do not already exist in our
      # output data frame
      .add_cols <- dummy[[i]][!colnames(dummy[[i]]) %in% colnames(mydat)]
      # and bind them to the output
      mydat <- dplyr::bind_cols(mydat, .add_cols)
    }
  }

  # return results
  mydat
}
