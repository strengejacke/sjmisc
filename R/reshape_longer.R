#' @title Reshape data into long format
#' @name reshape_longer
#'
#' @description \code{reshape_longer()} reshapes one or more columns from
#'   wide into long format.
#'
#' @param x A data frame.
#' @param columns Names of variables (as character vector), or column index of
#'   variables, that should be reshaped. If multiple column groups should be
#'   reshaped, use a list of vectors (see 'Examples').
#' @param names.to Character vector with name(s) of key column(s) to create in output.
#'   Either one name per column group that should be gathered, or a single string.
#'   In the latter case, this name will be used as key column, and only one key
#'   column is created.
#' @param values.to Character vector with names of value columns (variable names)
#'   to create in output. Must be of same length as number of column
#'   groups that should be gathered. See 'Examples'.
#' @param numeric.timevar Logical, if \code{TRUE}, the values of the \code{names.to}
#'    column will be recoded to numeric values, in sequential ascending order.
#' @param id Name of ID-variable.
#' @param labels Character vector of same length as \code{values.to} with variable
#'   labels for the new variables created from gathered columns.
#'   See 'Examples'.
#'
#' @seealso \code{\link{to_long}}
#'
#' @return A reshaped data frame.
#'
#' @examples
#' # Reshape one column group into long format
#' mydat <- data.frame(
#'   age = c(20, 30, 40),
#'   sex = c("Female", "Male", "Male"),
#'   score_t1 = c(30, 35, 32),
#'   score_t2 = c(33, 34, 37),
#'   score_t3 = c(36, 35, 38)
#' )
#'
#' reshape_longer(
#'   mydat,
#'   columns = c("score_t1", "score_t2", "score_t3"),
#'   names.to = "time",
#'   values.to = "score"
#' )
#'
#'
#' # Reshape multiple column groups into long format
#' mydat <- data.frame(
#'   age = c(20, 30, 40),
#'   sex = c("Female", "Male", "Male"),
#'   score_t1 = c(30, 35, 32),
#'   score_t2 = c(33, 34, 37),
#'   score_t3 = c(36, 35, 38),
#'   speed_t1 = c(2, 3, 1),
#'   speed_t2 = c(3, 4, 5),
#'   speed_t3 = c(1, 8, 6)
#' )
#'
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
#' # gather multiple columns, label columns
#' x <- reshape_longer(
#'   mydat,
#'   list(3:5, 6:8),
#'   names.to = "time",
#'   values.to = c("score", "speed"),
#'   labels = c("Test Score", "Time needed to finish")
#' )
#'
#' library(sjlabelled)
#' str(x$score)
#' get_label(x$speed)
#' @export
reshape_longer <- function(x, columns = colnames(x), names.to = "key", values.to = "value", labels = NULL, numeric.timevar = FALSE, id = ".id") {

  variable_attr <- lapply(x, attributes)

  if (!is.list(columns)) columns <- list(columns)

  columns <- lapply(columns, function(.x) {
    if (is.numeric(.x)) .x <- colnames(x)[.x]
    .x
  })

  dat <- stats::reshape(
    x,
    idvar = id,
    times = columns[[1]],
    timevar = names.to,
    v.names = values.to,
    varying = columns,
    direction = "long"
  )

  if (numeric.timevar) {
    f <- as.factor(dat[[names.to]])
    levels(f) <- 1:nlevels(f)
    dat[[names.to]] <- as.numeric(as.character(f))
  }

  for (i in colnames(dat)) {
    attributes(dat[[i]]) <- variable_attr[[i]]
  }

  if (!is.null(labels)) {
    if (length(labels) != length(values.to)) {
      insight::print_color("Could not set variable labels. 'labels' have different length than number of reshaped columns ('values.to').\n", "red")
    } else {
      for (i in 1:length(labels)) sjlabelled::set_label(dat[[values.to[i]]]) <- labels[i]
    }
  }

  row.names(dat) <- NULL
  dat
}
