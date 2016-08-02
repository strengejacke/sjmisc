#' @title Recode variable categories into new values
#' @name recode_to
#'
#' @description Recodes (or "renumbers") the categories of \code{var} into new category values, beginning
#'                with the lowest value specified by \code{lowest}. Useful if you want
#'                to recode dummy variables with 1/2 coding to 0/1 coding, or recoding scales from
#'                1-4 to 0-3 etc.
#'
#' @seealso \code{\link{rec}} for general recoding of variables and \code{\link{set_na}}
#'            for setting \code{\link{NA}} values.
#'
#' @param x Variable (vector), \code{data.frame} or \code{list} of variables that should be recoded.
#' @param lowest Indicating the lowest category value for recoding. Default is 0, so the new
#'          variable starts with value 0.
#' @param highest If specified and larger than \code{lowest}, all category values larger than
#'          \code{highest} will be set to \code{NA}. Default is \code{-1}, i.e. this argument is ignored
#'          and no NA's will be produced.
#' @return A new variable with recoded category values, where \code{lowest} indicates the lowest
#'           value; or a data frame or list of variables where variables have
#'           been recoded as described.
#'
#' @note Value and variable label attributes (see, for instance, \code{\link{get_labels}}
#'         or \code{\link{set_labels}}) are preserved.
#'
#' @examples
#' # recode 1-4 to 0-3
#' dummy <- sample(1:4, 10, replace = TRUE)
#' recode_to(dummy)
#'
#' # recode 3-6 to 0-3
#' # note that numeric type is returned
#' dummy <- as.factor(3:6)
#' recode_to(dummy)
#'
#' # lowest value starting with 1
#' dummy <- sample(11:15, 10, replace = TRUE)
#' recode_to(dummy, 1)
#'
#' # lowest value starting with 1, highest with 3
#' # all others set to NA
#' dummy <- sample(11:15, 10, replace = TRUE)
#' recode_to(dummy, 1, 3)
#'
#' # create list of variables
#' data(efc)
#' dummy <- list(efc$c82cop1, efc$c83cop2, efc$c84cop3)
#' # check original distribution of categories
#' lapply(dummy, table)
#' # renumber from 1 to 0
#' lapply(recode_to(dummy), table)
#'
#' @export
recode_to <- function(x, lowest = 0, highest = -1) {
  UseMethod("recode_to")
}

#' @export
recode_to.data.frame <- function(x, lowest = 0, highest = -1) {
  tibble::as_tibble(lapply(x, FUN = rec_to_helper, lowest, highest))
}

#' @export
recode_to.list <- function(x, lowest = 0, highest = -1) {
  lapply(x, FUN = rec_to_helper, lowest, highest)
}

#' @export
recode_to.default <- function(x, lowest = 0, highest = -1) {
  rec_to_helper(x, lowest, highest)
}


rec_to_helper <- function(x, lowest, highest) {
  # retrieve value labels
  val_lab <- get_labels(x,
                        attr.only = TRUE,
                        include.values = NULL,
                        include.non.labelled = TRUE)
  # retrieve variable label
  var_lab <- get_label(x)
  # check if factor
  if (is.factor(x)) {
    # try to convert to numeric
    x <- as.numeric(as.character(x))
  }
  # retrieve lowest category
  minval <- min(x, na.rm = TRUE)
  # check substraction difference between current lowest value
  # and requested lowest value
  downsize <- minval - lowest
  x <- sapply(x, function(y) y - downsize)
  # check for highest range
  # set NA to all values out of range
  if (highest > lowest) x[x > highest] <- NA
  # set back labels, if we have any
  if (!is.null(val_lab)) x <- suppressWarnings(set_labels(x, val_lab))
  if (!is.null(var_lab)) x <- suppressWarnings(set_label(x, var_lab))
  # return recoded x
  return(x)
}

