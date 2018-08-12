#' @title Recode variable categories into new values
#' @name recode_to
#'
#' @description Recodes (or "renumbers") the categories of variables into new
#'   category values, beginning with the lowest value specified by \code{lowest}.
#'   Useful when recoding dummy variables with 1/2 values to 0/1 values,  or
#'   recoding scales from 1-4 to 0-3 etc.
#'   \code{recode_to_if()} is a scoped variant of \code{recode_to()}, where
#'   recoding will be applied only to those variables that match the
#'   logical condition of \code{predicate}.
#'
#' @seealso \code{\link{rec}} for general recoding of variables and \code{\link{set_na}}
#'            for setting \code{\link{NA}} values.
#'
#' @param lowest Indicating the lowest category value for recoding. Default is 0, so the new
#'          variable starts with value 0.
#' @param highest If specified and greater than \code{lowest}, all category values larger than
#'          \code{highest} will be set to \code{NA}. Default is \code{-1}, i.e. this argument is ignored
#'          and no NA's will be produced.
#'
#' @inheritParams to_factor
#' @inheritParams rec
#'
#' @return \code{x} with recoded category values, where \code{lowest} indicates
#'   the lowest value;  If \code{x} is a data frame, for \code{append = TRUE},
#'   \code{x} including the recoded variables as new columns is returned; if
#'   \code{append = FALSE}, only the recoded variables will be returned.
#'
#' @note Value and variable label attributes are preserved.
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
#' recode_to(dummy, lowest = 1)
#'
#' # lowest value starting with 1, highest with 3
#' # all others set to NA
#' dummy <- sample(11:15, 10, replace = TRUE)
#' recode_to(dummy, lowest = 1, highest = 3)
#'
#' # recode multiple variables at once
#' data(efc)
#' recode_to(efc, c82cop1, c83cop2, c84cop3, append = FALSE)
#'
#' library(dplyr)
#' efc %>%
#'   select(c82cop1, c83cop2, c84cop3) %>%
#'   mutate(
#'     c82new = recode_to(c83cop2, lowest = 5),
#'     c83new = recode_to(c84cop3, lowest = 3)
#'   ) %>%
#'   head()
#'
#'
#' @export
recode_to <- function(x, ..., lowest = 0, highest = -1, append = TRUE, suffix = "_r0") {
  UseMethod("recode_to")
}


#' @importFrom dplyr quos
#' @export
recode_to.default <- function(x, ..., lowest = 0, highest = -1, append = TRUE, suffix = "_r0") {
  # evaluate arguments, generate data
  .dat <- get_dot_data(x, dplyr::quos(...))

  rec_to_fun(
    x = x,
    .dat = .dat,
    lowest = lowest,
    highest = highest,
    append = append,
    suffix = suffix
  )
}


#' @importFrom dplyr bind_cols select quos
#' @importFrom purrr map
#' @export
recode_to.mids <- function(x, ..., lowest = 0, highest = -1, append = TRUE, suffix = "_r0") {
  vars <- dplyr::quos(...)
  ndf <- prepare_mids_recode(x)

  # select variable and compute rowsums. add this variable
  # to each imputed

  ndf$data <- purrr::map(
    ndf$data,
    function(.x) {
      dat <- dplyr::select(.x, !!! vars)
      dplyr::bind_cols(
        .x,
        rec_to_fun(
          x = dat,
          .dat = dat,
          lowest = lowest,
          highest = highest,
          append = FALSE,
          suffix = suffix
        ))
    }
  )

  final_mids_recode(ndf)
}


#' @rdname recode_to
#' @importFrom dplyr select_if
#' @export
recode_to_if <- function(x, predicate, lowest = 0, highest = -1, append = TRUE, suffix = "_r0") {

  # select variables that match logical conditions
  .dat <- dplyr::select_if(x, .predicate = predicate)

  # if no variable matches the condition specified
  # in predicate, return original data

  if (sjmisc::is_empty(.dat)) {
    if (append)
      return(x)
    else
      return(.dat)
  }

  rec_to_fun(
    x = x,
    .dat = .dat,
    lowest = lowest,
    highest = highest,
    append = append,
    suffix = suffix
  )
}


#' @importFrom dplyr bind_cols
#' @importFrom tibble as_tibble
rec_to_fun <- function(x, .dat, lowest, highest, append, suffix) {
  if (is.data.frame(x)) {

    # remember original data, if user wants to bind columns
    orix <- tibble::as_tibble(x)

    # iterate variables of data frame
    for (i in colnames(.dat)) {
      x[[i]] <- rec_to_helper(
        x = .dat[[i]],
        lowest = lowest,
        highest = highest
      )
    }

    # coerce to tibble and select only recoded variables
    x <- tibble::as_tibble(x[colnames(.dat)])

    # add suffix to recoded variables?
    if (!is.null(suffix) && !sjmisc::is_empty(suffix)) {
      colnames(x) <- sprintf("%s%s", colnames(x), suffix)
    }

    # combine data
    if (append) x <- dplyr::bind_cols(orix, x)
  } else {
    x <- rec_to_helper(
      x = .dat,
      lowest = lowest,
      highest = highest
    )
  }

  x
}


#' @importFrom sjlabelled set_label set_labels
rec_to_helper <- function(x, lowest, highest) {
  # retrieve value labels
  val_lab <- sjlabelled::get_labels(
    x,
    attr.only = TRUE,
    values = NULL,
    non.labelled = TRUE
  )

  # retrieve variable label
  var_lab <- sjlabelled::get_label(x)

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
  if (!is.null(val_lab)) x <- suppressWarnings(sjlabelled::set_labels(x, labels = val_lab))
  if (!is.null(var_lab)) x <- suppressWarnings(sjlabelled::set_label(x, label = var_lab))

  # return recoded x
  x
}

