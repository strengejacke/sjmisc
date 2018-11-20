#' @title Split (categorical) vectors into dummy variables
#' @name to_dummy
#'
#' @description This function splits categorical or numeric vectors with
#'                more than two categories into 0/1-coded dummy variables.
#'
#' @param var.name Indicates how the new dummy variables are named. Use
#'          \code{"name"} to use the variable name or any other string that will
#'          be used as is. Only applies, if \code{x} is a vector. See 'Examples'.
#' @param suffix Indicates which suffix will be added to each dummy variable.
#'          Use \code{"numeric"} to number dummy variables, e.g. \emph{x_1},
#'          \emph{x_2}, \emph{x_3} etc. Use \code{"label"} to add value label,
#'          e.g. \emph{x_low}, \emph{x_mid}, \emph{x_high}. May be abbreviated.
#'
#' @inheritParams to_factor
#'
#' @return A data frame with dummy variables for each category of \code{x}.
#'         The dummy coded variables are of type \code{\link{atomic}}.
#'
#' @note \code{NA} values will be copied from \code{x}, so each dummy variable
#'         has the same amount of \code{NA}'s at the same position as \code{x}.
#'
#' @examples
#' data(efc)
#' head(to_dummy(efc$e42dep))
#'
#' # add value label as suffix to new variable name
#' head(to_dummy(efc$e42dep, suffix = "label"))
#'
#' # use "dummy" as new variable name
#' head(to_dummy(efc$e42dep, var.name = "dummy"))
#'
#' # create multiple dummies, append to data frame
#' to_dummy(efc, c172code, e42dep)
#'
#' # pipe-workflow
#' library(dplyr)
#' efc %>%
#'   select(e42dep, e16sex, c172code) %>%
#'   to_dummy()
#'
#' @export
to_dummy <- function(x, ..., var.name = "name", suffix = c("numeric", "label")) {
  # check for abbr
  suffix <- match.arg(suffix)
  # save variable name
  varname <- deparse(substitute(x))

  # evaluate arguments, generate data
  .dat <- get_dot_data(x, dplyr::quos(...))

  if (is.data.frame(x)) {
    # iterate variables of data frame
    x <- dplyr::bind_cols(
      purrr::map(colnames(.dat), ~ to_dummy_helper(
        x = .dat[[.x]], varname = .x, suffix = suffix
      ))
    )
  } else {
    # remove "data frame name"
    dollar_pos <- regexpr("$", varname, fixed = T)[1]
    if (dollar_pos != -1)
      varname <- substr(varname, start = dollar_pos + 1, stop = nchar(varname))

    # set default variable name
    if (var.name != "name") varname <- var.name

    # convert to dummy
    x <- to_dummy_helper(.dat, varname, suffix)
  }

  as.data.frame(x)
}


to_dummy_helper <- function(x, varname, suffix) {
  # make sure we have a factor, so order of values is correct
  if (is.character(x)) x <- to_factor(x)
  # check whether we have labels
  labels <- sjlabelled::get_labels(x, attr.only = F, values = "n", non.labelled = T)
  # get resp. set variable label for new dummy variables
  # get variable label
  label <- sjlabelled::get_label(x, def.value = varname)

  # get unique values
  values <- sort(unique(x))

  # find which labels / categories were
  # actually used
  if (is.null(names(labels))) {
    # find labels index numbers
    labels.nr <- seq_len(length(labels))[labels %in% values]
    # copy only used labels
    labels <- labels[labels %in% values]
  } else {
    # get label value labels
    label.names <- names(labels)
    # numeric?
    if (!anyNA(as.numeric(label.names)))
      label.names <- as.numeric(label.names)
    # find labels index numbers
    labels.nr <- seq_len(length(labels))[label.names %in% values]
    # copy only used labels
    labels <- labels[label.names %in% values]
  }

  # return value
  mydf <- data.frame()

  # create all dummy variables
  for (i in seq_len(length(values))) {
    # create dummy var
    dummy <- rep(0, length(x))
    # set NA
    dummy[is.na(x)] <- NA
    # copy dummy level
    dummy[which(x == values[i])] <- 1
    # set variable name
    sjlabelled::set_label(dummy) <- sprintf("%s: %s", label, labels[i])
    # bind to df
    if (nrow(mydf) == 0)
      mydf <- data.frame(dummy)
    else
      mydf <- cbind(mydf, dummy)
  }

  # prepare col.names
  col.nam <- rep(varname, ncol(mydf))

  if (suffix == "numeric")
    col.nam <- sprintf("%s_%i", col.nam, labels.nr)
  else
    col.nam <- sprintf("%s_%s", col.nam, labels)

  colnames(mydf) <- col.nam

  mydf
}
