#' @title Basic descriptive statistics
#' @name descr
#'
#' @description This function prints a basic descriptive statistic, including
#'   variable labels.
#'
#' @param x A vector or a data frame. May also be a grouped data frame
#'    (see 'Note' and 'Examples').
#' @param max.length Numeric, indicating the maximum length of variable labels
#'    in the output. If variable names are longer than \code{max.length},
#'    they will be shortened to the last whole word within the first
#'    \code{max.length} chars.
#' @param out Character vector, indicating whether the results should be printed
#'    to console (\code{out = "txt"}) or as HTML-table in the viewer-pane
#'    (\code{out = "viewer"}) or browser (\code{out = "browser"}).
#'
#' @inheritParams to_factor
#' @inheritParams frq
#'
#' @return A data frame with basic descriptive statistics.
#'
#' @note \code{data} may also be a grouped data frame (see \code{\link[dplyr]{group_by}})
#'    with up to two grouping variables. Descriptive tables are created for each
#'    subgroup then.
#'
#' @examples
#' data(efc)
#' descr(efc, e17age, c160age)
#'
#' efc$weights <- abs(rnorm(nrow(efc), 1, .3))
#' descr(efc, c12hour, barthtot, weights = weights)
#'
#' library(dplyr)
#' efc %>% select(e42dep, e15relat, c172code) %>% descr()
#'
#' # with grouped data frames
#' efc %>%
#'   group_by(e16sex) %>%
#'   select(e16sex, e42dep, e15relat, c172code) %>%
#'   descr()
#'
#' # you can select variables also inside 'descr()'
#' efc %>%
#'   group_by(e16sex, c172code) %>%
#'   descr(e16sex, c172code, e17age, c160age)
#'
#' # or even use select-helpers
#' descr(efc, contains("cop"), max.length = 20)
#'
#' @importFrom dplyr select mutate
#' @importFrom sjlabelled copy_labels
#' @export
descr <- function(x, ..., max.length = NULL, weights = NULL, out = c("txt", "viewer", "browser")) {

  out <- match.arg(out)

  if (out != "txt" && !requireNamespace("sjPlot", quietly = TRUE)) {
    message("Package `sjPlot` needs to be loaded to print HTML tables.")
    out <- "txt"
  }


  # get dot data
  dd <- get_dot_data(x, dplyr::quos(...))

  w.name <- deparse(substitute(weights))

  if (w.name == "NULL") {
    w.name <- NULL
  } else {
    dd$.weights <- x[[w.name]]
  }


  # return values
  dataframes <- list()

  # do we have a grouped data frame?
  if (inherits(dd, "grouped_df")) {

    # get grouped data
    grps <- get_grouped_data(dd)

    # now plot everything
    for (i in seq_len(nrow(grps))) {

      # copy back labels to grouped data frame
      tmp <- sjlabelled::copy_labels(grps$data[[i]], dd)

      dummy <- descr_helper(tmp, max.length)
      attr(dummy, "group") <- get_grouped_title(x, grps, i, sep = "\n")

      # save data frame for return value
      dataframes[[length(dataframes) + 1]] <- dummy
    }

    # add class-attr for print-method()
    if (out == "txt")
      class(dataframes) <- c("sjmisc_grpdescr", "list")
    else
      class(dataframes) <- c("sjt_grpdescr", "list")

  } else {
    dataframes <- descr_helper(dd, max.length)

    # add class-attr for print-method()
    if (out == "txt")
      class(dataframes) <- c("sjmisc_descr", class(dataframes))
    else
      class(dataframes) <- c("sjt_descr", class(dataframes))
  }

  # save how to print output
  attr(dataframes, "print") <- out

  dataframes
}


#' @importFrom dplyr select_if group_by summarise_all funs summarise
#' @importFrom tidyr gather
#' @importFrom sjlabelled get_label
#' @importFrom stats var na.omit sd median weighted.mean
#' @importFrom rlang .data
descr_helper <- function(dd, max.length) {

  # check if we have a single vector, because purrr would return
  # a result for each *value*, instead one result for the complete vector
  if (!is.data.frame(dd))
    dd <- as.data.frame(dd)

  if (obj_has_name(dd, ".weights")) {
    weights <- dd$.weights
    dd <- dplyr::select(dd, -.data$.weights)
  } else {
    weights <- NULL
  }


  ff <- function(x) is.numeric(x) | is.factor(x)
  dd <- dplyr::select_if(dd, ff)

  # get default variable name
  var.name <- colnames(dd)
  if (is.null(var.name)) var.name <- NA

  type <- var_type(dd)
  label <- unname(sjlabelled::get_label(dd, def.value = var.name))

  dd <- to_value(dd, keep.labels = FALSE)

  if (is.null(weights)) {
    x <- suppressWarnings(
      dd %>%
        dplyr::select_if(is.numeric) %>%
        tidyr::gather(key = "var", value = "val") %>%
        dplyr::group_by(.data$var) %>%
        dplyr::summarise_all(
          dplyr::funs(
            n = length(stats::na.omit(.data$val)),
            NA.prc = 100 * sum(is.na(.data$val)) / length(.data$val),
            mean = mean(.data$val, na.rm = TRUE),
            sd = stats::sd(.data$val, na.rm = TRUE),
            se = sqrt(stats::var(.data$val, na.rm = TRUE) / length(stats::na.omit(.data$val))),
            md = stats::median(.data$val, na.rm = TRUE),
            trimmed = mean(.data$val, na.rm = TRUE, trim = .1),
            range = sprintf(
              "%s (%s-%s)",
              as.character(round(diff(range(.data$val, na.rm = TRUE)), 2)),
              as.character(round(min(.data$val, na.rm = TRUE), 2)),
              as.character(round(max(.data$val, na.rm = TRUE), 2))
            ),
            skew = sjmisc.skew(.data$val)
          ))
    ) %>%
      as.data.frame()
  } else {
    dd$.weights <- weights

    x <- suppressWarnings(
      dd %>%
        dplyr::select_if(is.numeric) %>%
        tidyr::gather(key = "var", value = "val", -.data$.weights) %>%
        dplyr::group_by(.data$var) %>%
        dplyr::summarise(
          n = round(sum(.data$.weights[!is.na(.data$val)], na.rm = TRUE)),
          NA.prc = 100 * sum(is.na(.data$val)) / sum(.data$.weights[!is.na(.data$val)], na.rm = TRUE),
          mean = stats::weighted.mean(.data$val, w = .data$.weights, na.rm = TRUE),
          sd = wtd_sd_helper(.data$val, weights = .data$.weights),
          se = wtd_se_helper(.data$val, weights = .data$.weights),
          range = sprintf(
            "%s (%s-%s)",
            as.character(round(diff(range(.data$val, na.rm = TRUE)), 2)),
            as.character(round(min(.data$val, na.rm = TRUE), 2)),
            as.character(round(max(.data$val, na.rm = TRUE), 2))
          )
        )
    ) %>%
      as.data.frame()
  }


  # summarise_all() sorts variables, so restore order
  x <- x[match(var.name, x$var), ] %>% add_variables(type, label, .after = 1)

  # check if labels should be truncated
  x$label <- shorten_string(x$label, max.length)

  if (!is.null(weights)) attr(x, "weights") <- "TRUE"

  x
}


sjmisc.skew <- function(x) {
  if (any(ina <- is.na(x)))
    x <- x[!ina]

  n <- length(x)
  x <- x - mean(x)

  if (n < 3)
    return(NA)

  sqrt(n) * sum(x ^ 3) / (sum(x ^ 2) ^ (3 / 2)) * sqrt(n * (n - 1)) / (n - 2)
}


wtd_se_helper <- function(x, weights) {
  sqrt(wtd_var(x, weights) / length(stats::na.omit(x)))
}

wtd_sd_helper <- function(x, weights = NULL) {
  sqrt(wtd_var(x, weights))
}

wtd_var <- function(x, w) {
  if (is.null(w)) w <- rep(1, length(x))

  x[is.na(w)] <- NA
  w[is.na(x)] <- NA

  w <- na.omit(w)
  x <- na.omit(x)

  xbar <- sum(w * x) / sum(w)
  sum(w * ((x - xbar)^2)) / (sum(w) - 1)
}
