#' @title Frequencies of labelled variables
#' @name frq
#'
#' @description This function returns a frequency table of labelled vectors, as data frame.
#'
#' @param sort.frq Determines whether categories should be sorted
#'   according to their frequencies or not. Default is \code{"none"}, so
#'   categories are not sorted by frequency. Use \code{"asc"} or
#'   \code{"desc"} for sorting categories ascending or descending order.
#' @param weight.by Vector of weights that will be applied to weight all observations.
#'   Must be a vector of same length as the input vector. Default is
#'   \code{NULL}, so no weights are used.
#' @param auto.grp Numeric value, indicating the minimum amount of unique
#'   values in a variable, at which automatic grouping into smaller  units
#'   is done (see \code{\link{group_var}}). Default value for \code{auto.group}
#'   is \code{NULL}, i.e. auto-grouping is off.
#'
#' @inheritParams descr
#'
#' @return A list of data frames with values, value labels, frequencies, raw, valid and
#'           cumulative percentages of \code{x}.
#'
#' @note \code{x} may also be a grouped data frame (see \code{\link[dplyr]{group_by}})
#'       with up to two grouping variables. Frequency tables are created for each
#'       subgroup then.
#'
#' @seealso \code{\link{flat_table}} for labelled (proportional) tables.
#'
#' @examples
#' library(haven)
#' # create labelled integer
#' x <- labelled(
#'   c(1, 2, 1, 3, 4, 1),
#'   c(Male = 1, Female = 2, Refused = 3, "N/A" = 4)
#' )
#' frq(x)
#'
#' x <- labelled(
#'   c(1:3, tagged_na("a", "c", "z"), 4:1, 2:3),
#'   c("Agreement" = 1, "Disagreement" = 4, "First" = tagged_na("c"),
#'     "Refused" = tagged_na("a"), "Not home" = tagged_na("z"))
#' )
#' frq(x)
#'
#' # in a pipe
#' data(efc)
#' library(dplyr)
#' efc %>%
#'   select(e42dep, e15relat, c172code) %>%
#'   frq()
#'
#' # or:
#' # frq(efc, e42dep, e15relat, c172code)
#'
#' # with grouped data frames, in a pipe
#' efc %>%
#'   group_by(e16sex, c172code) %>%
#'   frq(e16sex, c172code, e42dep)
#'
#' # with select-helpers: all variables from the COPE-Index
#' # (which all have a "cop" in their name)
#' frq(efc, contains("cop"))
#'
#' # all variables from column "c161sex" to column "c175empl"
#' frq(efc, c161sex:c175empl)
#'
#' # for non-labelled data, variable name is printed,
#' # and "label" column is removed from output
#' data(iris)
#' frq(iris, Species)
#'
#' @importFrom stats na.omit
#' @importFrom dplyr full_join
#' @importFrom tibble add_row
#' @importFrom sjlabelled get_label get_labels get_values copy_labels
#' @export
frq <- function(x, ..., sort.frq = c("none", "asc", "desc"), weight.by = NULL, auto.grp = NULL) {

  # get dot data
  x <- get_dot_data(x, dplyr::quos(...))

  # match args
  sort.frq <- match.arg(sort.frq)

  # return values
  dataframes <- list()

  # do we have a grouped data frame?
  if (inherits(x, "grouped_df")) {

    # get grouped data
    grps <- get_grouped_data(x)

    # we may have more than two variables...
    for (j in seq_len(ncol(grps$data[[1]]))) {

      # now plot everything
      for (i in seq_len(nrow(grps))) {
        # copy back labels to grouped data frame
        tmp <- sjlabelled::copy_labels(grps$data[[i]][j], x)

        # print frequencies
        dummy <-
          frq_helper(
            x = tmp[[1]],
            sort.frq = sort.frq,
            weight.by = weight.by,
            cn = colnames(tmp)[1],
            auto.grp = auto.grp
          )

        attr(dummy, "group") <- get_grouped_title(x, grps, i, sep = "\n")

        # save data frame for return value
        dataframes[[length(dataframes) + 1]] <- dummy
      }

    }

  } else {
    # if we don't have data frame, coerce
    if (!is.data.frame(x)) x <- tibble::tibble(x)

    for (i in seq_len(ncol(x))) {
      # print frequencies
      dummy <-
        frq_helper(
          x = x[[i]],
          sort.frq = sort.frq,
          weight.by = weight.by,
          cn = colnames(x)[i],
          auto.grp = auto.grp
        )

      # save data frame for return value
      dataframes[[length(dataframes) + 1]] <- dummy
    }
  }

  # add class-attr for print-method()
  class(dataframes) <- c("sjmisc_frq", "list")

  dataframes
}


#' @importFrom dplyr n_distinct full_join
#' @importFrom stats na.omit xtabs na.pass sd weighted.mean
#' @importFrom sjlabelled get_labels get_label as_numeric
#' @importFrom tibble add_row
frq_helper <- function(x, sort.frq, weight.by, cn, auto.grp) {
  # remember type
  vartype <- var_type(x)

  # convert NaN and Inf to missing
  x <- zap_inf(x)

  # variable with only mising?
  if (length(stats::na.omit(x)) == 0) {
    mydat <- data.frame(
      val = NA,
      label = NA,
      frq = NA,
      raw.prc = NA,
      valid.prc = NA,
      cum.perc = NA
    )
    return(structure(class = "sjmisc_frq", list(mydat = mydat)))
  }


  # save descriptive statistics

  xnum <- sjlabelled::as_numeric(x, keep.labels = FALSE)
  if (!is.null(weight.by)) {
    mean.value <- stats::weighted.mean(stats::na.omit(xnum), w = weight.by)
    if (requireNamespace("sjstats", quietly = TRUE))
      sd.value <- sjstats::wtd_sd(stats::na.omit(xnum), weights = weight.by)
    else
      sd.value <- NA
  } else {
    mean.value <- mean(xnum, na.rm = TRUE)
    sd.value <- stats::sd(xnum, na.rm = TRUE)
  }


  # get variable label (if any)
  varlab <- sjlabelled::get_label(x)


  # numeric variables with many distinct values may
  # be grouped for better overview

  if (!is.null(auto.grp) && dplyr::n_distinct(x, na.rm = TRUE) >= auto.grp) {
    gl <- group_labels(x, size = "auto", n = auto.grp)
    x <- group_var(x, size = "auto", n = auto.grp)
    gv <- sort(stats::na.omit(unique(x)))
    names(gv) <- gl
    attr(x, "labels") <- gv
  }


  # get value labels (if any)
  labels <- sjlabelled::get_labels(x, attr.only = T, include.values = "n", include.non.labelled = T)


  # if we don't have variable label, use column name
  if (sjmisc::is_empty(varlab) && !sjmisc::is_empty(cn))
    varlab <- cn
  else if (!sjmisc::is_empty(varlab) && !sjmisc::is_empty(cn))
    varlab <- sprintf("%s (%s)", varlab, cn)


  # do we have a labelled vector?
  if (!is.null(labels)) {
    # add rownames and values as columns
    dat <-
      data.frame(
        n = names(labels),
        v = as.character(labels),
        stringsAsFactors = FALSE
      )

    colnames(dat) <- c("val", "label")

    # character vectors need to be converted with to_value
    # to avoid NAs, but only if character is non-numeric
    if (is.character(dat$val) && anyNA(suppressWarnings(as.numeric(dat$val))))
      dat$val <- to_value(dat$val, keep.labels = F)
    else
      dat$val <- as.numeric(dat$val)

    # weight data?
    if (!is.null(weight.by)) {
      dat2 <- data.frame(round(
        stats::xtabs(
          weights ~ x,
          data = data.frame(weights = weight.by, x = x),
          na.action = stats::na.pass,
          exclude = NULL
        ),
        0
      ))
    } else {
      # create frequency table
      dat2 <- data.frame(table(x, useNA = "always"))
    }
    colnames(dat2) <- c("val", "frq")
    dat2$val <- to_value(dat2$val, keep.labels = F)

    # join frq table and label columns
    mydat <- suppressMessages(dplyr::full_join(dat, dat2))

    # replace NA with 0, for proper percentages, i.e.
    # missing values don't appear (zero counts)
    mydat$frq <- suppressMessages(sjmisc::replace_na(mydat$frq, value = 0))
  } else {
    # weight data?
    if (!is.null(weight.by)) {
      mydat <- data.frame(round(
        stats::xtabs(
          weights ~ x,
          data = data.frame(weights = weight.by, x = x),
          na.action = stats::na.pass,
          exclude = NULL
        ),
        0
      ))
    } else {
      # if we have no labels, do simple frq table
      mydat <- data.frame(table(x, useNA = "always"))
    }

    colnames(mydat) <- c("val", "frq")

    # add values as label
    mydat$label <- as.character("<none>")
  }

  # need numeric
  if (is.factor(x) || is.character(x)) {
    x <- to_value(x, keep.labels = F)
  }

  # check if we have any NA-values - if not, add row for NA's
  if (!anyNA(mydat$val)) {
    mydat <- tibble::add_row(
      mydat,
      val = NA,
      label = NA,
      frq = 0
    )
  }

  # valid values are one row less, because last row is NA row
  valid.vals <- nrow(mydat) - 1

  # sort categories ascending or descending
  if (!is.null(sort.frq) && (sort.frq == "asc" || sort.frq == "desc")) {
    ord <- order(mydat$frq[seq_len(valid.vals)], decreasing = (sort.frq == "desc"))
    mydat <- mydat[c(ord, valid.vals + 1), ]
  }

  # raw percentages
  mydat$raw.prc <- mydat$frq / sum(mydat$frq)

  # compute valud and cumulative percentages
  mydat$valid.prc <- c(mydat$frq[seq_len(valid.vals)] / sum(mydat$frq[seq_len(valid.vals)]), NA)
  mydat$cum.prc <- c(cumsum(mydat$valid.prc[seq_len(valid.vals)]), NA)

  # proper rounding
  mydat$raw.prc <- 100 * round(mydat$raw.prc, 4)
  mydat$cum.prc <- 100 * round(mydat$cum.prc, 4)
  mydat$valid.prc <- 100 * round(mydat$valid.prc, 4)

  # "rename" labels for NA values
  if (!is.null(mydat$label)) mydat$label[is.na(mydat$label)] <- "NA"

  # save original order
  reihe <- to_value(mydat$val, start.at = 1, keep.labels = F)

  # sort
  if (sort.frq == "none") mydat <- mydat[order(reihe), ]

  # add variable label and type as attribute, for print-method
  attr(mydat, "label") <- varlab
  attr(mydat, "vartype") <- vartype
  attr(mydat, "mean") <- mean.value
  attr(mydat, "sd") <- sd.value

  mydat
}


get_grouped_title <- function(x, grps, i, sep = "\n") {
  # create title for first grouping level
  tp <- get_title_part(x, grps, 1, i)
  title <- sprintf("%s: %s", tp[1], tp[2])

  # do we have another groupng variable?
  if (length(attr(x, "vars", exact = T)) > 1) {
    tp <- get_title_part(x, grps, 2, i)
    title <- sprintf("%s%s%s: %s", title, sep, tp[1], tp[2])
  }

  # return title
  title
}


get_title_part <- function(x, grps, level, i) {
  # prepare title for group
  var.name <- colnames(grps)[level]

  # get values from value labels
  vals <- sjlabelled::get_values(x[[var.name]])
  # if we have no value labels, get values directly
  if (is.null(vals)) vals <- unique(x[[var.name]])
  # find position of value labels for current group
  lab.pos <- which(vals == grps[[var.name]][i])

  # get variable and value labels
  t1 <- sjlabelled::get_label(x[[var.name]], def.value = var.name)
  t2 <- sjlabelled::get_labels(x[[var.name]])[lab.pos]

  # if we have no value label, use value instead
  if (is.null(t2)) t2 <- vals[lab.pos]

  # generate title
  c(t1, t2)
}


#' @importFrom tidyr nest
#' @importFrom dplyr filter
#' @importFrom stats complete.cases
#' @importFrom rlang .data
get_grouped_data <- function(x) {
  # nest data frame
  grps <- tidyr::nest(x)

  # remove NA category for grouped data
  cc <- grps %>%
    dplyr::select(-.data$data) %>%
    stats::complete.cases()

  # select only complete cases
  grps <- dplyr::filter(grps, !! cc)

  # arrange data

  if (length(attr(x, "vars", exact = T)) == 1)
    reihe <- order(grps[[1]])
  else
    reihe <- order(grps[[1]], grps[[2]])

  grps <- grps[reihe, , drop = FALSE]

  grps
}
