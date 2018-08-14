#' @title Frequency table of labelled variables
#' @name frq
#'
#' @description This function returns a frequency table of labelled vectors, as data frame.
#'
#' @param sort.frq Determines whether categories should be sorted
#'   according to their frequencies or not. Default is \code{"none"}, so
#'   categories are not sorted by frequency. Use \code{"asc"} or
#'   \code{"desc"} for sorting categories ascending or descending order.
#' @param weight.by Bare name, or name as string, of a variable in \code{x}
#'   that indicates the vector of weights, which will be applied to weight all
#'   observations. Default is \code{NULL}, so no weights are used.
#' @param auto.grp Numeric value, indicating the minimum amount of unique
#'   values in a variable, at which automatic grouping into smaller  units
#'   is done (see \code{\link{group_var}}). Default value for \code{auto.group}
#'   is \code{NULL}, i.e. auto-grouping is off.
#' @param show.strings Logical, if \code{TRUE}, frequency tables for character
#'   vectors will not be printed. This is useful when printing frequency tables
#'   of all variables from a data frame, and due to computational reasons
#'   character vectors should not be printed.
#' @param grp.strings Numeric, if not \code{NULL}, groups string values in
#'   character vectors, based on their similarity. The similarity is estimated
#'   with the \pkg{stringdist}-package. See \code{\link{group_str}} for details
#'   on grouping, and that function's \code{maxdist}-argument to get more
#'   details on the distance of strings to be treated as equal.
#'
#' @inheritParams descr
#'
#' @return A list of data frames with values, value labels, frequencies, raw, valid and
#'           cumulative percentages of \code{x}.
#'
#' @note \code{x} may also be a grouped data frame (see \code{\link[dplyr]{group_by}})
#'       with up to two grouping variables. Frequency tables are created for each
#'       subgroup then.
#'       \cr \cr
#'       The \code{print()}-method adds a table header with information on the
#'       variable label, variable type, total and valid N, and mean and
#'       standard deviations. Mean and SD are \emph{always} printed, even for
#'       categorical variables (factors) or character vectors. In this case,
#'       values are coerced into numeric vector to calculate the summary
#'       statistics.
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
#' # group variables with large range
#' frq(efc, c160age)
#' frq(efc, c160age, auto.grp = 5)
#'
#' # and with weights
#' efc$weights <- abs(rnorm(n = nrow(efc), mean = 1, sd = .5))
#' frq(efc, c160age, auto.grp = 5, weight.by = weights)
#'
#' # group string values
#' \dontrun{
#' dummy <- efc %>% dplyr::select(3)
#' dummy$words <- sample(
#'   c("Hello", "Helo", "Hole", "Apple", "Ape",
#'     "New", "Old", "System", "Systemic"),
#'   size = nrow(dummy),
#'   replace = TRUE
#' )
#'
#' frq(dummy)
#' frq(dummy, grp.strings = 2)}
#'
#' @importFrom stats na.omit
#' @importFrom dplyr full_join select_if select
#' @importFrom sjlabelled get_label get_labels get_values copy_labels
#' @importFrom purrr map_if
#' @importFrom rlang quo_name enquo
#' @export
frq <- function(x,
                ...,
                sort.frq = c("none", "asc", "desc"),
                weight.by = NULL,
                auto.grp = NULL,
                show.strings = TRUE,
                grp.strings = NULL,
                out = c("txt", "viewer", "browser")) {

  out <- match.arg(out)

  if (out != "txt" && !requireNamespace("sjPlot", quietly = TRUE)) {
    message("Package `sjPlot` needs to be loaded to print HTML tables.")
    out <- "txt"
  }


  # get dot data
  xw <- get_dot_data(x, dplyr::quos(...))

  if (missing(weight.by)) {
    w <- NULL
    x <- xw
  } else {
    w <- rlang::quo_name(rlang::enquo(weight.by))

    w.string <- tryCatch(
      {
        eval(weight.by)
      },
      error = function(x) { NULL },
      warning = function(x) { NULL },
      finally = function(x) { NULL }
    )

    if (!is.null(w.string) && is.character(w.string)) w <- w.string


    if (!sjmisc::is_empty(w) && w != "NULL" && !obj_has_name(xw, w) && obj_has_name(x, w)) {
      x <- dplyr::bind_cols(xw, dplyr::select(x, !! w))
    } else {
      message(sprintf("Weights `%s` not found in data.", w))
      w <- NULL
      x <- xw
    }
  }


  # remove empty columns

  rem.col <- empty_cols(x)

  if (!sjmisc::is_empty(rem.col)) {
    rem.vars <- colnames(x)[rem.col]
    x <- remove_empty_cols(x)

    message(sprintf("Following %i variables have only missing values and are not shown:", length(rem.vars)))
    cat(paste(sprintf("%s [%i]", rem.vars, rem.col), collapse = ", "))
    cat("\n")
  }


  # match args
  sort.frq <- match.arg(sort.frq)

  # return values
  dataframes <- list()


  # remove strings from output, if requested
  # and check if there are any variables left to print

  if (!show.strings)
    x <- dplyr::select_if(x, no_character)

  if (sjmisc::is_empty(x)) return(NULL)


  # group strings

  if (!is.null(grp.strings)) {
    x <- x %>%
      purrr::map_if(is.character, ~ group_str(
        strings = .x, maxdist = grp.strings, remove.empty = FALSE)
      ) %>%
      as.data.frame()
  }


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

        if (!is.null(w))
          wb <- grps$data[[i]][[w]]
        else
          wb <- NULL

        # iterate data frame, but don't select
        # weighting variable
        if (is.null(w) || colnames(tmp)[1] != w) {
          dummy <-
            frq_helper(
              x = tmp[[1]],
              sort.frq = sort.frq,
              weight.by = wb,
              cn = colnames(tmp)[1],
              auto.grp = auto.grp
            )

          attr(dummy, "group") <- get_grouped_title(x, grps, i, sep = "\n")

          # save data frame for return value
          dataframes[[length(dataframes) + 1]] <- dummy
        }
      }
    }

  } else {
    # if we don't have data frame, coerce
    if (!is.data.frame(x)) x <- data.frame(x)

    if (!is.null(w))
      wb <- x[[w]]
    else
      wb <- NULL

    for (i in seq_len(ncol(x))) {
      # iterate data frame, but don't select
      # weighting variable
      if (is.null(w) || colnames(x)[i] != w) {
        dummy <-
          frq_helper(
            x = x[[i]],
            sort.frq = sort.frq,
            weight.by = wb,
            cn = colnames(x)[i],
            auto.grp = auto.grp
          )

        # save data frame for return value
        dataframes[[length(dataframes) + 1]] <- dummy
      }
    }
  }

  # add class-attr for print-method()
  if (out == "txt")
    class(dataframes) <- c("sjmisc_frq", "list")
  else
    class(dataframes) <- c("sjt_frq", "list")

  # save how to print output
  attr(dataframes, "print") <- out

  dataframes
}


#' @importFrom dplyr n_distinct full_join bind_rows
#' @importFrom stats na.omit xtabs na.pass sd weighted.mean
#' @importFrom sjlabelled get_labels get_label as_numeric
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
    # make sure, vector and weights have same length, so remove missing from weights

    weight.by[is.na(xnum)] <- NA
    xnum[is.na(weight.by)] <- NA
    x[is.na(weight.by)] <- NA

    mean.value <- stats::weighted.mean(stats::na.omit(xnum), w = stats::na.omit(weight.by))

    if (requireNamespace("sjstats", quietly = TRUE))
      sd.value <- sjstats::wtd_sd(stats::na.omit(xnum), weights = stats::na.omit(weight.by))
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
  labels <-
    sjlabelled::get_labels(
      x,
      attr.only = T,
      values = "n",
      non.labelled = T
    )


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
      dat$val <- sjlabelled::as_numeric(dat$val, keep.labels = F)
    else
      dat$val <- as.numeric(dat$val)

    # weight data?
    if (!is.null(weight.by)) {
      dat2 <- data.frame(round(
        stats::xtabs(
          weights ~ x,
          data = data.frame(weights = stats::na.omit(weight.by), x = stats::na.omit(x)),
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
    dat2$val <- sjlabelled::as_numeric(dat2$val, keep.labels = F)

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
          data = data.frame(weights = stats::na.omit(weight.by), x = stats::na.omit(x)),
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
    x <- sjlabelled::as_numeric(x, keep.labels = F)
  }

  # check if we have any NA-values - if not, add row for NA's
  if (!anyNA(mydat$val)) {
    mydat <- dplyr::bind_rows(
      mydat,
      data.frame(
        val = NA,
        label = NA,
        frq = 0
      )
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
  reihe <- sjlabelled::as_numeric(mydat$val, start.at = 1, keep.labels = F)

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


no_character <- function(x) !is.character(x)
