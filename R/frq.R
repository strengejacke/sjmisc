#' @title Frequency table of labelled variables
#' @name frq
#'
#' @description This function returns a frequency table of labelled vectors, as data frame.
#'
#' @param sort.frq Determines whether categories should be sorted
#'   according to their frequencies or not. Default is \code{"none"}, so
#'   categories are not sorted by frequency. Use \code{"asc"} or
#'   \code{"desc"} for sorting categories ascending or descending order.
#' @param weights Bare name, or name as string, of a variable in \code{x}
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
#' @param show.na Logical, or \code{"auto"}. If \code{TRUE}, the output always
#'   contains information on missing values, even if variables have no missing
#'   values. If \code{FALSE}, information on missing values are removed from
#'   the output. If \code{show.na = "auto"}, information on missing values
#'   is only shown when variables actually have missing values, else it's not
#'   shown.
#' @param grp.strings Numeric, if not \code{NULL}, groups string values in
#'   character vectors, based on their similarity. See \code{\link{group_str}}
#'   and \code{\link{str_find}} for details on grouping, and their
#'   \code{precision}-argument to get more details on the distance of strings
#'   to be treated as equal.
#' @param min.frq Numeric, indicating the minimum frequency for which a
#'   value will be shown in the output (except for the missing values, prevailing
#'   \code{show.na}). Default value for \code{min.frq} is \code{0}, so all value
#'   frequencies are shown. All values or categories that have less than
#'   \code{min.frq} occurences in the data will be summarized in a \code{"n < 100"}
#'   category.
#' @param rm.labels Logical, if \code{TRUE}, \code{label} column is removed from
#'   \code{frq} output. Default is \code{FALSE}.
#' @param title String, will be used as alternative title to the variable
#'   label. If \code{x} is a grouped data frame, \code{title} must be a
#'   vector of same length as groups.
#' @param file Destination file, if the output should be saved as file.
#'   Only used when \code{out} is not \code{"txt"}.
#' @param encoding Character vector, indicating the charset encoding used
#'   for variable and value labels. Default is \code{"UTF-8"}. Only used
#'   when \code{out} is not \code{"txt"}.
#'
#' @inheritParams descr
#' @inheritParams to_factor
#'
#' @return A list of data frames with values, value labels, frequencies, raw, valid and
#'           cumulative percentages of \code{x}.
#'
#' @details The \dots-argument not only accepts variable names or expressions
#'   from \code{\link[tidyselect]{select_helpers}}. You can also use logical
#'   conditions, math operations, or combining variables to produce "crosstables".
#'   See 'Examples' for more details.
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
#' # simple vector
#' data(efc)
#' frq(efc$e42dep)
#'
#' # remove labels
#' frq(efc$e42dep, rm.labels = TRUE)
#'
#' # with grouped data frames, in a pipe
#' library(dplyr)
#' efc %>%
#'   group_by(e16sex, c172code) %>%
#'   frq(e16sex, c172code, e42dep)
#'
#' # show only categories with a minimal amount of frequencies
#' frq(mtcars$gear)
#'
#' frq(mtcars$gear, min.frq = 10)
#'
#' frq(mtcars$gear, min.frq = 15)
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
#' # also works on grouped data frames
#' efc %>%
#'   group_by(c172code) %>%
#'   frq(is.na(nur_pst))
#'
#' # group variables with large range and with weights
#' efc$weights <- abs(rnorm(n = nrow(efc), mean = 1, sd = .5))
#' frq(efc, c160age, auto.grp = 5, weights = weights)
#'
#' # different weight options
#' frq(efc, c172code, weights = weights)
#' frq(efc, c172code, weights = "weights")
#' frq(efc, c172code, weights = efc$weights)
#' frq(efc$c172code, weights = efc$weights)
#'
#' # group string values
#' dummy <- efc[1:50, 3, drop = FALSE]
#' dummy$words <- sample(
#'   c("Hello", "Helo", "Hole", "Apple", "Ape",
#'     "New", "Old", "System", "Systemic"),
#'   size = nrow(dummy),
#'   replace = TRUE
#' )
#'
#' frq(dummy)
#' frq(dummy, grp.strings = 2)
#'
#' #### other expressions than variables
#'
#' # logical conditions
#' frq(mtcars, cyl ==6)
#'
#' frq(efc, is.na(nur_pst), contains("cop"))
#'
#' iris %>%
#'   frq(starts_with("Petal"), Sepal.Length > 5)
#'
#' # computation of variables "on the fly"
#' frq(mtcars, (gear + carb) / cyl)
#'
#' # crosstables
#' set.seed(123)
#' d <- data.frame(
#'   var_x = sample(letters[1:3], size = 30, replace = TRUE),
#'   var_y = sample(1:2, size = 30, replace = TRUE),
#'   var_z = sample(LETTERS[8:10], size = 30, replace = TRUE)
#' )
#' table(d$var_x, d$var_z)
#' frq(d, paste0(var_x, var_z))
#' frq(d, paste0(var_x, var_y, var_z))
#'
#' @importFrom stats na.omit
#' @importFrom dplyr full_join select_if select group_keys
#' @importFrom sjlabelled get_label get_labels get_values copy_labels
#' @importFrom purrr map_if
#' @importFrom rlang quo_name enquo
#' @export
frq <- function(x,
                ...,
                sort.frq = c("none", "asc", "desc"),
                weights = NULL,
                auto.grp = NULL,
                show.strings = TRUE,
                show.na = TRUE,
                grp.strings = NULL,
                min.frq = 0,
		rm.labels = FALSE,
                out = c("txt", "viewer", "browser"),
                title = NULL,
                encoding = "UTF-8",
                file = NULL) {

  out <- match.arg(out)

  if (out != "txt" && !requireNamespace("sjPlot", quietly = TRUE)) {
    message("Package `sjPlot` needs to be loaded to print HTML tables.")
    out <- "txt"
  }

  # check min.frq value
  if (!is.numeric(min.frq)) {
    message("min.frq value is not numeric. Returned output assumes default value 0.")
    min.frq <- 0
  }

  # get dot data
  xw <- get_dot_data(x, dplyr::enquos(...))

  if (missing(weights)) {
    w <- NULL
    x <- xw
  } else {
    w <- try(rlang::quo_name(rlang::enquo(weights)), silent = TRUE)
    if (inherits(w, "try-error")) w <- NULL

    w.string <- try(eval(weights), silent = TRUE)
    if (!inherits(w.string, "try-error") && is.character(w.string)) w <- w.string

    if (!sjmisc::is_empty(w) && w != "NULL" && !obj_has_name(xw, w) && obj_has_name(x, w)) {
      x <- dplyr::bind_cols(xw, data.frame(x[[w]]))
      colnames(x)[ncol(x)] <- w
    } else if (!sjmisc::is_empty(string_contains("$", w)) && length(w.string) > 1 && is.numeric(w.string)) {
      x <- cbind(xw, data.frame(w.string))
      w <- sub("(.*)\\$(.*)", "\\2", w)
      colnames(x)[ncol(x)] <- w
    } else {
      message(sprintf("Weights `%s` not found in data.", w))
      w <- NULL
      x <- xw
    }
  }


  if (!isTRUE(show.na)) {
    # remove empty columns

    rem.col <- empty_cols(x)

    if (!sjmisc::is_empty(rem.col)) {
      rem.vars <- colnames(x)[rem.col]
      x <- remove_empty_cols(x)

      message(sprintf("Following %i variables have only missing values and are not shown:", length(rem.vars)))
      cat(paste(sprintf("%s [%i]", rem.vars, rem.col), collapse = ", "))
      cat("\n")
    }
  }


  # match args
  sort.frq <- match.arg(sort.frq)

  # return values
  dataframes <- list()


  # remove strings from output, if requested
  # and check if there are any variables left to print

  if (!show.strings)
    x <- dplyr::select_if(x, no_character)

  if ((all(sjmisc::is_empty(stats::na.omit(x), first.only = FALSE)) && show.na == FALSE) || all(suppressMessages(replace_na(sjmisc::is_empty(x, first.only = FALSE, all.na.empty = FALSE), value = FALSE))))
    return(NULL)


  # group strings

  if (!is.null(grp.strings)) {
    a <- attributes(x)

    if (!is.data.frame(x)) {
      was.df <- FALSE
      x <- data.frame(x, stringsAsFactors = FALSE)
    } else
      was.df <- TRUE


    x <- x %>%
      purrr::map_if(is.character, ~ group_str(
        strings = .x, precision = grp.strings, remove.empty = FALSE)
      ) %>%
      as.data.frame(stringsAsFactors = FALSE)

    if (was.df)
      attributes(x) <- a
    else
      attributes(x[[1]]) <- a
  }


  # do we have a grouped data frame?
  if (inherits(x, "grouped_df")) {

    grkey <- colnames(dplyr::group_keys(x))
    for (i in grkey) {
      if (is.character(x[[i]])) x[[i]] <- as.factor(x[[i]])
    }

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

        # user-defined title
        if (!is.null(title) && length(title) >= i)
          gr.title <- title[i]
        else
          gr.title <- NULL

        # iterate data frame, but don't select
        # weighting variable
        if (is.null(w) || colnames(tmp)[1] != w) {
          dummy <-
            frq_helper(
              x = tmp[[1]],
              sort.frq = sort.frq,
              weight.by = wb,
              cn = colnames(tmp)[1],
              auto.grp = auto.grp,
              title = gr.title,
              show.na = show.na,
              min.frq = min.frq,
	      rm.labels = rm.labels
            )

          attr(dummy, "group") <- get_grouped_title(x, grps, i, sep = ", ", long = FALSE)

          # save data frame for return value
          dataframes[[length(dataframes) + 1]] <- dummy
        }
      }
    }

  } else {
    # if we don't have data frame, coerce
    if (!is.data.frame(x)) x <- data.frame(x, stringsAsFactors = FALSE)

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
            auto.grp = auto.grp,
            title = title,
            show.na = show.na,
            min.frq = min.frq,
	    rm.labels = rm.labels
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
  attr(dataframes, "encoding") <- encoding
  attr(dataframes, "file") <- file

  dataframes
}


#' @importFrom dplyr n_distinct full_join bind_rows
#' @importFrom stats na.omit xtabs na.pass sd weighted.mean
#' @importFrom sjlabelled get_labels get_label as_numeric
frq_helper <- function(x, sort.frq, weight.by, cn, auto.grp, title = NULL, show.na = TRUE, min.frq = 0, rm.labels = FALSE) {
  # remember type
  vartype <- var_type(x)

  # convert NaN and Inf to missing
  x <- zap_inf(x)

  # variable with only missing?
  if (length(stats::na.omit(x)) == 0 && show.na == FALSE) {
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
      attr.only = TRUE,
      values = "n",
      non.labelled = TRUE
    )


  # if we don't have variable label, use column name
  if (sjmisc::is_empty(varlab) && !sjmisc::is_empty(cn))
    varlab <- cn
  else if (!sjmisc::is_empty(varlab) && !sjmisc::is_empty(cn))
    varlab <- sprintf("%s (%s)", varlab, cn)


  # do we have a labelled vector?
  if (!is.null(labels)) {
    # add rownames and values as columns
    dat <- data_frame(
      n = names(labels),
      v = as.character(labels)
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

    if (!anyNA(suppressWarnings(as.numeric(attr(mydat$val, "levels"))))) {
      mydat$val <- sjlabelled::as_numeric(mydat$val, keep.labels = F)
    }

    # add values as label
    mydat$label <- as.character("<none>")
    mydat <- mydat[c("val", "label", "frq")]
  }

  min.frq.string <- sprintf("n < %g", min.frq)

  if (any(mydat$frq[!is.na(mydat$val)] < min.frq)) {
	  mydatS1 <- mydat[which(mydat$frq >= min.frq | is.na(mydat$val)), ]
	  mydatS2 <- mydat[which(mydat$frq < min.frq & !is.na(mydat$val)), ]

	  mydatS3 <- data_frame(
      val = min.frq.string,
      label = "<none>",
      frq = sum(mydatS2$frq)
    )
	  if (mydatS3$frq == 0) {
		  mydat <- mydatS1
	  } else {
		  mydat <- rbind(mydatS1, mydatS3)

		  rows.length <- length(row.names(mydat))

		  row.names(mydat) <- c(
					row.names(mydat)[-c(rows.length, rows.length - 1)],
					as.character(as.integer(row.names(mydat)[rows.length - 1]) + 1),
					row.names(mydat)[rows.length - 1]
		  )
	  }
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

  if (!all(is.na(mydat$val))) {

    extra.vals <- 1
    # Momentarily, in order to sort categories, we consider lower frequencies subtotal as a non valid value
    if (is.na(mydat$val[valid.vals]) & mydat$val[valid.vals + 1] == min.frq.string) {
      valid.vals <- valid.vals - 1
      extra.vals <- 2
    }


    # sort categories ascending or descending
    if (!is.null(sort.frq) && (sort.frq == "asc" || sort.frq == "desc")) {
      ord <- order(mydat$frq[seq_len(valid.vals)], decreasing = (sort.frq == "desc"))
    } else {
      ord <- seq_len(valid.vals)
    }
    mydat <- mydat[c(ord, (valid.vals + extra.vals):(valid.vals + 1)), ]
  }

  valid.vals <- nrow(mydat) - 1

  # raw percentages
  mydat$raw.prc <- mydat$frq / sum(mydat$frq)

  # compute valid and cumulative percentages
  mydat$valid.prc <- c(mydat$frq[seq_len(valid.vals)] / sum(mydat$frq[seq_len(valid.vals)]), NA)
  mydat$cum.prc <- c(cumsum(mydat$valid.prc[seq_len(valid.vals)]), NA)

  # proper rounding
  mydat$raw.prc <- 100 * round(mydat$raw.prc, 4)
  mydat$cum.prc <- 100 * round(mydat$cum.prc, 4)
  mydat$valid.prc <- 100 * round(mydat$valid.prc, 4)

  # "rename" labels for NA values
  if (!is.null(mydat$label)) mydat$label[is.na(mydat$val)] <- NA_character_

  if (!all(is.na(mydat$val))) {
    if (extra.vals == 1) {
      # save original order
      reihe <- sjlabelled::as_numeric(mydat$val, start.at = 1, keep.labels = F)
      # sort
      if (sort.frq == "none") mydat <- mydat[order(reihe), ]
    } else if (extra.vals == 2) {
      # save original order
      reihe <- suppressWarnings(sjlabelled::as_numeric(mydat$val[-c(valid.vals,valid.vals+1)], start.at = 1, keep.labels = F))
      # sort
      if (sort.frq == "none") mydat <- mydat[c(order(reihe), valid.vals, valid.vals+1), ]
    }
  }

  # remove NA, if requested
  has.na <- mydat$frq[nrow(mydat)] > 0
  if ((!is.logical(show.na) && show.na == "auto" && !has.na) || identical(show.na, FALSE))
    mydat <- mydat[-nrow(mydat), ]


  # remove labels if rm.labels == TRUE
  if (isTRUE(rm.labels)) {
	  mydat$label <- NULL
  }


  # add variable label and type as attribute, for print-method

  if (!is.null(title)) {
    attr(mydat, "label") <- title
    attr(mydat, "vartype") <- ""

  } else {
    attr(mydat, "label") <- varlab
    attr(mydat, "vartype") <- vartype
  }

  attr(mydat, "mean") <- mean.value
  attr(mydat, "sd") <- sd.value

  mydat
}


get_grouped_title <- function(x, grps, i, sep = ", ", long = FALSE) {
  # create title for first grouping level
  tp <- get_title_part(x, grps, 1, i)

  if (long)
    title <- sprintf("%s: %s", tp[1], tp[2])
  else
    title <- sprintf("%s", tp[2])

  # do we have another groupng variable?
  if (length(dplyr::group_vars(x)) > 1) {
    tp <- get_title_part(x, grps, 2, i)

    if (long)
      title <- sprintf("%s%s%s: %s", title, sep, tp[1], tp[2])
    else
      title <- sprintf("%s%s%s", title, sep, tp[2])
  }

  # return title
  title
}


get_title_part <- function(x, grps, level, i) {
  # prepare title for group
  var.name <- colnames(grps)[level]

  # get values from value labels
  vals <- sjlabelled::get_values(x[[var.name]])
  t2 <- NULL

  # if we have no value labels, get values directly
  if (is.null(vals)) {
    vals <- grps[[var.name]]
    if (is.factor(grps[[var.name]])) vals <- as.character(vals)
    lab.pos <- i
  } else {
    # find position of value labels for current group
    lab.pos <- which(vals == grps[[var.name]][i])
    t2 <- sjlabelled::get_labels(x[[var.name]])[lab.pos]
  }

  # get variable and value labels
  t1 <- sjlabelled::get_label(x[[var.name]], def.value = var.name)

  # if we have no value label, use value instead
  if (sjmisc::is_empty(t2)) t2 <- vals[lab.pos]

  # generate title
  c(t1, t2)
}


#' @importFrom dplyr filter group_vars
#' @importFrom stats complete.cases
#' @importFrom rlang .data
get_grouped_data <- function(x) {
  # nest data frame
  grps <- .nest(x)

  # remove NA category for grouped data
  cc <- grps %>%
    dplyr::select(-.data$data) %>%
    stats::complete.cases()

  # select only complete cases
  grps <- dplyr::filter(grps, !! cc)

  # arrange data

  if (length(dplyr::group_vars(x)) == 1)
    reihe <- order(grps[[1]])
  else
    reihe <- order(grps[[1]], grps[[2]])

  grps <- grps[reihe, , drop = FALSE]

  grps
}


no_character <- function(x) !is.character(x)
