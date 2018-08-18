#' @title Recode variables
#' @name rec
#'
#' @description \code{rec()} recodes values of variables, where variable
#'   selection is based on variable names or column position, or on
#'   select helpers (see documentation on \code{...}). \code{rec_if()} is a
#'   scoped variant of \code{rec()}, where recoding will be applied only
#'   to those variables that match the logical condition of \code{predicate}.
#'
#' @seealso \code{\link{set_na}} for setting \code{NA} values, \code{\link{replace_na}}
#'            to replace \code{NA}'s with specific value, \code{\link{recode_to}}
#'            for re-shifting value ranges and \code{\link{ref_lvl}} to change the
#'            reference level of (numeric) factors.
#'
#' @param predicate A predicate function to be applied to the columns. The
#'   variables for which \code{predicate} returns \code{TRUE} are selected.
#' @param rec String with recode pairs of old and new values. See 'Details'
#'   for examples. \code{\link{rec_pattern}} is a convenient function to
#'   create recode strings for grouping variables.
#' @param as.num Logical, if \code{TRUE}, return value will be numeric, not a factor.
#' @param var.label Optional string, to set variable label attribute for the
#'   returned variable (see vignette \href{https://cran.r-project.org/package=sjlabelled/vignettes/intro_sjlabelled.html}{Labelled Data and the sjlabelled-Package}).
#'   If \code{NULL} (default), variable label attribute of \code{x} will
#'   be used (if present). If empty, variable label attributes will be removed.
#' @param val.labels Optional character vector, to set value label attributes
#'   of recoded variable (see vignette \href{https://cran.r-project.org/package=sjlabelled/vignettes/intro_sjlabelled.html}{Labelled Data and the sjlabelled-Package}).
#'   If \code{NULL} (default), no value labels will be set. Value labels
#'   can also be directly defined in the \code{rec}-syntax, see
#'   'Details'.
#' @param append Logical, if \code{TRUE} (the default) and \code{x} is a data frame,
#'   \code{x} including the new variables as additional columns is returned;
#'   if \code{FALSE}, only the new variables are returned.
#' @param suffix String value, will be appended to variable (column) names of
#'   \code{x}, if \code{x} is a data frame. If \code{x} is not a data
#'   frame, this argument will be ignored. The default value to suffix
#'   column names in a data frame depends on the function call:
#'   \itemize{
#'     \item recoded variables (\code{rec()}) will be suffixed with \code{"_r"}
#'     \item recoded variables (\code{recode_to()}) will be suffixed with \code{"_r0"}
#'     \item dichotomized variables (\code{dicho()}) will be suffixed with \code{"_d"}
#'     \item grouped variables (\code{split_var()}) will be suffixed with \code{"_g"}
#'     \item grouped variables (\code{group_var()}) will be suffixed with \code{"_gr"}
#'     \item standardized variables (\code{std()}) will be suffixed with \code{"_z"}
#'     \item centered variables (\code{center()}) will be suffixed with \code{"_c"}
#'     \item de-meaned variables (\code{de_mean()}) will be suffixed with \code{"_dm"}
#'     \item grouped-meaned variables (\code{de_mean()}) will be suffixed with \code{"_gm"}
#'   }
#'
#' @inheritParams to_factor
#'
#' @return \code{x} with recoded categories. If \code{x} is a data frame,
#'   for \code{append = TRUE}, \code{x} including the recoded variables
#'   as new columns is returned; if \code{append = FALSE}, only
#'   the recoded variables will be returned.
#'
#' @details  The \code{rec} string has following syntax:
#'   \describe{
#'     \item{recode pairs}{each recode pair has to be separated by a \code{;}, e.g. \code{rec = "1=1; 2=4; 3=2; 4=3"}}
#'     \item{multiple values}{multiple old values that should be recoded into a new single value may be separated with comma, e.g. \code{"1,2=1; 3,4=2"}}
#'     \item{value range}{a value range is indicated by a colon, e.g. \code{"1:4=1; 5:8=2"} (recodes all values from 1 to 4 into 1, and from 5 to 8 into 2)}
#'     \item{value range for doubles}{for double vectors (with floating points), all values within the specified range are recoded; e.g. \code{1:2.5=1;2.6:3=2} recodes 1 to 2.5 into 1 and 2.6 to 3 into 2, but 2.55 would not be recoded (since it's not included in any of the specified ranges)}
#'     \item{\code{"min"} and \code{"max"}}{minimum and maximum values are indicates by \emph{min} (or \emph{lo}) and \emph{max} (or \emph{hi}), e.g. \code{"min:4=1; 5:max=2"} (recodes all values from minimum values of \code{x} to 4 into 1, and from 5 to maximum values of \code{x} into 2)}
#'     \item{\code{"else"}}{all other values, which have not been specified yet, are indicated by \emph{else}, e.g. \code{"3=1; 1=2; else=3"} (recodes 3 into 1, 1 into 2 and all other values into 3)}
#'     \item{\code{"copy"}}{the \code{"else"}-token can be combined with \emph{copy}, indicating that all remaining, not yet recoded values should stay the same (are copied from the original value), e.g. \code{"3=1; 1=2; else=copy"} (recodes 3 into 1, 1 into 2 and all other values like 2, 4 or 5 etc. will not be recoded, but copied, see 'Examples')}
#'     \item{\code{NA}'s}{\code{\link{NA}} values are allowed both as old and new value, e.g. \code{"NA=1; 3:5=NA"} (recodes all NA into 1, and all values from 3 to 5 into NA in the new variable)}
#'     \item{\code{"rev"}}{\code{"rev"} is a special token that reverses the value order (see 'Examples')}
#'     \item{direct value labelling}{value labels for new values can be assigned inside the recode pattern by writing the value label in square brackets after defining the new value in a recode pair, e.g. \code{"15:30=1 [young aged]; 31:55=2 [middle aged]; 56:max=3 [old aged]"}. See 'Examples'.}
#'   }
#'
#' @note Please note following behaviours of the function:
#'   \itemize{
#'     \item the \code{"else"}-token should always be the last argument in the \code{rec}-string.
#'     \item Non-matching values will be set to \code{NA}, unless captured by the \code{"else"}-token.
#'     \item Tagged NA values (see \code{\link[haven]{tagged_na}}) and their value labels will be preserved when copying NA values to the recoded vector with \code{"else=copy"}.
#'     \item Variable label attributes (see, for instance, \code{\link[sjlabelled]{get_label}}) are preserved (unless changed via \code{var.label}-argument), however, value label attributes are removed (except for \code{"rev"}, where present value labels will be automatically reversed as well). Use \code{val.labels}-argument to add labels for recoded values.
#'     \item If \code{x} is a data frame, all variables should have the same categories resp. value range (else, see second bullet, \code{NA}s are produced).
#'   }
#'
#' @examples
#' data(efc)
#' table(efc$e42dep, useNA = "always")
#'
#' # replace NA with 5
#' table(rec(efc$e42dep, rec = "1=1;2=2;3=3;4=4;NA=5"), useNA = "always")
#'
#' # recode 1 to 2 into 1 and 3 to 4 into 2
#' table(rec(efc$e42dep, rec = "1,2=1; 3,4=2"), useNA = "always")
#'
#' # keep value labels. variable label is automatically preserved
#' library(dplyr)
#' efc %>%
#'   select(e42dep) %>%
#'   rec(rec = "1,2=1; 3,4=2",
#'       val.labels = c("low dependency", "high dependency")) %>%
#'   str()
#'
#' # works with mutate
#' efc %>%
#'   select(e42dep, e17age) %>%
#'   mutate(dependency_rev = rec(e42dep, rec = "rev")) %>%
#'   head()
#'
#' # recode 1 to 3 into 4 into 2
#' table(rec(efc$e42dep, rec = "min:3=1; 4=2"), useNA = "always")
#'
#' # recode 2 to 1 and all others into 2
#' table(rec(efc$e42dep, rec = "2=1; else=2"), useNA = "always")
#'
#' # reverse value order
#' table(rec(efc$e42dep, rec = "rev"), useNA = "always")
#'
#' # recode only selected values, copy remaining
#' table(efc$e15relat)
#' table(rec(efc$e15relat, rec = "1,2,4=1; else=copy"))
#'
#' # recode variables with same category in a data frame
#' head(efc[, 6:9])
#' head(rec(efc[, 6:9], rec = "1=10;2=20;3=30;4=40"))
#'
#' # recode multiple variables and set value labels via recode-syntax
#' dummy <- rec(
#'   efc, c160age, e17age,
#'   rec = "15:30=1 [young]; 31:55=2 [middle]; 56:max=3 [old]",
#'   append = FALSE
#' )
#' frq(dummy)
#'
#' # recode variables with same value-range
#' lapply(
#'   rec(
#'     efc, c82cop1, c83cop2, c84cop3,
#'     rec = "1,2=1; NA=9; else=copy",
#'     append = FALSE
#'   ),
#'   table,
#'   useNA = "always"
#' )
#'
#' # recode character vector
#' dummy <- c("M", "F", "F", "X")
#' rec(dummy, rec = "M=Male; F=Female; X=Refused")
#'
#' # recode numeric to character
#' rec(efc$e42dep, rec = "1=first;2=2nd;3=third;else=hi")
#'
#' # recode non-numeric factors
#' data(iris)
#' table(rec(iris, Species, rec = "setosa=huhu; else=copy", append = FALSE))
#'
#' # recode floating points
#' table(rec(
#'   iris, Sepal.Length, rec = "lo:5=1;5.01:6.5=2;6.501:max=3", append = FALSE
#' ))
#'
#' # preserve tagged NAs
#' library(haven)
#' x <- labelled(c(1:3, tagged_na("a", "c", "z"), 4:1),
#'               c("Agreement" = 1, "Disagreement" = 4, "First" = tagged_na("c"),
#'                 "Refused" = tagged_na("a"), "Not home" = tagged_na("z")))
#' # get current value labels
#' x
#' # recode 2 into 5; Values of tagged NAs are preserved
#' rec(x, rec = "2=5;else=copy")
#' na_tag(rec(x, rec = "2=5;else=copy"))
#'
#' # use select-helpers from dplyr-package
#' rec(
#'   efc, contains("cop"), c161sex:c175empl,
#'   rec = "0,1=0; else=1",
#'   append = FALSE
#' )
#'
#' # recode only variables that have a value range from 1-4
#' p <- function(x) min(x, na.rm = TRUE) > 0 && max(x, na.rm = TRUE) < 5
#' rec_if(efc, predicate = p, rec = "1:3=1;4=2;else=copy")
#'
#'
#' @export
rec <- function(x, ..., rec, as.num = TRUE, var.label = NULL, val.labels = NULL, append = TRUE, suffix = "_r") {
  UseMethod("rec")
}


#' @importFrom dplyr quos
#' @export
rec.default <- function(x, ..., rec, as.num = TRUE, var.label = NULL, val.labels = NULL, append = TRUE, suffix = "_r") {

  # evaluate arguments, generate data
  .dat <- get_dot_data(x, dplyr::quos(...))

  rec_core_fun(
    x = x,
    .dat = .dat,
    rec = rec,
    as.num = as.num,
    var.label = var.label,
    val.labels = val.labels,
    append = append,
    suffix = suffix
  )
}


#' @importFrom dplyr bind_cols select quos
#' @importFrom purrr map
#' @export
rec.mids <- function(x, ..., rec, as.num = TRUE, var.label = NULL, val.labels = NULL, append = TRUE, suffix = "_r") {
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
        rec_core_fun(
          x = dat,
          .dat = dat,
          rec = rec,
          as.num = as.num,
          var.label = var.label,
          val.labels = val.labels,
          append = FALSE,
          suffix = suffix
        ))
    }
  )

  final_mids_recode(ndf)
}


#' @importFrom dplyr select_if
#' @rdname rec
#' @export
rec_if <- function(x, predicate, rec, as.num = TRUE, var.label = NULL, val.labels = NULL, append = TRUE, suffix = "_r") {

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


  rec_core_fun(
    x = x,
    .dat = .dat,
    rec = rec,
    as.num = as.num,
    var.label = var.label,
    val.labels = val.labels,
    append = append,
    suffix = suffix
  )
}


rec_core_fun <- function(x, .dat, rec, as.num = TRUE, var.label = NULL, val.labels = NULL, append = TRUE, suffix = "_r") {

  if (is.data.frame(x)) {

    # remember original data, if user wants to bind columns
    orix <- x

    # iterate variables of data frame
    for (i in colnames(.dat)) {
      x[[i]] <- rec_helper(
        x = .dat[[i]],
        recodes = rec,
        as.num = as.num,
        var.label = var.label,
        val.labels = val.labels
      )
    }

    # select only recoded variables
    x <- x[colnames(.dat)]

    # add suffix to recoded variables?
    if (!is.null(suffix) && !sjmisc::is_empty(suffix)) {
      colnames(x) <- sprintf("%s%s", colnames(x), suffix)
    }

    # combine data
    if (append) x <- dplyr::bind_cols(orix, x)
  } else {
    x <- rec_helper(
      x = .dat,
      recodes = rec,
      as.num = as.num,
      var.label = var.label,
      val.labels = val.labels
    )
  }

  x
}


#' @importFrom stats na.omit
#' @importFrom sjlabelled drop_labels get_na get_label get_labels set_label set_labels
rec_helper <- function(x, recodes, as.num, var.label, val.labels) {
  # retrieve variable label
  if (is.null(var.label))
    var_lab <- sjlabelled::get_label(x)
  else
    var_lab <- var.label

  # do we have any value labels?
  val_lab <- val.labels

  # remember if NA's have been recoded...
  na_recoded <- FALSE

  # drop labels when reversing
  if (recodes == "rev") x <- sjlabelled::drop_labels(x, drop.na = TRUE)

  # get current NA values
  current.na <- sjlabelled::get_na(x)

  # do we have a factor with "x"?
  if (is.factor(x)) {
    # save variable labels before in case we just want
    # to reverse the order
    if (is.null(val_lab) && recodes == "rev") {
      val_lab <-
        rev(
          sjlabelled::get_labels(
            x,
            attr.only = TRUE,
            values = NULL,
            non.labelled = TRUE,
            drop.na = TRUE
          )
        )
    }

    if (is_num_fac(x)) {
      # numeric factors coerced to numeric
      x <- as.numeric(as.character(x))
    } else {
      # non-numeric factors coerced to character
      x <- as.character(x)
      # non-numeric factors will always be factor again
      as.num <- FALSE
    }
  }


  # is vector a double with decimals?
  with_dec <- is_float(x)


  # retrieve min and max values
  min_val <- min(x, na.rm = T)
  max_val <- max(x, na.rm = T)

  # do we have special recode-token?
  if (recodes == "rev") {

    # retrieve unique valus, sorted
    ov <- sort(unique(stats::na.omit(as.vector(x))))

    # new values should be reversed order
    nv <- rev(ov)

    # create recodes-string
    recodes <- paste(sprintf("%i=%i", ov, nv), collapse = ";")

    # when we simply reverse values, we can keep value labels
    if (is.null(val_lab)) {
      val_lab <-
        rev(
          sjlabelled::get_labels(
            x,
            attr.only = TRUE,
            values = NULL,
            non.labelled = TRUE,
            drop.na = TRUE
          )
        )
    }
  }

  # we allow direct labelling, so extract possible direct labels here
  # this piece of code is definitely not the best solution, I bet...
  # but it seems to work, and I discovered the regex-pattern by myself :-)
  # this function extracts direct value labels from the recodes-pattern and
  # creates a named vector with value labels, e.g.:
  # "18:23=1 [18to23]; 24:65=2 [24to65]; 66:max=3 [> 65]"
  # will return a named vector with value 1 to 3, where the text inside [ and ]
  # is used as name for each value
  dir.label <- unlist(lapply(strsplit(
    unlist(regmatches(
      recodes,
      gregexpr(
        pattern = "=([^\\]]*)\\]",
        text = recodes,
        perl = T
      )
    )),
    split = "\\[", perl = T
  ),
  function(x) {
    tmp <- as.numeric(trim(substr(x[1], 2, nchar(x[1]))))
    names(tmp) <- trim(substr(x[2], 1, nchar(x[2]) - 1))
    tmp
  }))

  # if we found any labels, replace the value label argument
  if (!is.null(dir.label) && !sjmisc::is_empty(dir.label)) val_lab <- dir.label

  # remove possible direct labels from recode pattern
  recodes <- gsub(
    pattern = "\\[([^\\[]*)\\]",
    replacement = "",
    x = recodes,
    useBytes = TRUE,
    perl = TRUE
  )

  # prepare and clean recode string
  # retrieve each single recode command
  rec_string <- unlist(strsplit(recodes, ";", fixed = TRUE))

  # remove spaces
  rec_string <- gsub(" ", "", rec_string, useBytes = TRUE, fixed = TRUE)

  # remove line breaks
  rec_string <- gsub("\n", "", rec_string, useBytes = TRUE, fixed = F)
  rec_string <- gsub("\r", "", rec_string, useBytes = TRUE, fixed = F)

  # replace min and max placeholders
  rec_string <- gsub("(min)\\b", as.character(min_val), rec_string, useBytes = TRUE, perl = TRUE)
  rec_string <- gsub("(lo)\\b", as.character(min_val), rec_string, useBytes = TRUE, perl = TRUE)
  rec_string <- gsub("(max)\\b", as.character(max_val), rec_string, useBytes = TRUE, perl = TRUE)
  rec_string <- gsub("(hi)\\b", as.character(max_val), rec_string, useBytes = TRUE, perl = TRUE)

  # retrieve all recode-pairs, i.e. all old-value = new-value assignments
  rec_pairs <- strsplit(rec_string, "=", fixed = TRUE)

  # check for correct syntax
  correct_syntax <- unlist(lapply(rec_pairs, function(r) if (length(r) != 2) r else NULL))

  # found any errors in syntax?
  if (!is.null(correct_syntax)) {
    stop(sprintf("?Syntax error in argument \"%s\"", paste(correct_syntax, collapse = "=")), call. = F)
  }

  # the new, recoded variable
  new_var <- rep(-Inf, length(x))

  # now iterate all recode pairs
  # and do each recoding step
  for (i in seq_len(length(rec_pairs))) {
    # retrieve recode pairs as string, and start with separaring old-values
    # at comma separator
    old_val_string <- unlist(strsplit(rec_pairs[[i]][1], ",", fixed = TRUE))
    new_val_string <- rec_pairs[[i]][2]
    new_val <- c()

    # check if new_val_string is correct syntax
    if (new_val_string == "NA") {
      # here we have a valid NA specification
      new_val <- NA
    } else if (new_val_string == "copy") {
      # copy all remaining values, i.e. don't recode
      # remaining values that have not else been specified
      # or recoded. NULL indicates the "copy"-token
      new_val <- NULL
    } else {
      # can new value be converted to numeric?
      new_val <- suppressWarnings(as.numeric(new_val_string))
      # if not, assignment is wrong
      if (is.na(new_val)) new_val <- new_val_string
    }

    # retrieve and check old values
    old_val <- c()
    for (j in seq_len(length(old_val_string))) {
      # copy to shorten code
      ovs <- old_val_string[j]

      # check if old_val_string is correct syntax
      if (ovs == "NA") {
        # here we have a valid NA specification
        # add value to vector of old values that
        # should be recoded
        old_val <- c(old_val, NA)
      } else if (ovs == "else") {
        # here we have a valid "else" specification
        # add all remaining values (in the new variable
        # created as "-Inf") to vector that should be recoded
        old_val <- -Inf
        break
      } else if (length(grep(":", ovs, fixed = TRUE)) > 0) {
        # this value indicates a range of values to be recoded, because
        # we have found a colon. now copy from and to values from range
        from <- suppressWarnings(as.numeric(unlist(strsplit(ovs, ":", fixed = T))[1]))
        to <- suppressWarnings(as.numeric(unlist(strsplit(ovs, ":", fixed = T))[2]))
        # check for valid range values
        if (is.na(from) || is.na(to)) {
          stop(sprintf("?Syntax error in argument \"%s\"", ovs), call. = F)
        }
        # for floating point range, we keep the range
        if (with_dec)
          old_val <- ovs
        else
          # add range to vector of old values
          old_val <- c(old_val, seq(from, to))
      } else {
        # can new value be converted to numeric?
        ovn <- suppressWarnings(as.numeric(ovs))
        # if not, assignment is wrong
        if (is.na(ovn)) ovn <- ovs
        # add old recode values to final vector of values
        old_val <- c(old_val, ovn)
      }
    }

    # now we have all recode values and want
    # to replace old with new values...
    for (k in seq_len(length(old_val))) {
      # check for "else" token
      if (is.infinite(old_val[k])) {
        # else-token found. we first need to preserve NA, but only,
        # if these haven't been copied before
        if (!na_recoded) new_var[which(is.na(x))] <- x[which(is.na(x))]
        # find replace-indices. since "else"-token has to be
        # the last argument in the "recodes"-string, the remaining,
        # non-recoded values are still "-Inf". Hence, find positions
        # of all not yet recoded values
        rep.pos <- which(new_var == -Inf)
        # else token found, now check whether we have a "copy"
        # token as well. in this case, new_val would be NULL
        if (is.null(new_val)) {
          # all not yet recodes values in new_var should get
          # the values at that position of "x" (the old variable),
          # i.e. these values remain unchanged.
          new_var[rep.pos] <- x[rep.pos]
        } else {
          # find all -Inf in new var and replace them with replace value
          new_var[rep.pos] <- new_val
        }
        # check for "NA" token
      } else if (is.na(old_val[k])) {
        # replace all NA with new value
        new_var[which(is.na(x))] <- new_val
        # remember that we have recoded NA's. Might be
        # important for else-token above.
        na_recoded <- TRUE
      } else if (is.character(old_val[k]) && with_dec) {
        # this value indicates a range of values to be recoded, because
        # we have found a colon. now copy from and to values from range
        from <- suppressWarnings(as.numeric(unlist(strsplit(old_val[k], ":", fixed = T))[1]))
        to <- suppressWarnings(as.numeric(unlist(strsplit(old_val[k], ":", fixed = T))[2]))
        # if old_val is a character, we assume we have a double with decimal
        # points and want to recode a range
        new_var[which(x >= from & x <= to)] <- new_val

      } else {
        # else we have numeric values, which should be replaced
        new_var[which(x == old_val[k])] <- new_val
      }
    }
  }

  # replace remaining -Inf with NA
  if (any(is.infinite(new_var))) new_var[which(new_var == -Inf)] <- NA

  # add back NA labels
  if (!is.null(current.na) && length(current.na) > 0) {
    # add named missings
    val_lab <- c(val_lab, current.na)
  }

  # set back variable and value labels
  new_var <- suppressWarnings(sjlabelled::set_label(x = new_var, label = var_lab))
  new_var <- suppressWarnings(sjlabelled::set_labels(x = new_var, labels = val_lab))

  # return result as factor?
  if (!as.num) new_var <- to_factor(new_var)

  new_var
}
