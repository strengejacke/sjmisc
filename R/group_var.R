#' @title Recode numeric variables into equal-ranged groups
#' @name group_var
#'
#' @description Recode numeric variables into equal ranged, grouped factors,
#'   i.e. a variable is cut into a smaller number of groups, where each group
#'   has the same value range. \code{group_labels()} creates the related value
#'   labels. \code{group_var_if()} and \code{group_labels_if()} are scoped
#'   variants of \code{group_var()} and \code{group_labels()}, where grouping
#'   will be applied only to those variables that match the logical condition
#'   of \code{predicate}.
#'
#' @seealso \code{\link{split_var}} to split variables into equal sized groups,
#'   \code{\link{group_str}} for grouping string vectors or
#'   \code{\link{rec_pattern}} and \code{\link{rec}} for another convenient
#'   way of recoding variables into smaller groups.
#'
#' @param size Numeric; group-size, i.e. the range for grouping. By default,
#'   for each 5 categories of \code{x} a new group is defined, i.e. \code{size = 5}.
#'   Use \code{size = "auto"} to automatically resize a variable into a maximum
#'   of 30 groups (which is the ggplot-default grouping when plotting
#'   histograms). Use \code{n} to determine the amount of groups.
#' @param right.interval Logical; if \code{TRUE}, grouping starts with the lower
#'   bound of \code{size}. See 'Details'.
#' @param n Sets the maximum number of groups that are defined when auto-grouping is on
#'   (\code{size = "auto"}). Default is 30. If \code{size} is not set to \code{"auto"},
#'   this argument will be ignored.
#'
#' @inheritParams to_dummy
#' @inheritParams rec
#'
#' @return
#'   \itemize{
#'     \item For \code{group_var()}, a grouped variable, either as numeric or as factor (see paramter \code{as.num}). If \code{x} is a data frame, only the grouped variables will be returned.
#'     \item For \code{group_labels()}, a string vector or a list of string vectors containing labels based on the grouped categories of \code{x}, formatted as "from lower bound to upper bound", e.g. \code{"10-19"  "20-29"  "30-39"} etc. See 'Examples'.
#'   }
#'
#' @note Variable label attributes (see, for instance,
#'   \code{\link[sjlabelled]{set_label}}) are preserved. Usually you should use
#'   the same values for \code{size} and \code{right.interval} in
#'   \code{group_labels()} as used in the \code{group_var} function if you want
#'   matching labels for the related recoded variable.
#'
#' @details If \code{size} is set to a specific value, the variable is recoded
#'   into several groups, where each group has a maximum range of \code{size}.
#'   Hence, the amount of groups differ depending on the range of \code{x}.
#'   \cr \cr
#'   If \code{size = "auto"}, the variable is recoded into a maximum of
#'   \code{n} groups. Hence, independent from the range of
#'   \code{x}, always the same amount of groups are created, so the range
#'   within each group differs (depending on \code{x}'s range).
#'   \cr \cr
#'   \code{right.interval} determins which boundary values to include when
#'   grouping is done. If \code{TRUE}, grouping starts with the \strong{lower
#'   bound} of \code{size}. For example, having a variable ranging from
#'   50 to 80, groups cover the ranges from  50-54, 55-59, 60-64 etc.
#'   If \code{FALSE} (default), grouping starts with the \code{upper bound}
#'   of \code{size}. In this case, groups cover the ranges from
#'   46-50, 51-55, 56-60, 61-65 etc. \strong{Note:} This will cover
#'   a range from 46-50 as first group, even if values from 46 to 49
#'   are not present. See 'Examples'.
#'   \cr \cr
#'   If you want to split a variable into a certain amount of equal
#'   sized groups (instead of having groups where values have all the same
#'   range), use the \code{\link{split_var}} function!
#'   \cr \cr
#'   \code{group_var()} also works on grouped data frames (see \code{\link[dplyr]{group_by}}).
#'   In this case, grouping is applied to the subsets of variables
#'   in \code{x}. See 'Examples'.
#'
#'
#' @examples
#' age <- abs(round(rnorm(100, 65, 20)))
#' age.grp <- group_var(age, size = 10)
#' hist(age)
#' hist(age.grp)
#'
#' age.grpvar <- group_labels(age, size = 10)
#' table(age.grp)
#' print(age.grpvar)
#'
#' # histogram with EUROFAMCARE sample dataset
#' # variable not grouped
#' library(sjlabelled)
#' data(efc)
#' hist(efc$e17age, main = get_label(efc$e17age))
#'
#' # bar plot with EUROFAMCARE sample dataset
#' # grouped variable
#' ageGrp <- group_var(efc$e17age)
#' ageGrpLab <- group_labels(efc$e17age)
#' barplot(table(ageGrp), main = get_label(efc$e17age), names.arg = ageGrpLab)
#'
#' # within a pipe-chain
#' library(dplyr)
#' efc %>%
#'   select(e17age, c12hour, c160age) %>%
#'   group_var(size = 20)
#'
#' # create vector with values from 50 to 80
#' dummy <- round(runif(200, 50, 80))
#' # labels with grouping starting at lower bound
#' group_labels(dummy)
#' # labels with grouping startint at upper bound
#' group_labels(dummy, right.interval = TRUE)
#'
#' # works also with gouped data frames
#' mtcars %>%
#'   group_var(disp, size = 4, append = FALSE) %>%
#'   table()
#'
#' mtcars %>%
#'   group_by(cyl) %>%
#'   group_var(disp, size = 4, append = FALSE) %>%
#'   table()
#' @export
group_var <- function(x, ..., size = 5, as.num = TRUE, right.interval = FALSE, n = 30, append = TRUE, suffix = "_gr") {

  # evaluate arguments, generate data
  .dat <- get_dot_data(x, dplyr::quos(...))

  recode_fun(
    x = x,
    .dat = .dat,
    fun = get("g_v_helper", asNamespace("sjmisc")),
    suffix = suffix,
    append = append,
    groupsize = size,
    as.num = as.num,
    right.interval = right.interval,
    groupcount = n
  )
}


#' @rdname group_var
#' @export
group_var_if <- function(x, predicate, size = 5, as.num = TRUE, right.interval = FALSE, n = 30, append = TRUE, suffix = "_gr") {

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


  recode_fun(
    x = x,
    .dat = .dat,
    fun = get("g_v_helper", asNamespace("sjmisc")),
    suffix = suffix,
    append = append,
    groupsize = size,
    as.num = as.num,
    right.interval = right.interval,
    groupcount = n
  )
}


g_v_helper <- function(x, groupsize, as.num, right.interval, groupcount) {
  # do we have labels?
  varlab <- sjlabelled::get_label(x)

  # group variable
  x <- group_helper(x, groupsize, right.interval, groupcount)

  # set new levels of grouped variable
  levels(x) <- seq_len(nlevels(x))

  # convert to numeric?
  if (as.num) x <- as.numeric(as.character(x))

  # set back variable labels
  if (!is.null(varlab)) x <- sjlabelled::set_label(x, label = varlab)

  x
}


#' @rdname group_var
#' @export
group_labels <- function(x, ..., size = 5, right.interval = FALSE, n = 30) {
  # evaluate arguments, generate data
  .dat <- get_dot_data(x, dplyr::quos(...))

  gl_fun(
    x = x,
    .dat = .dat,
    size = size,
    right.interval = right.interval,
    n = n
  )
}


#' @rdname group_var
#' @export
group_labels_if <- function(x, predicate, size = 5, right.interval = FALSE, n = 30) {

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


  gl_fun(
    x = x,
    .dat = .dat,
    size = size,
    right.interval = right.interval,
    n = n
  )
}


gl_fun <- function(x, .dat, size, right.interval, n) {
  if (is.data.frame(x)) {
    # iterate variables of data frame
    return(
      purrr::map(.dat, ~ g_l_helper(
        x = .x,
        groupsize = size,
        right.interval = right.interval,
        groupcount = n
      )))

  } else {
    x <- g_l_helper(
      x = .dat,
      groupsize = size,
      right.interval = right.interval,
      groupcount = n
    )
  }

  x
}


g_l_helper <- function(x, groupsize, right.interval, groupcount) {
  # do we have labels?
  varlab <- sjlabelled::get_label(x)

  # group variable
  x <- group_helper(x, groupsize, right.interval, groupcount)

  # Gruppen holen
  lvl <- levels(x)

  # rückgabewert init
  retval <- rep(c(""), length(lvl))

  # alle Gruppierungen durchgehen
  for (i in seq_len(length(lvl))) {
    # Länge jedes Labels der Gruppeneinteilungen auslesen
    sublength <- nchar(lvl[i])
    # "(" und "]", das bei "cut"-Funktion automatisch erstellt wird,
    # aus dem Label entfernen
    lvlstr <- substr(lvl[i], 2, sublength - 1)
    # Unter- und Obergrenze in jeweils einem string
    subs <- strsplit(lvlstr, ",")
    # Untergrenze als Zahlenwert
    lower <- as.numeric(subs[[1]][1])
    # Obergrenze als Zahlenwert
    upper <- as.numeric(subs[[1]][2])

    # Prüfen, welche Intervallgrenze ein-
    # und welche ausgeschlossen werden soll
    if (right.interval) {
      lower <- lower + 1
    } else {
      upper <- upper - 1
    }
    # Rückgabe des Strings
    retval[i] <- c(paste(lower, "-", upper, sep = ""))
  }

  # set back variable labels
  if (!is.null(varlab)) retval <- sjlabelled::set_label(retval, label = varlab)

  retval
}


group_helper <- function(x, groupsize, right.interval, groupcount) {
  # check if factor. factors need conversion
  # to numeric before grouped
  if (is.factor(x)) x <- sjlabelled::as_numeric(x, keep.labels = FALSE)

  # minimum range. will be changed when autogrouping
  minval <- 0
  multip <- 2
  # check for auto-grouping
  if (groupsize == "auto") {
    # determine groupsize, which is 1/30 of range
    size <- ceiling((max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) / groupcount)
    # reset groupsize
    groupsize <- as.numeric(size)
    # change minvalue
    minval <- min(x, na.rm = TRUE)
    multip <- 1
  }
  # Einteilung der Variablen in Gruppen. Dabei werden unbenutzte
  # Faktoren gleich entfernt
  x <-
    droplevels(cut(x, breaks = c(
      seq(minval, max(x, na.rm = TRUE) + multip * groupsize, by = groupsize)
    ), right = right.interval))

  x
}
