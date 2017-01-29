#' @title Recode numeric variables into equal-ranged groups
#' @name group_var
#'
#' @description Recode numeric variables into equal ranged, grouped factors,
#'                i.e. a variable is cut into a smaller number of groups,
#'                where each group has the same value range, and create the
#'                related value labels.
#'
#' @seealso \code{\link{split_var}} to split variables into
#'          equal sized groups, \code{\link{group_str}} for grouping string vectors
#'          or \code{\link{rec_pattern}} and \code{\link{rec}} for another
#'          convenbient way of recoding variables into smaller groups.
#'
#' @param groupsize Numeric; group-size, i.e. the range for grouping. By default,
#'          for each 5 categories of \code{x} a new group is defined, i.e. \code{groupsize=5}.
#'          Use \code{groupsize = "auto"} to automatically resize a variable into
#'          a maximum of 30 groups (which is the ggplot-default grouping when
#'          plotting histograms). Use \code{groupcount} to determine the amount
#'          of groups.
#' @param right.interval Logical; if \code{TRUE}, grouping starts with the lower
#'          bound of \code{groupsize}. See 'Details'.
#' @param groupcount Sets the maximum number of groups that are defined when auto-grouping is on
#'          (\code{groupsize="auto"}). Default is 30. If \code{groupsize} is not set to \code{"auto"},
#'          this argument will be ignored.
#'
#' @inheritParams to_factor
#' @inheritParams rec
#'
#' @return \itemize{
#'           \item For \code{group_var}, a grouped variable, either as numeric or as factor (see paramter \code{as.num}). If \code{x} is a data frame, only the grouped variables will be returned.
#'           \item For \code{group_label}, a string vector or a list of string vectors containing labels based on the grouped categories of \code{x}, formatted as "from lower bound to upper bound", e.g. \code{"10-19"  "20-29"  "30-39"} etc. See 'Examples'.
#'         }
#'
#' @note Variable label attributes (see, for instance, \code{\link{set_label}}) are preserved.
#'       Usually you should use the same values for \code{groupsize} and
#'       \code{right.interval} in \code{group_label()} as used in the
#'       \code{group_var} function if you want matching labels for the related
#'       recoded variable.
#'
#' @details If \code{groupsize} is set to a specific value, the variable is recoded
#'            into several groups, where each group has a maximum range of \code{groupsize}.
#'            Hence, the amount of groups differ depending on the range of \code{x}.
#'            \cr \cr
#'            If \code{groupsize = "auto"}, the variable is recoded into a maximum of
#'            \code{groupcount} groups. Hence, independent from the range of
#'            \code{x}, always the same amount of groups are created, so the range
#'            within each group differs (depending on \code{x}'s range).
#'            \cr \cr
#'            \code{right.interval} determins which boundary values to include when
#'            grouping is done. If \code{TRUE}, grouping starts with the \strong{lower
#'            bound} of \code{groupsize}. For example, having a variable ranging from
#'            50 to 80, groups cover the ranges from  50-54, 55-59, 60-64 etc.
#'            If \code{FALSE} (default), grouping starts with the \code{upper bound}
#'            of \code{groupsize}. In this case, groups cover the ranges from
#'            46-50, 51-55, 56-60, 61-65 etc. \strong{Note:} This will cover
#'            a range from 46-50 as first group, even if values from 46 to 49
#'            are not present. See 'Examples'.
#'            \cr \cr
#'            If you want to split a variable into a certain amount of equal
#'            sized groups (instead of having groups where values have all the same
#'            range), use the \code{\link{split_var}} function!
#'
#' @examples
#' age <- abs(round(rnorm(100, 65, 20)))
#' age.grp <- group_var(age, 10)
#' hist(age)
#' hist(age.grp)
#'
#' age.grpvar <- group_labels(age, 10)
#' table(age.grp)
#' print(age.grpvar)
#'
#' # histogram with EUROFAMCARE sample dataset
#' # variable not grouped
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
#' efc %>% select(e17age, c12hour, c160age) %>% group_var(groupsize = 20)
#'
#' # create vector with values from 50 to 80
#' dummy <- round(runif(200, 50, 80))
#' # labels with grouping starting at lower bound
#' group_labels(dummy)
#' # labels with grouping startint at upper bound
#' group_labels(dummy, right.interval = TRUE)
#'
#' @importFrom purrr map
#' @export
group_var <- function(x, ..., groupsize = 5, as.num = TRUE, right.interval = FALSE,
                      groupcount = 30, suffix = "_gr") {
  # evaluate arguments, generate data
  .dots <- match.call(expand.dots = FALSE)$`...`
  .dat <- get_dot_data(x, .dots)

  # get variable names
  .vars <- dot_names(.dots)

  # if user only provided a data frame, get all variable names
  if (is.null(.vars) && is.data.frame(x)) .vars <- colnames(x)

  # if we have any dot names, we definitely have a data frame
  if (!is.null(.vars)) {

    for (i in .vars) {
      x[[i]] <- g_v_helper(
        x = .dat[[i]],
        groupsize = groupsize,
        as.num = as.num,
        right.interval = right.interval,
        groupcount = groupcount
      )
    }

    # coerce to tibble and select only recoded variables
    x <- tibble::as_tibble(x[.vars])

    # add suffix to recoded variables?
    if (!is.null(suffix) && !sjmisc::is_empty(suffix)) {
      colnames(x) <- sprintf("%s%s", colnames(x), suffix)
    }
  } else {
    x <- g_v_helper(
      x = .dat,
      groupsize = groupsize,
      as.num = as.num,
      right.interval = right.interval,
      groupcount = groupcount
    )
  }

  x
}


g_v_helper <- function(x, groupsize, as.num, right.interval, groupcount) {
  # do we have labels?
  varlab <- get_label(x)
  # group variable
  x <- group_helper(x, groupsize, right.interval, groupcount)
  # set new levels of grouped variable
  levels(x) <- seq_len(nlevels(x))
  # convert to numeric?
  if (as.num) x <- as.numeric(as.character(x))
  # set back variable labels
  if (!is.null(varlab)) x <- set_label(x, varlab)
  return(x)
}


#' @rdname group_var
#' @export
group_labels <- function(x, ..., groupsize = 5, right.interval = FALSE, groupcount = 30) {
  # evaluate arguments, generate data
  .dots <- match.call(expand.dots = FALSE)$`...`
  .dat <- get_dot_data(x, .dots)

  # get variable names
  .vars <- dot_names(.dots)

  # if user only provided a data frame, get all variable names
  if (is.null(.vars) && is.data.frame(x)) .vars <- colnames(x)

  # if we have any dot names, we definitely have a data frame
  if (!is.null(.vars)) {

    # iterate variables of data frame
    return(
      purrr::map(.dat, ~ g_l_helper(
        x = .x,
        groupsize = groupsize,
        right.interval = right.interval,
        groupcount = groupcount
    )))

  } else {
    x <- g_l_helper(
      x = .dat,
      groupsize = groupsize,
      right.interval = right.interval,
      groupcount = groupcount
    )
  }

  x
}


g_l_helper <- function(x, groupsize, right.interval, groupcount) {
  # do we have labels?
  varlab <- get_label(x)
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
  if (!is.null(varlab)) retval <- set_label(retval, varlab)
  return(retval)
}


group_helper <- function(x, groupsize, right.interval, groupcount) {
  # check if factor. factors need conversion
  # to numeric before grouped
  if (is.factor(x)) x <- to_value(x, keep.labels = FALSE)

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
  x <- droplevels(cut(x, breaks = c(seq(minval, max(x, na.rm = TRUE) + multip * groupsize, by = groupsize)),
                      right = right.interval))
  return(x)
}
