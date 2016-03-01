#' @title Recode numeric variables into equal-ranged groups
#' @name group_var
#'
#' @description Recode numeric variables into \emph{equal spaced} grouped factors,
#'                i.e. a variable is cut into a smaller number of groups,
#'                where each group has values of equal range.
#'
#' @seealso \code{\link{group_labels}} to create the associated value labels for
#'          grouped variables, \code{\link{split_var}} to split variables into
#'          equal sized groups, \code{\link{group_str}} for grouping string vectors
#'          or \code{\link{rec_pattern}} and \code{\link{rec}} for another
#'          convenbient way of recoding variables into smaller groups.
#'
#' @param var Numeric; variable, which should recoded into groups.
#' @param groupsize Numeric; group-size, i.e. the range for grouping. By default,
#'          for each 5 categories of \code{var} a new group is defined, i.e. \code{groupsize=5}.
#'          Use \code{groupsize = "auto"} to automatically resize a variable into
#'          a maximum of 30 groups (which is the ggplot-default grouping when
#'          plotting histograms). Use \code{groupcount} to determine the amount
#'          of groups.
#' @param as.num Logical; if \code{TRUE}, the recoded variable will
#'          be returned as numeric vector. If \code{FALSE}, a factor is returned.
#' @param right.interval Logical; if \code{TRUE}, grouping starts with the lower
#'          bound of \code{groupsize}. See 'Details'.
#' @param groupcount Sets the maximum number of groups that are defined when auto-grouping is on
#'          (\code{groupsize="auto"}). Default is 30. If \code{groupsize} is not set to \code{"auto"},
#'          this argument will be ignored.
#'
#' @return A grouped variable, either as numeric or as factor (see paramter \code{as.num}).
#'
#' @note Variable label attributes (see, for instance, \code{\link{set_label}}) are preserved.
#'
#' @details If \code{groupsize} is set to a specific value, the variable is recoded
#'            into several groups, where each group has a maximum range of \code{groupsize}.
#'            Hence, the amount of groups differ depending on the range of \code{var}.
#'            \cr \cr
#'            If \code{groupsize = "auto"}, the variable is recoded into a maximum of
#'            \code{groupcount} groups. Hence, independent from the range of
#'            \code{var}, always the same amount of groups are created, so the range
#'            within each group differs (depending on \code{var}'s range).
#'            \cr \cr
#'            \code{right.interval} determins which boundary values to include when
#'            grouping is done. If \code{TRUE}, grouping starts with the \strong{lower
#'            bound} of \code{groupsize}. For example, having a variable ranging from
#'            50 to 80, groups cover the ranges from  50-54, 55-59, 60-64 etc.
#'            If \code{FALSE} (default), grouping starts with the \code{upper bound}
#'            of \code{groupsize}. In this case, groups cover the ranges from
#'            46-50, 51-55, 56-60, 61-65 etc. \strong{Note:} This will cover
#'            a range from 46-50 as first group, even if values from 46 to 49
#'            are not present. See 'Examples' in \code{\link{group_labels}}.
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
#' barplot(table(ageGrp),
#'         main = get_label(efc$e17age),
#'         names.arg = ageGrpLab)
#'
#' @export
group_var <- function(var,
                      groupsize = 5,
                      as.num = TRUE,
                      right.interval = FALSE,
                      groupcount = 30) {
  # do we have labels?
  varlab <- get_label(var)
  # group variable
  var <- group_helper(var, groupsize, right.interval, groupcount)
  # set new levels of grouped variable
  levels(var) <- c(1:length(levels(var)))
  # convert to numeric?
  if (as.num) var <- as.numeric(as.character(var))
  # set back variable labels
  if (!is.null(varlab)) var <- set_label(var, varlab)
  return(var)
}


#' @title Create labels for recoded groups
#' @name group_labels
#'
#' @description Creates the related labels for the grouped variable created by
#'                \code{\link{group_var}}.
#'
#' @seealso \itemize{
#'            \item \code{\link{group_var}}
#'            \item \code{\link{group_str}}
#'          }
#'
#' @note Usually you should use the same values for \code{groupsize} and
#'         \code{right.interval} as used in the \code{\link{group_var}} function
#'         if you want to create labels for the related recoded variable.
#'
#' @return A string vector containing labels based on the grouped categories of \code{var},
#'           formatted as "from lower bound to upper bound", e.g. \code{"10-19"  "20-29"  "30-39"} etc.
#'           See examples below.
#'
#' @inheritParams group_var
#'
#' @details See 'Details' in \code{\link{group_var}}.
#'
#' @note Variable label attributes (see, for instance, \code{\link{set_label}}) are preserved.
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
#'
#' # create vector with values from 50 to 80
#' dummy <- round(runif(200, 50, 80))
#' # labels with grouping starting at lower bound
#' group_labels(dummy)
#' # labels with grouping startint at upper bound
#' group_labels(dummy, right.interval = TRUE)
#'
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
#' barplot(table(ageGrp),
#'         main = get_label(efc$e17age),
#'         names.arg = ageGrpLab)
#'
#' @export
group_labels <- function(var,
                         groupsize = 5,
                         right.interval = FALSE,
                         groupcount = 30) {
  # do we have labels?
  varlab <- get_label(var)
  # group variable
  var <- group_helper(var, groupsize, right.interval, groupcount)
  # Gruppen holen
  lvl <- levels(var)
  # rückgabewert init
  retval <- rep(c(""), length(lvl))
  # alle Gruppierungen durchgehen
  for (i in 1:length(lvl)) {
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


group_helper <- function(var, groupsize, right.interval, groupcount) {
  # minimum range. will be changed when autogrouping
  minval <- 0
  multip <- 2
  # check for auto-grouping
  if (groupsize == "auto") {
    # determine groupsize, which is 1/30 of range
    size <- ceiling((max(var, na.rm = TRUE) - min(var, na.rm = TRUE)) / groupcount)
    # reset groupsize var
    groupsize <- as.numeric(size)
    # change minvalue
    minval <- min(var, na.rm = TRUE)
    multip <- 1
  }
  # Einteilung der Variablen in Gruppen. Dabei werden unbenutzte
  # Faktoren gleich entfernt
  var <- droplevels(cut(var,
                        breaks = c(seq(minval,
                                       max(var, na.rm = TRUE) + multip * groupsize,
                                       by = groupsize)),
                        right = right.interval))
  return(var)
}
