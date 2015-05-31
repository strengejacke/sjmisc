#' @title Dichotomize variables
#' @name dicho
#'
#' @description Dichotomizes variables into dummy variables (0/1). Dichotomization is
#'                either done by median, mean or a specific value (see \code{dichBy}).
#'                Either single vectors, a complete data frame or a \code{list} of
#'                variables can be dichotomized.
#'
#' @param x variable (vector), \code{data.frame} or \code{list} of variables
#'          that should be dichotomized
#' @param dichBy indicates the split criterion where a variable is dichotomized
#'          \describe{
#'            \item{\code{"median"}}{by default, \code{var} is split into two groups at the median (\code{dichBy = "median"} or \code{dichBy = "md"})}
#'            \item{\code{dichBy = "mean"}}{(or \code{dichBy = "m"}) splits \code{var} into two groups at the mean of \code{var}}
#'            \item{\code{dichBy = "value"}}{(or \code{dichBy = "v"}) splits \code{var} into two groups at a specific value (see \code{dichVal})}
#'            }
#' @param dichVal numeric, indicates a value where \code{var} is dichotomized when \code{dichBy = "value"}.
#'          \strong{Note that \code{dichVal} is inclusive}, i.e. \code{dichVal = 10} will split \code{var}
#'          into one group with values from lowest to 10 and another group with values greater
#'          than 10.
#' @param asNum logical, if \code{TRUE}, return value will be numeric, not a factor.
#' @return a dichotomized factor (or numeric, if \code{asNum = TRUE}) variable (0/1-coded),
#'           respectively a data frame or list of dichotomized factor (or numeric) variables.
#'
#' @examples
#' data(efc)
#' summary(efc$c12hour)
#' table(dicho(efc$c12hour))
#' table(dicho(efc$c12hour, "mean"))
#' table(dicho(efc$c12hour, "value", 30))
#'
#' # sample data frame, values from 1-4
#' head(efc[, 6:10])
#' # dichtomized values (1 to 2 = 0, 3 to 4 = 1)
#' head(dicho(efc[, 6:10], "v", 2))
#'
#' # dichtomize several variables in a list
#' dummy <- list(efc$c12hour, efc$e17age, efc$c160age)
#' dicho(dummy)
#'
#' @export
dicho <- function(x, dichBy = "median", dichVal = -1, asNum = FALSE) {
  # check abbreviations
  if (dichBy == "md") dichBy <- "median"
  if (dichBy == "m") dichBy <- "mean"
  if (dichBy == "v") dichBy <- "value"
  # check for correct dichotome types
  if (dichBy != "median" && dichBy != "mean" && dichBy != "value") {
    stop("Parameter \"dichBy\" must either be \"median\", \"mean\" or \"value\"..." , call. = FALSE)
  }
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # dichotomize all
    for (i in 1:nvars) x[[i]] <- dicho_helper(x[[i]], dichBy, dichVal, asNum)
    return(x)
  } else {
    return(dicho_helper(x, dichBy, dichVal, asNum))
  }
}


dicho_helper <- function(var, dichBy, dichVal, asNum) {
  # check if factor
  if (is.factor(var)) {
    # non-numeric-factor cannot be converted
    if (is_num_fac(var)) {
      # try to convert to numeric
      var <- as.numeric(as.character(var))
    } else {
      # convert non-numeric factor to numeric
      # factor levels are replaced by numeric values
      var <- to_value(var, keep.labels = FALSE)
      message("Trying to dichotomize non-numeric factor.")
    }
  }
  # split at median
  if (dichBy == "median") {
    var <- ifelse(var <= median(var, na.rm = T), 0, 1)
    # split at mean
  } else if (dichBy == "mean") {
    var <- ifelse(var <= mean(var, na.rm = T), 0, 1)
    # split at specific value
  } else {
    var <- ifelse(var <= dichVal, 0, 1)
  }
  if (!asNum) var <- as.factor(var)
  return(var)
}


#' @title Recode count variables into grouped factors
#' @name group_var
#'
#' @description Recode count variables into grouped factors, i.e. a variable is
#'                cut into a smaller number of groups.
#'
#' @seealso \itemize{
#'            \item \code{\link{group_labels}}
#'            \item \code{\link{group_str}}
#'          }
#'
#' @param var numeric; variable, which should recoded into groups.
#' @param groupsize numeric; group-size, i.e. the range for grouping. By default,
#'          for each 5 categories of \code{var} a new group is defined, i.e. \code{groupsize=5}.
#'          Use \code{groupsize = "auto"} to automatically resize a variable into
#'          a maximum of 30 groups (which is the ggplot-default grouping when
#'          plotting histograms). Use \code{autoGroupCount} to determin the amount
#'          of groups.
#' @param asNumeric logical; if \code{TRUE} (default), the recoded variable will
#'          be returned as numeric vector. If \code{FALSE}, a factor is returned.
#' @param rightInterval logical; if \code{TRUE}, grouping starts with the lower
#'          bound of \code{groupsize}. See 'Details'.
#' @param autoGroupCount Sets the maximum number of groups that are defined when auto-grouping is on
#'          (\code{groupsize="auto"}). Default is 30. If \code{groupsize} is not set to \code{"auto"},
#'          this parameter will be ignored.
#'
#' @return A grouped variable, either as numeric or as factor (see paramter \code{asNumeric}).
#'
#' @details If \code{groupsize} is set to a specific value, the variable is recoded
#'            into several groups, where each group has a maximum range of \code{groupsize}.
#'            Hence, the amount of groups differ depending on the range of \code{var}.
#'            \cr \cr
#'            If \code{groupsize = "auto"}, the variable is recoded into a maximum of
#'            \code{autoGroupCount} groups. Hence, independent from the range of
#'            \code{var}, always the same amount of groups are created, so the range
#'            within each group differs (depending on \code{var}'s range).
#'            \cr \cr
#'            \code{rightInterval} determins which boundary values to include when
#'            grouping is done. If \code{TRUE}, grouping starts with the \strong{lower
#'            bound} of \code{groupsize}. For example, having a variable ranging from
#'            50 to 80, groups cover the ranges from  50-54, 55-59, 60-64 etc.
#'            If \code{FALSE} (default), grouping starts with the \code{upper bound}
#'            of \code{groupsize}. In this case, groups cover the ranges from
#'            46-50, 51-55, 56-60, 61-65 etc. \strong{Note:} This will cover
#'            a range from 46-50 as first group, even if values from 46 to 49
#'            are not present. See 'Examples' in \code{\link{group_labels}}.
#'
#' @examples
#' age <- abs(round(rnorm(100, 65, 20)))
#' age.grp <- group_var(age, 10)
#' hist(age)
#' hist(age.grp)
#'
#' # histogram with EUROFAMCARE sample dataset
#' # variable not grouped
#' data(efc)
#' \dontrun{
#' library(sjPlot)
#' sjp.frq(efc$e17age,
#'         title = get_var_labels(efc$e17age),
#'         type = "h",
#'         showValueLabels = FALSE)}
#'
#' # bar plot with EUROFAMCARE sample dataset
#' # grouped variable
#' data(efc)
#' ageGrp <- group_var(efc$e17age)
#' ageGrpLab <- group_labels(efc$e17age)
#' \dontrun{
#' library(sjPlot)
#' sjp.frq(ageGrp,
#'         title = get_var_labels(efc$e17age),
#'         axisLabels.x = ageGrpLab)}
#'
#' @export
group_var <- function(var,
                      groupsize = 5,
                      asNumeric = TRUE,
                      rightInterval = FALSE,
                      autoGroupCount = 30) {
  # group variable
  var <- group_helper(var, groupsize, rightInterval, autoGroupCount)
  # set new levels of grouped variable
  levels(var) <- c(1:length(levels(var)))
  # convert to numeric?
  if (asNumeric) var <- as.numeric(as.character(var))
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
#'         \code{rightInterval} as used in the \code{\link{group_var}} function
#'         if you want to create labels for the related recoded variable.
#'
#' @param var numeric variable, which should recoded into groups.
#' @param groupsize group-size, i.e. the range for grouping. By default, for each 5 categories
#'          new group is built, i.e. \code{groupsize = 5}. Use \code{groupsize = "auto"} to automatically
#'          resize a variable into a maximum of 30 groups (which is the ggplot-default grouping when
#'          plotting histograms). Use parameter \code{autoGroupCount} to define the amount of groups.
#' @param rightInterval logical; if \code{TRUE}, grouping starts with the lower bound of \code{groupsize}.
#'          If \code{FALSE} (default), grouping starts with the upper bound of \code{groupsize}. See 'Examples'
#'          and 'Details'.
#' @param autoGroupCount Sets the maximum number of groups that are built when auto-grouping is on
#'          (\code{groupsize = "auto"}). Default is 30. If \code{groupsize} is not set to \code{"auto"},
#'          this parameter will be ignored.
#'
#' @return A string vector containing labels based on the grouped categories of \code{var},
#'           formatted as "from lower bound to upper bound", e.g. \code{"10-19"  "20-29"  "30-39"} etc.
#'           See examples below.
#'
#' @details See 'Details' in \code{\link{group_var}}.
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
#' # create vector with values from 50 to 80
#' dummy <- round(runif(200, 50, 80))
#' # labels with grouping starting at lower bound
#' group_labels(dummy)
#' # labels with grouping startint at upper bound
#' group_labels(dummy, rightInterval = TRUE)
#'
#'
#' # histogram with EUROFAMCARE sample dataset
#' # variable not grouped
#' data(efc)
#' \dontrun{
#' library(sjPlot)
#' sjp.frq(efc$e17age,
#'         title = get_var_labels(efc$e17age),
#'         type = "h",
#'         showValueLabels = FALSE)}
#'
#' # bar plot with EUROFAMCARE sample dataset
#' # grouped variable
#' data(efc)
#' ageGrp <- group_var(efc$e17age)
#' ageGrpLab <- group_labels(efc$e17age)
#' \dontrun{
#' library(sjPlot)
#' sjp.frq(ageGrp,
#'         title = get_var_labels(efc$e17age),
#'         axisLabels.x = ageGrpLab)}
#'
#' @export
group_labels <- function(var,
                         groupsize = 5,
                         rightInterval = FALSE,
                         autoGroupCount = 30) {
  # group variable
  var <- group_helper(var, groupsize, rightInterval, autoGroupCount)
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
    if (rightInterval) {
      lower <- lower + 1
    } else {
      upper <- upper - 1
    }
    # Rückgabe des Strings
    retval[i] <- c(paste(lower, "-", upper, sep = ""))
  }
  return(retval)
}


group_helper <- function(var, groupsize, rightInterval, autoGroupCount) {
  # minimum range. will be changed when autogrouping
  minval <- 0
  multip <- 2
  # check for auto-grouping
  if (groupsize == "auto") {
    # determine groupsize, which is 1/30 of range
    size <- ceiling((max(var, na.rm = TRUE) - min(var, na.rm = TRUE)) / autoGroupCount)
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
                        right = rightInterval))
  return(var)
}


#' @title Insert line breaks in long labels
#' @name word_wrap
#'
#' @description Insert line breaks in long character strings. Useful if you want to wordwrap
#'                labels / titles for plots or tables.
#'
#' @param labels label(s) as character string, where a line break should be
#'          inserted. Several strings may be passed as vector
#'          (e.g. \code{labels = c("first long string", "second long string")})
#' @param wrap the maximum amount of chars per line (i.e. line length)
#' @param linesep by default, this parameter is \code{NULL} and a regular new line
#'          string (\code{"\\n"}) is used. For HTML-purposes, for instance, \code{linesep}
#'          could be \code{"<br>"}.
#' @return New label(s) with line breaks inserted at every \code{wrap}'s position.
#'
#' @examples
#' word_wrap(c("A very long string", "And another even longer string!"), 10)
#'
#' message(word_wrap("Much too long string for just one line!", 15))
#'
#' @export
word_wrap <- function(labels, wrap, linesep=NULL) {
  # check for valid value
  if (is.null(labels) || length(labels) == 0) return(NULL)
  # default line separator is \n
  if (is.null(linesep)) {
    linesep <- '\\1\n'
    lsub <- 0
    ori.linesep <- '\n'
  } else {
    # however, for html-function we can use "<br>"
    # as parameter
    lsub <- nchar(linesep) - 1
    ori.linesep <- linesep
    linesep <- sprintf("\\1%s", linesep)
  }
  # create regex pattern for line break
  pattern <- c(paste('(.{1,', wrap, '})(\\s|$)', sep = ""))
  # iterate all labels
  for (n in 1:length(labels)) {
    # check if wrap exceeds lengths of labels
    if (wrap > 0 && nchar(labels[n]) > wrap) {
      # insert line breaks
      labels[n] <- gsub(pattern, linesep, labels[n])
      # -----------------------
      # in case label was short enough, we still have a line break
      # at the end of the label. here we remove any trailing line breaks
      # -----------------------
      # get length of label
      l <- nchar(labels[n])
      # get last char
      lc <- substr(labels[n], l - lsub, l)
      # check if line break
      if (lc == ori.linesep) {
        # if yes, remove it
        labels[n] <- substr(labels[n], 0, l - (lsub + 1))
      }
    }
  }
  return(labels)
}


#' @title Recode variable categories into new values
#' @name recode_to
#'
#' @description Recodes (or "renumbers") the categories of \code{var} into new category values, beginning
#'                with the lowest value specified by parameter \code{lowest}. Useful if you want
#'                to recode dummy variables with 1/2 coding to 0/1 coding, or recoding scales from
#'                1-4 to 0-3 etc.
#'
#' @seealso \code{\link{rec}} for general recoding of variables and \code{\link{set_na}}
#'            for setting \code{\link{NA}} values.
#'
#' @param x variable (vector), data frame or \code{list} of variables that should be recoded.
#' @param lowest indicating the lowest category value for recoding. Default is 0, so the new
#'          variable starts with value 0.
#' @param highest if specified and larger than \code{lowest}, all category values larger than
#'          \code{highest} will be set to \code{NA}. Default is \code{-1}, i.e. this parameter is ignored
#'          and no NA's will be produced.
#' @return A new variable with recoded category values, where \code{lowest} indicates the lowest
#'           value; or a data frame or \code{list} of variables where variables have
#'           been recoded as described.
#'
#' @note Value and variable label attributes (see, for instance, \code{\link{get_val_labels}}
#'         or \code{\link{set_val_labels}}) are retained.
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
#' recode_to(dummy, 1)
#'
#' # lowest value starting with 1, highest with 3
#' # all others set to NA
#' dummy <- sample(11:15, 10, replace = TRUE)
#' recode_to(dummy, 1, 3)
#'
#' # create list of variables
#' data(efc)
#' dummy <- list(efc$c82cop1, efc$c83cop2, efc$c84cop3)
#' # check original distribution of categories
#' lapply(dummy, table)
#' # renumber from 1 to 0
#' lapply(recode_to(dummy), table)
#'
#' @export
recode_to <- function(x, lowest=0, highest=-1) {
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # dichotomize all
    for (i in 1:nvars) x[[i]] <- rec_to_helper(x[[i]], lowest, highest)
    return(x)
  } else {
    return(rec_to_helper(x, lowest, highest))
  }
}


rec_to_helper <- function(var, lowest, highest) {
  # retrieve value labels
  val_lab <- get_val_labels(var)
  # retrieve variable label
  var_lab <- get_var_labels(var)
  # check if factor
  if (is.factor(var)) {
    # try to convert to numeric
    var <- as.numeric(as.character(var))
  }
  # retrieve lowest category
  minval <- min(var, na.rm = TRUE)
  # check substraction difference between current lowest value
  # and requested lowest value
  downsize <- minval - lowest
  var <- sapply(var, function(x) x - downsize)
  # check for highest range
  # set NA to all values out of range
  if (highest > lowest) var[var > highest] <- NA
  # set back labels, if we have any
  if (!is.null(val_lab)) var <- suppressWarnings(set_val_labels(var, val_lab))
  if (!is.null(var_lab)) var <- suppressWarnings(set_var_labels(var, var_lab))
  # return recoded var
  return(var)
}


#' @title Recode numeric variables
#' @name rec
#'
#' @description Recodes the categories of a (numeric) variable \code{x} into new
#'                category values.
#'
#' @seealso \code{\link{set_na}} for setting \code{NA} values and \code{\link{recode_to}}
#'            for re-shifting value ranges.
#'
#' @param x a numeric variable (vector) or a \code{\link{factor}} with numeric
#'          levels that should be recoded; or a data frame or \code{list} of
#'          variables.
#' @param recodes a string with recode pairs of old and new values. See details for
#'          examples.
#' @return A numeric variable with recoded category values, or a data frame
#'           or \code{list}-object with recoded categories for all variables.
#'
#' @details  The \code{recodes} string has following syntax:
#'           \itemize{
#'            \item each recode pair has to be separated by a \code{;}, e.g. \code{recodes = "1=1; 2=4; 3=2; 4=3"}
#'            \item multiple old values that should be recoded into a new single value may be separated with comma, e.g. \code{"1,2=1; 3,4=2"}
#'            \item a value range is indicated by a colon, e.g. \code{"1:4=1; 5:8=2"} (recodes all values from 1 to 4 into 1, and from 5 to 8 into 2)
#'            \item minimum and maximum values are indicates by \emph{min} and \emph{max}, e.g. \code{"min:4=1; 5:max=2"} (recodes all values from minimum values of \code{x} to 4 into 1, and from 5 to maximum values of \code{x} into 2)
#'            \item all other values except specified are indicated by \emph{else}, e.g. \code{"3=1; 1=2; else=3"} (recodes 3 into 1, 1 into 2 and all other values into 3)
#'            \item the \code{"else"}-token can be combined with \emph{keep}, indicating that all remaining, not yet recoded values should stay the same, e.g. \code{"3=1; 1=2; else=keep"} (recodes 3 into 1, 1 into 2 and all other values like 2, 4 or 5 etc. will not be recoded, but copied, see 'Examples')
#'            \item \code{\link{NA}} values are allowed both as old and new value, e.g. \code{"NA=1; 3:5=NA"} (recodes all NA from old value into 1, and all old values from 3 to 5 into NA in the new variable)
#'            \item \code{"rev"} is a special token that reverses the value order (see 'Examples')
#'           }
#'
#' @note Please note following behaviours of the function:
#'       \itemize{
#'         \item the \code{"else"}-token should always be the last parameter in the \code{recodes}-string.
#'         \item Non-matching values will be set to \code{\link{NA}}.
#'         \item Variable label attributes (see, for instance, \code{\link{get_var_labels}}) are retained, however, value label attributes are removed.
#'         \item If \code{x} is a data frame or \code{list} of variables, all variables should have the same categories resp. value range (else, see first bullet, \code{NA}s are produced).
#'       }
#'
#' @examples
#' data(efc)
#' table(efc$e42dep, exclude = NULL)
#'
#' # replace NA with 5
#' table(rec(efc$e42dep, "1=1;2=2;3=3;4=4;NA=5"), exclude = NULL)
#'
#' # recode 1 to 2 into 1 and 3 to 4 into 2
#' table(rec(efc$e42dep, "1,2=1; 3,4=2"), exclude = NULL)
#'
#' # recode 1 to 3 into 4 into 2
#' table(rec(efc$e42dep, "min:3=1; 4=2"), exclude = NULL)
#'
#' # recode 2 to 1 and all others into 2
#' table(rec(efc$e42dep, "2=1; else=2"), exclude = NULL)
#'
#' # reverse value order
#' table(rec(efc$e42dep, "rev"), exclude = NULL)
#'
#' # recode only selected values, copy remaining
#' table(efc$e15relat)
#' table(rec(efc$e15relat, "1,2,4=1; else=keep"))
#'
#' # recode variables with same categorie in a data frame
#' head(efc[, 6:9])
#' head(rec(efc[, 6:9], "1=10;2=20;3=30;4=40"))
#'
#' # recode list of variables. create dummy-list of
#' # variables with same value-range
#' dummy <- list(efc$c82cop1, efc$c83cop2, efc$c84cop3)
#' # show original distribution
#' lapply(dummy, table, exclude = NULL)
#' # show recodes
#' lapply(rec(dummy, "1,2=1; NA=9; else=keep"), table, exclude = NULL)
#'
#' @export
rec <- function(x, recodes) {
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # dichotomize all
    for (i in 1:nvars) x[[i]] <- rec_helper(x[[i]], recodes)
    return(x)
  } else {
    return(rec_helper(x, recodes))
  }
}


rec_helper <- function(x, recodes) {
  # retrieve variable label
  var_lab <- get_var_labels(x)
  val_lab <- NULL
  # remember if NA's have been recoded...
  na_recoded <- FALSE
  # -------------------------------
  # do we have a factor with "x"?
  # -------------------------------
  if (is.factor(x)) {
    # factor may only have numeric levels!
    if (!is_num_fac(x)) {
      stop("'x' may only have numeric factor levels!", call. = F)
    } else {
      x <- as.numeric(as.character(x))
    }
  }
  # -------------------------------
  # retrieve min and max values
  # -------------------------------
  min_val <- min(x, na.rm = T)
  max_val <- max(x, na.rm = T)
  # -------------------------------
  # do we have special recode-token?
  # -------------------------------
  if (recodes == "rev") {
    # retrieve unique valus, sorted
    ov <- sort(unique(na.omit(x)))
    # new values should be reversed order
    nv <- rev(ov)
    # create recodes-string
    recodes <- paste(sprintf("%i=%i", ov, nv), collapse = ";")
    # when we simply reverse values, we can keep value labels
    val_lab <- get_val_labels(x)
  }
  # -------------------------------
  # prepare and clean recode string
  # -------------------------------
  # retrieve each single recode command
  rec_string <- unlist(strsplit(recodes, ";", fixed = TRUE))
  # remove spaces
  rec_string <- gsub(" ", "", rec_string, fixed = TRUE)
  # replace min and max placeholders
  rec_string <- gsub("min", as.character(min_val), rec_string, fixed = TRUE)
  rec_string <- gsub("max", as.character(max_val), rec_string, fixed = TRUE)
  # retrieve all recode-pairs, i.e. all old-value = new-value assignments
  rec_pairs <- strsplit(rec_string, "=", fixed = TRUE)
  # -------------------------------
  # check for correct syntax
  # -------------------------------
  correct_syntax <- unlist(lapply(rec_pairs, function(r) if (length(r) != 2) r else NULL))
  # found any errors in syntax?
  if (!is.null(correct_syntax)) {
    stop(sprintf("?Syntax error in parameter \"%s\"", paste(correct_syntax, collapse = "=")), call. = F)
  }
  # -------------------------------
  # the new, recoded variable
  # -------------------------------
  new_var <- rep(-Inf, length(x))
  # -------------------------------
  # now iterate all recode pairs
  # and do each recoding step
  # -------------------------------
  for (i in 1:length(rec_pairs)) {
    # retrieve recode pairs as string, and start with separaring old-values
    # at comma separator
    old_val_string <- unlist(strsplit(rec_pairs[[i]][1], ",", fixed = TRUE))
    new_val_string <- rec_pairs[[i]][2]
    new_val <- c()
    # -------------------------------
    # check if new_val_string is correct syntax
    # -------------------------------
    if (new_val_string == "NA") {
      # here we have a valid NA specification
      new_val <- NA
    } else if (new_val_string == "keep") {
      # keep all remaining values, i.e. don't recode
      # remaining values that have not else been specified
      # or recoded. NULL indicates the "keep"-token
      new_val <- NULL
    } else {
      # can new value be converted to numeric?
      new_val <- suppressWarnings(as.numeric(new_val_string))
      # if not, assignment is wrong
      if (is.na(new_val)) {
        stop(sprintf("?Syntax error in parameter \"%s\"", paste(rec_pairs[[i]], collapse = "=")), call. = F)
      }
    }
    # -------------------------------
    # retrieve and check old values
    # -------------------------------
    old_val <- c()
    for (j in 1:length(old_val_string)) {
      # copy to shorten code
      ovs <- old_val_string[j]
      # -------------------------------
      # check if old_val_string is correct syntax
      # -------------------------------
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
          stop(sprintf("?Syntax error in parameter \"%s\"", ovs), call. = F)
        }
        # add range to vector of old values
        old_val <- c(old_val, seq(from, to))
      } else {
        # can new value be converted to numeric?
        ovn <- suppressWarnings(as.numeric(ovs))
        # if not, assignment is wrong
        if (is.na(ovn)) {
          stop(sprintf("?Syntax error in parameter \"%s\"", ovs), call. = F)
        }
        # add old recode values to final vector of values
        old_val <- c(old_val, ovn)
      }
    }
    # --------------------------------------
    # now we have all recode values and want
    # to replace old with new values...
    # --------------------------------------
    for (k in 1:length(old_val)) {
      # check for "else" token
      if (is.infinite(old_val[k])) {
        # else-token found. we first need to retain NA, but only,
        # if these haven't been copied before
        if (!na_recoded) new_var[which(is.na(x))] <- NA
        # find replace-indices. since "else"-token has to be
        # the last parameter in the "recodes"-string, the remaining,
        # non-recoded values are still "-Inf". Hence, find positions
        # of all not yet recoded values
        rep.pos <- which(new_var == -Inf)
        # else token found, now check whether we have a "keep"
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
      } else {
        # else we have numeric values, which should be replaced
        new_var[which(x == old_val[k])] <- new_val
      }
    }
  }
  # replace remaining -Inf with NA
  if (any(is.infinite(new_var))) new_var[which(new_var == -Inf)] <- NA
  # set back variable and value labels
  new_var <- set_var_labels(new_var, var_lab)
  new_var <- set_val_labels(new_var, val_lab)
  return(new_var)
}


#' @title Set NA for specific variable values
#' @name set_na
#'
#' @description This function sets specific values of a variable, data frame
#'                or \code{list} of variables as missings (\code{NA}).
#'
#' @seealso \code{\link{rec}} for general recoding of variables and \code{\link{recode_to}}
#'            for re-shifting value ranges.
#'
#' @param x a variable (vector), data frame or \code{list} of variables where new
#'          missing values should be defined. If \code{x} is a data frame, each
#'          column is assumed to be a new variable, where missings should be defined.
#' @param values a numeric vector with values that should be replaced with \code{\link{NA}}'s.
#'          Thus, for each variable in \code{x}, \code{values} are replaced by \code{NA}'s.
#'
#' @return \code{x}, where each value of \code{values} is replaced by an \code{NA}.
#'
#' @note Value and variable label attributes (see, for instance, \code{\link{get_val_labels}}
#'         or \code{\link{set_val_labels}}) are retained.
#'
#' @examples
#' # create random variable
#' dummy <- sample(1:8, 100, replace = TRUE)
#' # show value distribution
#' table(dummy)
#' # set value 1 and 8 as missings
#' dummy <- set_na(dummy, c(1, 8))
#' # show value distribution, including missings
#' table(dummy, exclude = NULL)
#'
#' # create sample data frame
#' dummy <- data.frame(var1 = sample(1:8, 100, replace = TRUE),
#'                     var2 = sample(1:10, 100, replace = TRUE),
#'                     var3 = sample(1:6, 100, replace = TRUE))
#' # show head of data frame
#' head(dummy)
#' # set value 2 and 4 as missings
#' dummy <- set_na(dummy, c(2, 4))
#' # show head of new data frame
#' head(dummy)
#'
#' # create list of variables
#' data(efc)
#' dummy <- list(efc$c82cop1, efc$c83cop2, efc$c84cop3)
#' # check original distribution of categories
#' lapply(dummy, table, exclude = NULL)
#' # set 3 to NA
#' lapply(set_na(dummy, 3), table, exclude = NULL)
#'
#' @export
set_na <- function(x, values) {
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # dichotomize all
    for (i in 1:nvars) x[[i]] <- set_na_helper(x[[i]], values)
    return(x)
  } else {
    return(set_na_helper(x, values))
  }
}


set_na_helper <- function(var, values) {
  # ----------------------------
  # auto-detect variable label attribute
  # ----------------------------
  attr.string <- getValLabelAttribute(var)
  # check if var has label attributes
  if (!is.null(attr.string)) {
    # retrieve value labels
    vl <- attr(var, attr.string, exact = T)
    # retrieve label names
    ln <- names(vl)
  } else {
    # if var has no label attributes, use values
    # as labels
    vl <- as.character(sort(unique(na.omit(var))))
    ln <- vl
  }
  # iterate all values that should be
  # replaced by NA's
  for (i in seq_along(values)) {
    # find associated values in var
    # and set them to NA
    var[var == values[i]] <- NA
    # check if value labels exist, and if yes, remove them
    labelpos <- suppressWarnings(which(as.numeric(vl) == values[i]))
    # remove NA label
    if (length(labelpos > 0)) {
      vl <- vl[-labelpos]
      ln <- ln[-labelpos]
    } else {
      # if vl were not numeric convertable, try character conversion
      # check if value labels exist, and if yes, remove them
      labelpos <- suppressWarnings(which(as.character(vl) == values[i]))
      # remove NA label
      if (length(labelpos > 0)) {
        vl <- vl[-labelpos]
        ln <- ln[-labelpos]
      }
    }
  }
  # set back updated label attribute
  if (!is.null(attr.string)) {
    # do we have any labels left?
    if (length(vl) > 0) {
      # if yes, set back label attribute
      attr(var, attr.string) <- vl
      names(attr(var, attr.string)) <- ln
    } else {
      # else remove attribute
      attr(var, attr.string) <- NULL
    }
  }
  return(var)
}


#' @title Weight a variable
#' @name weight2
#'
#' @description This function weights the variable \code{var} by
#'                a specific vector of \code{weights}. It's an
#'                alternative weight calculation to \code{\link{weight}},
#'                though \code{\link{weight}} usage is recommended.
#'                This function sums up all \code{weights} values of the associated
#'                categories of \code{var}, whereas the \code{\link{weight}} function
#'                uses a \code{\link{xtabs}} formula to weight cases. Thus, this function
#'                may return a vector of different length than \code{var}.
#'
#' @seealso \code{\link{weight}}
#'
#' @param var The (unweighted) variable
#' @param weights A vector with same length as \code{var}, which
#'          contains weight factors. Each value of \code{var} has a
#'          specific assigned weight in \code{weights}.
#'
#' @return The weighted \code{var}.
#'
#' @note See 'Note' in \code{\link{weight}}
#'
#' @examples
#' v <- sample(1:4, 20, TRUE)
#' table(v)
#' w <- abs(rnorm(20))
#' table(weight2(v, w))
#'
#' @export
weight2 <- function(var, weights) {
  items <- unique(var)
  newvar <- c()
  for (i in 1:length(items)) {
    newcount = round(sum(weights[which(var == items[i])]))
    newvar <- c(newvar, rep(items[i], newcount))
  }
  return(newvar)
}


#' @title Weight a variable
#' @name weight
#' @description This function weights the variable \code{var} by
#'                a specific vector of \code{weights}.
#'
#' @seealso \code{\link{weight2}}
#'
#' @param var The (unweighted) variable
#' @param weights A vector with same length as \code{var}, which
#'          contains weight factors. Each value of \code{var} has a
#'          specific assigned weight in \code{weights}.
#'
#' @return The weighted \code{var}.
#'
#' @note The values of the returned vector are in sorted order, whereas the values'
#'        order of the original \code{var} may be spread randomly. Hence, \code{var} can't be
#'        used, for instance, for further cross tabulation. In case you want to have
#'        weighted contingency tables or (grouped) box plots etc., use the \code{weightBy}
#'        parameter of most functions.
#'
#' @examples
#' v <- sample(1:4, 20, TRUE)
#' table(v)
#' w <- abs(rnorm(20))
#' table(weight(v, w))
#'
#' @export
weight <- function(var, weights) {
  # init values
  weightedvar <- c()
  wtab <- round(xtabs(weights ~ var,
                      data = data.frame(weights = weights, var = var),
                      na.action = na.pass,
                      exclude = NULL))
  # iterate all table values
  for (w in 1:length(wtab)) {
    # retrieve count of each table cell
    w_count <- wtab[[w]]
    # retrieve "cell name" which is identical to the variable value
    w_value <- as.numeric(names(wtab[w]))
    # append variable value, repeating it "w_count" times.
    weightedvar <- c(weightedvar, rep(w_value, w_count))
  }
  return(weightedvar)
}


#' @title Group near elements of string vectors
#' @name group_str
#'
#' @seealso \code{\link{str_pos}}
#'
#' @description This function groups elements of a string vector (character or string
#'                variable) according to the element's distance ('similatiry'). The
#'                more similar two string elements are, the higher is the
#'                chance to be combined into a group.
#'
#' @param strings a character vector with string elements
#' @param maxdist the maximum distance between two string elements, which is allowed to treat two
#'          elements as similar or equal.
#' @param method Method for distance calculation. The default is \code{"lv"}. See
#'          \code{\link[stringdist]{stringdist}} package for details.
#' @param strict if \code{TRUE}, value matching is more strictly. See examples for details.
#' @param trim.whitespace if \code{TRUE} (default), leading and trailing white spaces will
#'          be removed from string values.
#' @param remove.empty if \code{TRUE} (default), empty string values will be removed from the
#'          character vector \code{strings}.
#' @param showProgressBar If \code{TRUE}, the progress bar is displayed when computing the distance matrix.
#'          Default in \code{FALSE}, hence the bar is hidden.
#'
#' @return A character vector where similar string elements (values) are recoded into a new, single value.
#'
#' @examples
#' \dontrun{
#' library(sjPlot)
#' oldstring <- c("Hello", "Helo", "Hole", "Apple",
#'                "Ape", "New", "Old", "System", "Systemic")
#' newstring <- group_str(oldstring)
#' sjt.frq(data.frame(oldstring, newstring),
#'         removeStringVectors = FALSE,
#'         autoGroupStrings = FALSE)
#'
#' newstring <- group_str(oldstring, strict = TRUE)
#' sjt.frq(data.frame(oldstring, newstring),
#'         removeStringVectors = FALSE,
#'         autoGroupStrings = FALSE)}
#'
#' @export
group_str <- function(strings, maxdist = 2, method = "lv", strict = FALSE, trim.whitespace = TRUE, remove.empty = TRUE, showProgressBar = FALSE) {
  # -------------------------------------
  # check if required package is available
  # -------------------------------------
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    stop("Package 'stringdist' needed for this function to work. Please install it.", call. = FALSE)
  }
  # -------------------------------------
  # coerce to character, if necessary
  # -------------------------------------
  if (!is.character(strings)) strings <- as.character(strings)
  # -------------------------------------
  # trim white spaces
  # -------------------------------------
  if (trim.whitespace) {
    for (i in 1:length(strings)) strings[i] <- trim(strings[i])
  }
  # -------------------------------------
  # remove empty values
  # -------------------------------------
  if (remove.empty) {
    removers <- c()
    for (i in 1:length(strings)) {
      if (0 == nchar(strings[i])) removers <- c(removers, i)
    }
    if (length(removers) > 0) strings <- strings[-removers]
  }
  # -------------------------------------
  # create matrix from string values of variable
  # -------------------------------------
  m <- stringdist::stringdistmatrix(strings, strings, method = method)
  colnames(m) <- strings
  rownames(m) <- strings
  # -------------------------------------
  # init variable that contains "close" pairs
  # -------------------------------------
  pairs <- list()
  # -------------------------------------
  # helper function that finds elements in
  # final list of grouped elements
  # -------------------------------------
  findInPairs <- function(curel) {
    elfound <- FALSE
    if (length(pairs) > 0) {
      for (ll in 1:length(pairs)) {
        pel <- pairs[[ll]]
        if (any(pel == curel)) elfound <- TRUE
      }
    }
    return(elfound)
  }
  # -------------------------------------
  # create progress bar
  # -------------------------------------
  if (showProgressBar) pb <- txtProgressBar(min = 0,
                                            max = ncol(m),
                                            style = 3)
  # -------------------------------------
  # iterate matrix
  # -------------------------------------
  for (i in 1:nrow(m)) {
    # update progress bar
    if (showProgressBar) setTxtProgressBar(pb, i)
    # -------------------------------------
    # check if current element is already grouped
    # -------------------------------------
    if (!findInPairs(rownames(m)[i])) {
      # -------------------------------------
      # current row element has not been grouped
      # yet, so go on...
      # -------------------------------------
      pairvector <- c()
      for (j in 1:ncol(m)) {
        # -------------------------------------
        # check if we found a pair's distance that
        # is within the maximum requested distance
        # i.e. which are "close" enough
        # -------------------------------------
        if (m[i, j] <= maxdist) {
          # -------------------------------------
          # go through all rows of this column and
          # check if there's a better match for the
          # currently compared token
          # -------------------------------------
          foundBetterToken <- !strict
          for (cnt in 1:nrow(m)) {
            if (strict) {
              if (m[cnt, j] > 0 && m[cnt, j] < m[i, j]) foundBetterToken <- TRUE
            } else {
              if (m[cnt, j] <= maxdist && m[i, cnt] <= maxdist) foundBetterToken <- FALSE
            }
          }
          # -------------------------------------
          # in the current column, there's no better
          # matching of strings, so we pick this values
          # and add it to our results
          # -------------------------------------
          if (!foundBetterToken) {
            # -------------------------------------
            # remember string value
            # -------------------------------------
            token <- colnames(m)[j]
            # -------------------------------------
            # check if we already found a string value
            # within this column. if not, add string values
            # to "close" pairs of this column
            # -------------------------------------
            if (!any(pairvector == token) && !findInPairs(token)) pairvector <- c(pairvector, token)
          }
        }
      }
      # -------------------------------------
      # now we have a vector with all "close" string values
      # from the current row's value
      # -------------------------------------
      pairvector <- sort(pairvector)
      # -------------------------------------
      # check if we already have saved these values to our list
      # if not, add "close" values as new list element
      # -------------------------------------
      if (!any(unlist(lapply(pairs, function(x) length(x) == length(pairvector) && any(x == pairvector))))) pairs <- c(pairs, list(pairvector))
    }
  }
  # -------------------------------------
  # we now have a list, where each list element
  # is a vector of "close" string values
  # -------------------------------------
  strings.new <- c()
  # -------------------------------------
  # go through each list element
  # -------------------------------------
  for (i in 1:length(pairs)) {
    r <- pairs[[i]]
    # -------------------------------------
    # find vector indices of "close" values in
    # original string
    # -------------------------------------
    indices <- unlist(lapply(r, function(x) which(strings == x)))
    newvalue <- r[1]
    count <- 2
    # -------------------------------------
    # "merge" each close values into one
    # single value that combines all close values
    # -------------------------------------
    while (count <= length(r)) {
      newvalue <- paste0(newvalue, ", ", r[count])
      count <- count + 1
    }
    strings.new[indices] <- newvalue
  }
  if (showProgressBar) close(pb)
  # -------------------------------------
  # return new vector, where all single "close"
  # values are replaced by the group of closed values.
  # e.g. the three values "hello", "holle" and "hole"
  # will be "recoded" into on value "hello, holle, hole"
  # -------------------------------------
  return(strings.new)
}


#' @title Trim leading and trailing whitespaces from strings
#' @name trim
#'
#' @param x a character vector or string. Function is vectorized, i.e. vector
#'          may have a length greater than 1. See 'Examples'.
#'
#' @return Trimmed \code{x}, i.e. with leading and trailing spaces removed.
#'
#' @examples
#' trim("white space at end ")
#' trim(" white space at start and end ")
#' trim(c(" string1 ", "   string2", "string 3   "))
#'
#' @export
trim <- function(x) gsub("^\\s+|\\s+$", "", x)



#' @title Find partial matching and close distance elements in strings
#' @name str_pos
#' @description This function finds the element indices of partial matching or similar strings
#'                in a character vector. Can be used to find exact or slightly mistyped elements
#'                in a string vector.
#'
#' @seealso \code{\link{group_str}}
#'
#' @param searchString a character vector with string elements
#' @param findTerm the string that should be matched against the elements of \code{searchString}.
#' @param maxdist the maximum distance between two string elements, which is allowed to treat them
#'          as similar or equal.
#' @param part.dist.match activates similar matching (close distance strings) for parts (substrings)
#'          of the \code{searchString}. Following values are accepted:
#'          \itemize{
#'            \item 0 for no partial distance matching
#'            \item 1 for one-step matching, which means, only substrings of same length as \code{findTerm} are extracted from \code{searchString} matching
#'            \item 2 for two-step matching, which means, substrings of same length as \code{findTerm} as well as strings with a slightly wider range are extracted from \code{searchString} matching
#'          }
#'          Default value is 0. See 'Details' for more information.
#' @param showProgressBar If \code{TRUE}, the progress bar is displayed when computing the distance matrix.
#'          Default in \code{FALSE}, hence the bar is hidden.
#'
#' @return A numeric vector with index position of elements in \code{searchString} that
#'           partially match or are similar to \code{findTerm}. Returns \code{-1} if no
#'           match was found.
#'
#' @note This function does \emph{not} return the position of a matching string \emph{inside}
#'         another string, but the element's index of the \code{searchString} vector, where
#'         a (partial) match with \code{findTerm} was found. Thus, searching for "abc" in
#'         a string "this is abc" will not return 9 (the start position of the substring),
#'         but 1 (the element index, which is always 1 if \code{searchString} only has one element).
#'
#' @details For \code{part.dist.match = 1}, a substring of \code{length(findTerm)} is extracted
#'            from \code{searchString}, starting at position 0 in \code{searchString} until
#'            the end of \code{searchString} is reached. Each substring is matched against
#'            \code{findTerm}, and results with a maximum distance of \code{maxdist}
#'            are considered as "matching". If \code{part.dist.match = 2}, the range
#'            of the extracted substring is increased by 2, i.e. the extracted substring
#'            is two chars longer.
#'
#' @examples
#' \dontrun{
#' string <- c("Hello", "Helo", "Hole", "Apple", "Ape", "New", "Old", "System", "Systemic")
#' str_pos(string, "hel")   # partial match
#' str_pos(string, "stem")  # partial match
#' str_pos(string, "R")     # no match
#' str_pos(string, "saste") # similarity to "System"
#'
#' # finds two indices, because partial matching now
#' # also applies to "Systemic"
#' str_pos(string,
#'         "sytsme",
#'         part.dist.match = 1)
#'
#' # finds nothing
#' str_pos("We are Sex Pistols!", "postils")
#' # finds partial matching of similarity
#' str_pos("We are Sex Pistols!", "postils", part.dist.match = 1)}
#'
#' @export
str_pos <- function(searchString,
                    findTerm,
                    maxdist = 2,
                    part.dist.match = 0,
                    showProgressBar = FALSE) {
  # -------------------------------------
  # init return value
  # -------------------------------------
  indices <- c()
  # -------------------------------------
  # find element indices from partial matching of string and find term
  # -------------------------------------
  pos <- as.numeric(grep(findTerm, searchString, ignore.case = T))
  if (length(pos) > 0) indices <- c(indices, pos)
  # -------------------------------------
  # check if required package is available
  # -------------------------------------
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    warning("Package 'stringdist' needed for this function to fully work. Please install it. Only partial matching indices are returned.", call. = F)
    return(indices)
  }
  # -------------------------------------
  # find element indices from similar strings
  # -------------------------------------
  pos <- which(stringdist::stringdist(tolower(findTerm), tolower(searchString)) <= maxdist)
  if (length(pos) > 0) indices <- c(indices, pos)
  # -------------------------------------
  # find element indices from partial similar (distance)
  # string matching
  # -------------------------------------
  if (part.dist.match > 0) {
    # -------------------------------------
    # helper function to trim white spaces
    # -------------------------------------
    trim <- function(x) gsub("^\\s+|\\s+$", "", x)
    ftlength <- nchar(findTerm)
    # -------------------------------------
    # create progress bar
    # -------------------------------------
    if (showProgressBar) pb <- txtProgressBar(min = 0,
                                              max = length(searchString),
                                              style = 3)
    # -------------------------------------
    # iterate search string vector
    # -------------------------------------
    for (ssl in 1:length(searchString)) {
      # -------------------------------------
      # retrieve each element of search string vector
      # we do this step by step instead of vectorizing
      # due to the substring approach
      # -------------------------------------
      sst <- searchString[ssl]
      # -------------------------------------
      # we extract substrings of same length as findTerm
      # starting from first char of searchString until end
      # and try to find similar matches
      # -------------------------------------
      steps <- nchar(sst) - ftlength + 1
      for (pi in 1:steps) {
        # -------------------------------------
        # retrieve substring
        # -------------------------------------
        sust <- trim(substr(sst, pi, pi + ftlength - 1))
        # -------------------------------------
        # find element indices from similar substrings
        # -------------------------------------
        pos <- which(stringdist::stringdist(tolower(findTerm), tolower(sust)) <= maxdist)
        if (length(pos) > 0) indices <- c(indices, ssl)
      }
      if (part.dist.match > 1) {
        # -------------------------------------
        # 2nd loop picks longer substrings, because similarity
        # may also be present if length of strings differ
        # (e.g. "app" and "apple")
        # -------------------------------------
        steps <- nchar(sst) - ftlength
        if (steps > 1) {
          for (pi in 2:steps) {
            # -------------------------------------
            # retrieve substring
            # -------------------------------------
            sust <- trim(substr(sst, pi - 1, pi + ftlength))
            # -------------------------------------
            # find element indices from similar substrings
            # -------------------------------------
            pos <- which(stringdist::stringdist(tolower(findTerm), tolower(sust)) <= maxdist)
            if (length(pos) > 0) indices <- c(indices, ssl)
          }
        }
      }
      # update progress bar
      if (showProgressBar) setTxtProgressBar(pb, ssl)
    }
  }
  if (showProgressBar) close(pb)
  # -------------------------------------
  # return result
  # -------------------------------------
  if (length(indices) > 0) return(sort(unique(indices)))
  return(-1)
}


#' @title Row means with min amount of valid values
#' @name mean_n
#' @description This function is similar to the SPSS \code{MEAN.n} function and computes
#'                row means from a \code{\link{data.frame}} or \code{\link{matrix}} if at least \code{n}
#'                values of a row are valid (and not \code{\link{NA}}).
#'
#' @param dat a \code{\link{data.frame}} with at least two columns, where row means are applied.
#' @param n May either be
#'          \itemize{
#'            \item a numeric value that indicates the amount of valid values per row to calculate the row mean;
#'            \item or a value between 0 and 1, indicating a proportion of valid values per row to calculate the row mean (see details).
#'          }
#'          If a row's amount of valid values is less than \code{n}, \code{\link{NA}} will be returned as row mean value.
#' @param digits numeric value indicating the number of decimal places to be used for rounding mean
#'          value. Negative values are allowed (see ‘Details’).
#'
#' @return A vector with row mean values of \code{df} for those rows with at least \code{n}
#'           valid values. Else, \code{\link{NA}} is returned.
#'
#' @details Rounding to a negative number of \code{digits} means rounding to a power of
#'            ten, so for example mean_n(df, 3, digits = -2) rounds to the
#'            nearest hundred. \cr \cr
#'          For \code{n}, must be a numeric value from \code{0} to \code{ncol(dat)}. If
#'            a \emph{row} in \code{dat} has at least \code{n} non-missing values, the
#'            row mean is returned. If \code{n} is a non-integer value from 0 to 1,
#'            \code{n} is considered to indicate the proportion of necessary non-missing
#'            values per row. E.g., if \code{n = .75}, a row must have at least \code{ncol(dat) * n}
#'            non-missing values for the row mean to be calculated. See examples.
#'
#' @references \itemize{
#'              \item \href{http://candrea.ch/blog/compute-spss-like-mean-index-variables/}{candrea's blog}
#'              \item \href{http://r4stats.com/2014/09/03/adding-the-spss-mean-n-function-to-r/}{r4stats.com}
#'              }
#'
#' @examples
#' dat <- data.frame(c1 = c(1,2,NA,4),
#'                   c2 = c(NA,2,NA,5),
#'                   c3 = c(NA,4,NA,NA),
#'                   c4 = c(2,3,7,8))
#'
#' # needs at least 4 non-missing values per row
#' mean_n(dat, 4) # 1 valid return value
#'
#' # needs at least 3 non-missing values per row
#' mean_n(dat, 3) # 2 valid return values
#'
#' # needs at least 2 non-missing values per row
#' mean_n(dat, 2)
#'
#' # needs at least 1 non-missing value per row
#' mean_n(dat, 1) # all means are shown
#'
#' # needs at least 50% of non-missing values per row
#' mean_n(dat, .5) # 3 valid return values
#'
#' # needs at least 75% of non-missing values per row
#' mean_n(dat, .75) # 2 valid return values
#'
#' @export
mean_n <- function(dat, n, digits = 2) {
  # ---------------------------------------
  # is 'n' indicating a proportion?
  # ---------------------------------------
  digs <- n %% 1
  if (digs != 0) n <- round(ncol(dat) * digs)
  # ---------------------------------------
  # coerce matrix to data frame
  # ---------------------------------------
  if (is.matrix(dat)) dat <- as.data.frame(dat)
  # ---------------------------------------
  # check if we have a data framme with at least two columns
  # ---------------------------------------
  if (!is.data.frame(dat) || ncol(dat) < 2) {
    warning("'dat' must be a data.frame with at least two columns.", call. = F)
    return(NA)
  }
  # ---------------------------------------
  # n may not be larger as df's amount of columns
  # ---------------------------------------
  if (ncol(dat) < n) {
    warning("'n' must be smaller or equal to data.frame's amount of columns.", call. = F)
    return(NA)
  }
  round(apply(dat, 1, function(x) ifelse(sum(!is.na(x)) >= n, mean(x, na.rm = TRUE), NA)), digits)
}
