#' @title Convert a haven-imported data frame to sjPlot format
#' @name to_sjPlot
#'
#' @description This function converts a data frame, which was imported with any of
#'                \code{haven}'s read functions and contains \code{\link[haven]{labelled}} class vectors or
#'                a single vector of type \code{labelled} into an sjPlot friendly data
#'                frame format, which means that simply all \code{\link[haven]{labelled}} class
#'                attributes will be removed, so all vectors / variables will most
#'                likely become \code{\link{atomic}}. Additionally, \code{tbl_df} and
#'                \code{tbl} class attributes will be removed from data frames. See 'Note'.
#'
#' @seealso \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data initialization}
#'
#' @param x a data frame, which contains \code{\link[haven]{labelled}} class vectors or a single vector
#'          of class \code{labelled}.
#' @return a data frame or single vector (depending on \code{x}) with 'sjPlot' friendly
#'           object classes.
#'
#' @note This function is currently only used to avoid possible compatibility issues
#'         with \code{\link[haven]{labelled}} class vectors and \code{tbl_df} resp.
#'         \code{tbl} class attributes for data frames. Some known issues with \code{\link[haven]{labelled}}
#'         class vectors have already been fixed, so it might be that this function
#'         will become redundant in the future. Currently, data frames with \code{tbl_df} and
#'         \code{tbl} class attributes may cause difficulties when indexing columns
#'         like \code{data.frame[, colnr]} - only \code{data.frame[[colnr]]} seems
#'         to be safe when accessing data frame columns from within function calls.
#'
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export
to_sjPlot <- function(x) {
  # -------------------------------------
  # check if complete data frame or only single
  # vector should be converted
  # -------------------------------------
  if (is.data.frame(x) || is.matrix(x)) {
    # -------------------------------------
    # create progress bar
    # -------------------------------------
    pb <- utils::txtProgressBar(min = 0,
                                max = ncol(x),
                                style = 3)
    # tell user...
    message("Converting labelled-classes. Please wait...\n")
    for (i in 1:ncol(x)) {
      # remove labelled class
      if (is_labelled(x[[i]])) x[[i]] <- unclass(x[[i]])
      # update progress bar
      utils::setTxtProgressBar(pb, i)
    }
    close(pb)
    # remove redundant class attributes
    class(x) <- "data.frame"
  } else {
    # remove labelled class
    if (is_labelled(x)) x <- unclass(x)
  }
  return(x)
}


#' @title Convert variable into factor and replaces values with associated value labels
#' @name to_label
#'
#' @description This function converts (replaces) variable values (also of factors)
#'                with their associated value labels. Might be helpful for factor variables.
#'                For instance, if you have a Gender variable with 0/1 value, and associated
#'                labels are male/female, this function would convert all 0 to male and
#'                all 1 to female and returns the new variable as \code{\link{factor}}.
#'
#' @seealso \code{\link{to_factor}} to convert a numeric variable into a factor (and
#'            retain labels) and \code{\link{to_value}} to convert a factor into
#'            a numeric variable.
#'
#' @param x A variable of type \code{\link{numeric}}, \code{\link{atomic}},
#'          \code{\link{factor}} or \code{\link[haven]{labelled}}
#'          \emph{with associated value labels} (see \code{\link{set_labels}}),
#'          respectively a data frame with such variables.
#' @param add.non.labelled logical, if \code{TRUE}, values without associated
#'          value label will also be converted to labels (as is). See 'Examples'.
#' @param drop.na logical, if \code{TRUE}, all types of missing value codes are
#'          converted into NA before \code{x} is converted as factor. If
#'          \code{FALSE}, missing values will be left as their original codes.
#'          See 'Examples' and \code{\link{get_na}}.
#' @return A factor variable with the associated value labels as factor levels, or a
#'           data frame with such factor variables (if \code{x} was a data frame).
#'
#' @note Value and variable label attributes (see, for instance, \code{\link{get_labels}}
#'         or \code{\link{set_labels}}) will be removed  when converting variables to factors.
#'         \cr \cr
#'         Factors with non-numeric factor-levels won't be changed and returned "as is"
#'         (see 'Examples').
#'
#' @details See 'Details' in \code{\link{get_na}}.
#'
#' @examples
#' data(efc)
#' print(get_labels(efc)['c161sex'])
#' head(efc$c161sex)
#' head(to_label(efc$c161sex))
#'
#' print(get_labels(efc)['e42dep'])
#' table(efc$e42dep)
#' table(to_label(efc$e42dep))
#'
#' head(efc$e42dep)
#' head(to_label(efc$e42dep))
#'
#' # structure of numeric values won't be changed
#' # by this function, it only applies to labelled vectors
#' # (typically categorical or factor variables)
#' str(efc$e17age)
#' str(to_label(efc$e17age))
#'
#' \dontrun{
#' # factor with non-numeric levels won't be changed, either,
#' # however, a warning is produced
#' to_label(factor(c("a", "b", "c")))}
#'
#' # create vector
#' x <- c(1, 2, 3, 2, 4, NA)
#' # add less labels than values
#' x <- set_labels(x, c("yes", "maybe", "no"), add.non.labelled = FALSE)
#' # convert to label w/o non-labelled values
#' to_label(x)
#' # convert to label, including non-labelled values
#' to_label(x, add.non.labelled = TRUE)
#'
#'
#' library(haven)
#' # create labelled integer, with missing flag
#' x <- labelled(c(1, 2, 1, 3, 4, 1),
#'               c(Male = 1, Female = 2, Refused = 3, "N/A" = 4),
#'               c(FALSE, FALSE, TRUE, TRUE))
#' # to labelled factor, with missing labels
#' to_label(x, drop.na = FALSE)
#' # to labelled factor, missings removed
#' to_label(x, drop.na = TRUE)
#'
#' @export
to_label <- function(x, add.non.labelled = FALSE, drop.na = TRUE) {
  if (is.matrix(x) || is.data.frame(x)) {
    for (i in 1:ncol(x)) x[[i]] <- to_label_helper(x[[i]],
                                                   add.non.labelled,
                                                   drop.na)
    return(x)
  } else {
    return(to_label_helper(x,
                           add.non.labelled,
                           drop.na))
  }
}


to_label_helper <- function(x, add.non.labelled, drop.na) {
  # check if factor has numeric factor levels
  if (is.factor(x) && !is_num_fac(x)) {
    # if not, stop here - factor levels are already "labelled".
    warning("'x' may have numeric factor levels only.", call. = F)
    return(x)
  }
  # remove missings?
  if (drop.na) x <- to_na(x)
  # get value labels
  vl <- get_labels(x,
                   attr.only = TRUE,
                   include.values = NULL,
                   include.non.labelled = add.non.labelled)
  # check if we have any labels, else
  # return variable "as is"
  if (!is.null(vl)) {
    # get associated values for value labels
    vn <- get_values(x, sort.val = FALSE, drop.na = FALSE)
    # replace values with labels
    if (is.factor(x)) {
      # set new levels
      levels(x) <- vl
      # remove attributes
      x <- remove_labels(x)
    } else {
      for (i in 1:length(vl)) x[x == vn[i]] <- vl[i]
      # to factor
      x <- factor(x, levels = vl)
    }
  }
  # return as factor
  return(x)
}


#' @title Remove value and variable labels from vector or data frame
#' @name remove_labels
#'
#' @description This function removes value and variable label attributes
#'                from a vector or data frame. These attributes are typically
#'                added to variables when importing foreign data (see
#'                \code{\link{read_spss}}) or manually adding label attributes
#'                with \code{\link{set_labels}}.
#'
#' @seealso \href{http://www.strengejacke.de/sjPlot/labelleddata/}{sjPlot-manual}
#'            on working with labelled data, and \code{\link{add_labels}} for
#'            adding label attributes (subsetted) data frames.
#'
#' @param x vector or \code{data.frame} with variable and/or value label attributes
#' @return \code{x} with removed value and variable label attributes.
#'
#' @examples
#' data(efc)
#' str(efc)
#' str(remove_labels(efc))
#'
#' @export
remove_labels <- function(x) {
  if (is.data.frame(x)) {
    for (i in 1:ncol(x)) x[[i]] <- remove_labels_helper(x[[i]])
  } else {
    x <- remove_labels_helper(x)
  }
  return(x)
}


remove_labels_helper <- function(x) {
  # find label-attribute string
  attr.string <- getValLabelAttribute(x)
  # remove attributes
  if (!is.null(attr.string)) attr(x, attr.string) <- NULL
  # find label-attribute string
  attr.string <- getVarLabelAttribute(x)
  # remove attributes
  if (!is.null(attr.string)) attr(x, attr.string) <- NULL
  # remove is_na attribute
  na.attr <- getNaFromAttribute(x)
  if (!is.null(na.attr)) attr(x, getNaAttribute()) <- NULL
  # unclass, if labelled. labelled class may throw
  # errors / warnings, when not havin label attributes
  if (is_labelled(x)) x <- unclass(x)
  # return var
  return(x)
}


#' @title Convert variable into factor and keep value labels
#' @name to_fac
#'
#' @description This function converts a variable into a factor, but keeps
#'                variable and value labels, if these are attached as attributes
#'                to the variale. See 'Examples'.
#'
#' @seealso \code{\link{to_value}} to convert a factor into a numeric value and
#'            \code{\link{to_label}} to convert a value into a factor with labelled
#'            factor levels.
#'
#' @param x A (numeric or atomic) variable or a data frame with
#'          (numeric or atomic) variables.
#' @param drop.na logical, if \code{TRUE}, all types of missing value codes are
#'          converted into NA before \code{x} is converted as factor. If
#'          \code{FALSE}, missing values will be left as their original codes.
#'          See 'Examples' and \code{\link{get_na}}.
#' @return A factor variable, including variable and value labels, respectively
#'           a data frame with factor variables (including variable and value labels)
#'           if \code{x} was a data frame.
#'
#' @note This function is intended for use with vectors that have value and variable
#'        labels attached. Unlike \code{\link{as.factor}}, \code{to_factor} converts
#'        a variable into a factor and retains the value and variable label attributes.
#'        \cr \cr
#'        Attaching labels is automatically done by importing data sets
#'        with one of the \code{read_*}-functions, like \code{\link{read_spss}}.
#'        Else, value and variable labels can be manually added to vectors
#'        with \code{\link{set_labels}} and \code{\link{set_label}}.
#'
#' @details See 'Details' in \code{\link{get_na}}.
#'
#' @examples
#' \dontrun{
#' data(efc)
#' library(sjPlot)
#' # normal factor conversion, loses value attributes
#' efc$e42dep <- as.factor(efc$e42dep)
#' sjt.frq(efc$e42dep)
#'
#' data(efc)
#' # factor conversion, which keeps value attributes
#' efc$e42dep <- to_factor(efc$e42dep)
#' sjt.frq(efc$e42dep)}
#'
#' library(haven)
#' # create labelled integer, with missing flag
#' x <- labelled(c(1, 2, 1, 3, 4, 1),
#'               c(Male = 1, Female = 2, Refused = 3, "N/A" = 4),
#'               c(FALSE, FALSE, TRUE, TRUE))
#' # to factor, with missing labels
#' to_factor(x, drop.na = FALSE)
#' # to factor, missings removed
#' to_factor(x, drop.na = TRUE)
#'
#' @export
to_fac <- function(x, drop.na = TRUE) {
  if (is.matrix(x) || is.data.frame(x)) {
    for (i in 1:ncol(x)) x[[i]] <- to_fac_helper(x[[i]], drop.na)
    return(x)
  } else {
    return(to_fac_helper(x, drop.na))
  }
}


#' @name to_factor
#' @rdname to_fac
#' @export
to_factor <- function(x, drop.na = TRUE) {
  return(to_fac(x, drop.na))
}


to_fac_helper <- function(x, drop.na) {
  # is already factor?
  if (is.factor(x)) return(x)
  # remove missings?
  if (drop.na) x <- to_na(x)
  # retrieve value labels
  lab <- get_labels(x,
                    attr.only = TRUE,
                    include.values = NULL,
                    include.non.labelled = TRUE)
  # retrieve variable labels
  varlab <- get_label(x)
  # retrieve missing codes
  nas <- suppressMessages(get_na(x))
  # convert variable to factor
  x <- as.factor(x)
  # set back value labels
  x <- set_labels(x, lab, force.labels = FALSE, force.values = TRUE)
  # set back variable labels
  x <- set_label(x, varlab)
  # set back missing codes
  x <- set_na(x, nas, as.attr = TRUE)
  return(x)
}


#' @title Convert factors to numeric variables
#' @name to_value
#'
#' @description This function converts (replaces) factor values with the
#' related factor level index number, thus the factor is converted to
#' a numeric variable.
#'
#' @seealso \code{\link{to_label}} to convert a value into a factor with labelled
#'            factor levels and \code{\link{to_factor}} to convert a numeric variable
#'            into a factor (and retain labels)
#'
#' @param x A (factor) variable or a data frame with (factor) variables.
#' @param start.at the starting index, i.e. the lowest numeric value of the variable's
#'          value range. By default, this argument is \code{NULL}, hence the lowest
#'          value of the returned numeric variable corresponds to the lowest factor
#'          level (if factor is \code{\link{numeric}}) or to \code{1} (if factor levels
#'          are not numeric).
#' @param keep.labels logical, if \code{TRUE}, former factor levels will be attached as
#'          value labels. See \code{\link{set_labels}} for more details.
#' @return A numeric variable with values ranging either from \code{start.at} to
#'           \code{start.at} + length of factor levels, or to the corresponding
#'           factor levels (if these were numeric). Or a data frame with numeric
#'           variables, if \code{x} was a data frame.
#'
#' @examples
#' data(efc)
#' test <- to_label(efc$e42dep)
#' table(test)
#'
#' table(to_value(test))
#' hist(to_value(test, 0))
#'
#' # set lowest value of new variable
#' # to "5".
#' table(to_value(test, 5))
#'
#' # numeric factor keeps values
#' dummy <- factor(c("3", "4", "6"))
#' table(to_value(dummy))
#'
#' # do not drop unused factor levels
#' dummy <- ordered(c(rep("No", 5), rep("Maybe", 3)),
#'                  levels = c("Yes", "No", "Maybe"))
#' to_value(dummy)
#'
#' # non-numeric factor is converted to numeric
#' # starting at 1
#' dummy <- factor(c("D", "F", "H"))
#' table(to_value(dummy))
#'
#' @export
to_value <- function(x, start.at = NULL, keep.labels = TRUE) {
  if (is.matrix(x) || is.data.frame(x)) {
    for (i in 1:ncol(x)) x[[i]] <- to_value_helper(x[[i]], start.at, keep.labels)
    return(x)
  } else {
    return(to_value_helper(x, start.at, keep.labels))
  }
}


to_value_helper <- function(x, start.at, keep.labels) {
  # is already numeric?
  if (is.numeric(x)) return(x)
  # retrieve "value labels"
  labels <- levels(x)
  # check if we have numeric factor levels
  if (is_num_fac(x)) {
    # convert to numeric via as.vector
    new_value <- as.numeric(as.vector((x)))
    # new minimum value?
    if (!is.null(start.at) && is.numeric(start.at)) {
      # check if lowest value of variable differs from
      # requested minimum conversion value
      val_diff <- start.at - min(new_value, na.rm = T)
      # adjust new_value
      new_value <- new_value + val_diff
    }
  } else {
    # check start.at value
    if (is.null(start.at)) start.at <- 1
    # get amount of categories
    l <- length(levels(x))
    # determine highest category value
    end <- start.at + l - 1
    # replace labels with numeric values
    levels(x) <- c(start.at:end)
    # convert to numeric
    new_value <- as.numeric(as.character(x))
  }
  # check if we should attach former labels as value labels
  if (keep.labels) new_value <- set_labels(new_value, labels, T)
  return(new_value)
}


#' @title Set back value and variable labels to subsetted data frames
#' @name add_labels
#'
#' @description Subsetting-functions usually drop value and variable labels from
#'                subsetted data frames (if the original data frame has value and variable
#'                label attributes). This function adds back these value and variable
#'                labels to subsetted data frames that have been subsetted, for instance,
#'                 with \code{\link{subset}}.
#'                \cr \cr
#'                In case \code{df_origin = NULL}, all possible label attributes
#'                from \code{df_new} are removed.
#'
#' @seealso \href{http://www.strengejacke.de/sjPlot/labelleddata/}{sjPlot-manual}
#'            on working with labelled data, and \code{\link{remove_labels}} for
#'            removing label attributes from data frames.
#'
#' @param df_new the new, subsetted data frame
#' @param df_origin the original data frame where the subset (\code{df_new}) stems from;
#'          use \code{NULL}, if value and variable labels from \code{df_new} should be removed.
#' @return Returns \code{df_new} with either removed value and variable label attributes
#'           (if \code{df_origin = NULL}) or with added value and variable label
#'           attributes (if \code{df_origin} was the original subsetted data frame).
#'
#' @note In case \code{df_origin= NULL}, all possible label attributes
#'         from \code{df_new} are removed. dplyr >= 0.4.2 no longer drops
#'         vector attributes; you'll only need
#'         to set back labels when using dplyr up to 0.4.1.
#'
#' @examples
#' data(efc)
#' efc.sub <- subset(efc, subset = e16sex == 1, select = c(4:8))
#' str(efc.sub)
#'
#' efc.sub <- add_labels(efc.sub, efc)
#' str(efc.sub)
#'
#' efc.sub <- add_labels(efc.sub)
#' str(efc.sub)
#'
#' @export
add_labels <- function(df_new, df_origin = NULL) {
  # check if old df is NULL. if so, we remove all labels
  # from the data frame.
  if (is.null(df_origin)) {
    # tell user
    message("Removing all variable and value labels from data frame.")
    # remove all labels
    df_new <- remove_labels(df_new)
  } else {
    # check params
    if (is.data.frame(df_new) && is.data.frame(df_origin)) {
      # retrieve variables of subsetted data frame
      cn <- colnames(df_new)
      # check for valid colnames, i.e. if all column
      # names really match the original column names.
      if (sum(cn %in% colnames(df_origin) == F) > 0) {
        # if not, return only matching colnames
        cn <- cn[cn %in% colnames(df_origin)]
      }
      # get var-labels of original data frame, and select only those
      # labels from variables that appear in the new (subsetted) data frame
      df_new <- set_label(df_new, get_label(df_origin[, cn]))
      # same for value labels
      df_new <- set_labels(df_new, get_labels(df_origin[, cn],
                                              attr.only = TRUE,
                                              include.values = NULL,
                                              include.non.labelled = FALSE))
    } else {
      warning("Both 'df_origin' and 'df_new' must be of class 'data.frame'.", call. = F)
    }
  }
  return(df_new)
}


#' @title Convert missing values of labelled variables into NA
#' @name to_na
#'
#' @description This function converts missing values that are still stored as
#'                original value code into \code{NA}.
#'
#' @seealso \code{\link{get_na}} to get value codes of missing values.
#'
#' @param x a variable (vector), \code{data.frame} or \code{list} of variables
#'          with attached value labels and missing value codes
#'          (see \code{\link[haven]{labelled}}).
#' @return \code{x}, where each value code of missing values is comverted
#'            to \code{NA}.
#'
#' @details \code{to_na} converts values to \code{NA}, which are defined
#'            as missing through the \code{is_na}-attribute of a vector
#'            (see \code{\link[haven]{labelled}}). \code{\link{set_na}},
#'            by contrast, converts those values to \code{NA} that are
#'            specified in the function's \code{values} argument; hence,
#'            \code{\link{set_na}} ignores the \code{is_na}-attribute.
#'            \cr \cr
#'            Furthermore, see 'Details' in \code{\link{get_values}},
#'            \code{\link{get_na}} and \code{\link{to_na}}.
#'
#' @note This is a convenient function for \code{set_na(x, get_na(x))}.
#'
#' @examples
#' library(haven)
#'
#' # create labelled factor, with missing flag
#' x <- labelled(c("M", "M", "F", "X", "N/A"),
#'               c(Male = "M", Female = "F",
#'                 Refused = "X", "Not applicable" = "N/A"),
#'               c(FALSE, FALSE, TRUE, TRUE))
#' x
#' get_na(x)
#' to_na(x)
#'
#' # create labelled integer, with missing flag
#' x <- labelled(c(1, 2, 1, 3, 4, 1),
#'              c(Male = 1, Female = 2, Refused = 3, "N/A" = 4),
#'              c(FALSE, FALSE, TRUE, TRUE))
#' x
#' get_na(x)
#' to_na(x)
#'
#' # get summary
#' x <- labelled(c(1, 2, 1, 3, 4, 1, NA, 5),
#'               c(Male = 1, Female = 2, Refused = 5),
#'               c(FALSE, FALSE, TRUE))
#' summary(x)
#'
#' @export
to_na <- function(x) {
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # na all
    for (i in 1:nvars) x[[i]] <- to_na_helper(x[[i]])
    return(x)
  } else {
    return(to_na_helper(x))
  }
}

to_na_helper <- function(x) set_na(x, suppressMessages(get_na(x)), as.attr = FALSE)


#' @title Add missing value labels to partially labelled vector
#' @name fill_labels
#'
#' @description This function adds value labels to a partially labelled vector,
#'                i.e. if not all values are labelled, non-labelled values
#'                get labels.
#'
#' @param x a variable (vector), \code{data.frame} or \code{list} of variables
#'          with partially added value labels (see \code{\link[haven]{labelled}}).
#' @return \code{x}, where labels for non-labelled values are added
#'
#' @examples
#' library(haven)
#'
#' # create labelled integer, with missing flag
#' x <- labelled(c(1, 2, 1, 3, 4, 1, 5),
#'               c(Good = 1, Bad = 5))
#' get_labels(x)
#' get_labels(x, include.non.labelled = TRUE)
#'
#' fill_labels(x)
#' get_labels(fill_labels(x))
#'
#' # create partially labelled vector with missings
#' x <- labelled(c(1, 2, 1, 3, 4, 1, 5),
#'               c(Male = 1, Female = 2, Refused = 5),
#'               c(FALSE, FALSE, TRUE))
#' x
#' fill_labels(x)
#' get_labels(fill_labels(x))
#'
#' # get summary
#' x <- labelled(c(1, 2, 1, 3, 4, 1, NA, 5),
#'               c(Male = 1, Female = 2, Refused = 5),
#'               c(FALSE, FALSE, TRUE))
#' summary(x)
#'
#' @export
fill_labels <- function(x) {
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # na all
    for (i in 1:nvars) x[[i]] <- fill_labels_helper(x[[i]])
    return(x)
  } else {
    return(fill_labels_helper(x))
  }
}

fill_labels_helper <- function(x) {
  # get current labels
  current.values <- get_labels(x, attr.only = T, include.non.labelled = F)
  # get all labels, including non-labelled values
  all.values <- get_labels(x, attr.only = T, include.non.labelled = T)
  # have any values?
  if (!is.null(all.values)) {
    # get missing values
    missings <- getNaFromAttribute(x)
    # create new missing vector
    all.missings <- rep(FALSE, length(all.values))
    # "insert" former missings into new missing vector
    if (!is.null(missings)) all.missings[match(current.values, all.values)] <- missings
    # set back all labels
    x <- set_labels(x, all.values, force.labels = T)
    # set back missing information
    x <- set_na(x, all.missings, as.attr = T)
  }
  return(x)
}
