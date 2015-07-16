#' @title Attach variable label(s) to variables
#' @name set_label
#' @description This function sets variable labels to a single variable or to
#'                a set of variables in a \code{data.frame} or \code{list}-object.
#'                To each variable, the attribute \code{"label"} or \code{"variable.label"}
#'                with the related variable name is attached. Most functions of the
#'                \emph{sjPlot} package can automatically retrieve the variable
#'                name to use it as axis labels or plot title (see 'Details').
#'
#' @seealso The sjPlot manual on \href{http://www.strengejacke.de/sjPlot/datainit/}{data initialization} or
#'            \href{http://www.strengejacke.de/sjPlot/view_spss/}{inspecting (SPSS imported) data frames} for
#'            more details; \code{\link{set_labels}} to manually set value labels or \code{\link{get_label}}
#'            to get variable labels.
#'
#' @param x variable (vector), \code{list} of variables or a \code{data.frame}
#'          where variables labels should be attached.
#' @param lab if \code{x} is a vector (single variable), use a single character string with
#'          the variable label for \code{x}. If \code{x} is a data frame, use a
#'          vector with character labels of same length as \code{ncol(x)}.
#'          Use \code{lab = ""} to remove labels-attribute from \code{x}, resp.
#'          set any value of vector \code{lab} to \code{""} to remove specific variable
#'          label attributes from a data frame's variable.
#' @param attr.string attribute string for the variable label. \strong{Note:}
#'          Usually, this argument should be ignored. It is only used internally
#'          for the \code{\link{write_spss}} and \code{\link{write_stata}} functions.
#' @return \code{x}, with attached variable label attribute(s), which contains the
#'           variable name(s); or with removed label-attribute if
#'            \code{lab = ""}.
#'
#' @details See 'Details' in \code{\link{get_labels}}
#'
#' @note See 'Note' in \code{\link{get_labels}}
#'
#' @examples
#' # sample data set, imported from SPSS.
#' data(efc)
#'
#' \dontrun{
#' library(sjPlot)
#' sjt.frq(efc$e42dep)
#' sjt.frq(data.frame(efc$e42dep, efc$e16sex))}
#'
#'
#' # manually set value and variable labels
#' dummy <- sample(1:4, 40, replace=TRUE)
#' dummy <- set_labels(dummy, c("very low", "low", "mid", "hi"))
#' dummy <- set_label(dummy, "Dummy-variable")
#' # auto-detection of value labels by default, auto-detection of
#' # variable labels if argument "title" set to NULL.
#' \dontrun{
#' library(sjPlot)
#' sjp.frq(dummy, title = NULL)}
#'
#'
#' # Set variable labels for data frame
#' dummy <- data.frame(a = sample(1:4, 10, replace = TRUE),
#'                     b = sample(1:4, 10, replace = TRUE),
#'                     c = sample(1:4, 10, replace = TRUE))
#' dummy <- set_label(dummy,
#'                    c("Variable A",
#'                      "Variable B",
#'                      "Variable C"))
#' str(dummy)
#'
#' # remove one variable label
#' dummy <- set_label(dummy,
#'                    c("Variable A",
#'                      "",
#'                      "Variable C"))
#' str(dummy)
#'
#'
#' # setting same variable labels to multiple vectors
#'
#' # create a set of dummy variables
#' dummy1 <- sample(1:4, 40, replace=TRUE)
#' dummy2 <- sample(1:4, 40, replace=TRUE)
#' dummy3 <- sample(1:4, 40, replace=TRUE)
#' # put them in list-object
#' dummies <- list(dummy1, dummy2, dummy3)
#' # and set variable labels for all three dummies
#' dummies <- set_label(dummies, c("First Dummy", "2nd Dummy", "Third dummy"))
#' # see result...
#' get_label(dummies)
#'
#' @export
set_label <- function(x, lab, attr.string = NULL) {
  # ----------------------------
  # auto-detect variable label attribute
  # ----------------------------
  if (is.null(attr.string)) attr.string <- getVarLabelAttribute(x)
  # still nothing found? then leave...
  if (is.null(attr.string)) attr.string <- "label"
  # ----------------------------
  # do we have all necessary arguments?
  # ----------------------------
  if (!is.null(lab) && !is.null(x)) {
    # ----------------------------
    # if we have a data frame, we need a variable label
    # for each column (variable) of the data frame
    # ----------------------------
    if (is.data.frame(x) || is.list(x)) {
      # ----------------------------
      # get length of data frame or list, i.e.
      # determine number of variables
      # ----------------------------
      if (is.data.frame(x))
        nvars <- ncol(x)
      else
        nvars <- length(x)
      # ----------------------------
      # check for matching length of supplied labels
      # ----------------------------
      if (nvars != length(lab)) {
        message("argument \"lab\" must be of same length as numbers of columns in \"x\".")
      } else {
        # ----------------------------
        # do we have a data frame? If yes, save column names
        # ----------------------------
        if (is.data.frame(x)) cnames <- colnames(x)
        # ----------------------------
        # iterate all columns / list elements
        # ----------------------------
        for (i in 1:nvars) {
          if (is_empty(lab[i])) {
            # ----------------------------
            # empty label value means, remove
            # the label attribute
            # ----------------------------
            attr(x[[i]], attr.string) <- NULL
          } else {
            # ----------------------------
            # set variable label
            # ----------------------------
            attr(x[[i]], attr.string) <- lab[i]
            # ----------------------------
            # set names attribute. equals variable name
            # ----------------------------
            if (is.data.frame(x)) names(attr(x[[i]], attr.string)) <- cnames[i]
          }
        }
      }
    } else {
      if (is_empty(lab))
        # empty label, so remove label attribute
        attr(x, attr.string) <- NULL
      else
        # set label attribute
        attr(x, attr.string) <- lab
    }
  }
  return(x)
}


#' @name set_var_labels
#' @rdname set_label
#' @export
set_var_labels <- function(x, lab, attr.string = NULL) {
  .Deprecated("set_label")
  return(set_label(x, lab, attr.string))
}


#' @title Attach value labels to variables
#' @name set_labels
#'
#' @description This function adds character labels as attribute to a variable
#'                or vector \code{x}, resp. to a set of variables in a
#'                \code{data.frame} or \code{list}-object. To each variable,
#'                the attribute \code{"labels"} or \code{"value.labels"}
#'                with the related \code{labels} is added. These value labels will be accessed
#'                by functions of the \emph{sjPlot} package, in order to automatically set values
#'                or legend labels.
#'
#' @seealso The sjPlot manual on \href{http://www.strengejacke.de/sjPlot/datainit/}{data initialization} or
#'            \href{http://www.strengejacke.de/sjPlot/view_spss/}{inspecting (SPSS imported) data frames} for
#'            more details; \code{\link{set_label}} to manually set variable labels or
#'            \code{\link{get_label}} to get variable labels; \code{\link{add_labels}} to
#'            add additional value labels without replacing the existing ones.
#'
#' @param x variable (vector), \code{list} of variables or a \code{data.frame}
#'          where value label attributes should be added. Replaces former value labels.
#' @param labels (named) character vector of labels that will be added to \code{x} as
#'          \code{"labels"} or \code{"value.labels"} attribute. If \code{labels} is
#'          \strong{not} a named vector, its length must equal the value range of
#'          \code{x}, i.e. if \code{x} has values from 1 to 3, \code{labels} should have a length of 3.
#'          If \code{labels} is a named vector, value labels will be set accordingly, even
#'          if \code{x} has a different length of unique values. See also 'Note'.
#'          If \code{x} is a data frame, \code{labels} may also be a \code{\link{list}} of
#'          (named) character vectors. If \code{labels} is a list, it must have the same length as
#'          number of columns of \code{x}. If \code{labels} is a vector and \code{x} is a data frame,
#'          the \code{labels} will be applied to each column of \code{x}.
#'          Use \code{labels = ""} to remove labels-attribute from \code{x}.
#' @param force.labels logical; if \code{TRUE}, all \code{labels} are added as value label
#'          attribute, even if \code{x} has less unique values then length of \code{labels}
#'          or if \code{x} has a smaller range then length of \code{labels}. See 'Examples'.
#'          This parameter will be ignored, if \code{labels} is a named vector.
#' @param force.values logical, if \code{TRUE} (default) and \code{labels} has less
#'          elements than unique values of \code{x}, additional values not covered
#'          by \code{labels} will be added as label as well. See 'Examples'.
#'          This parameter will be ignored, if \code{labels} is a named vector.
#' @return \code{x} with value label attributes; or with removed label-attributes if
#'            \code{labels = ""}.
#'
#' @details See 'Details' in \code{\link{get_labels}}.
#'
#' @note If \code{labels} is a named vector, \code{force.labels} and
#'         \code{force.values} will be ignored. Furthermore,. see 'Note'
#'         in \code{\link{get_labels}}.
#'
#' @examples
#' \dontrun{
#' library(sjPlot)
#' dummy <- sample(1:4, 40, replace = TRUE)
#' sjp.frq(dummy)
#'
#' dummy <- set_labels(dummy, c("very low", "low", "mid", "hi"))
#' sjp.frq(dummy)}
#'
#' # force using all labels, even if not all labels
#' # have associated values in vector
#' x <- c(2, 2, 3, 3, 2)
#' # only two value labels
#' x <- set_labels(x, c("1", "2", "3"))
#' x
#' \dontrun{
#' sjp.frq(x)}
#' # all three value labels
#' x <- set_labels(x, c("1", "2", "3"), force.labels = TRUE)
#' x
#' \dontrun{
#' sjp.frq(x)}
#'
#' # create vector
#' x <- c(1, 2, 3, 2, 4, NA)
#' # add less labels than values
#' x <- set_labels(x, c("yes", "maybe", "no"), force.values = FALSE)
#' x
#' # add all necessary labels
#' x <- set_labels(x, c("yes", "maybe", "no"), force.values = TRUE)
#' x
#'
#' # set labels and missings
#' x <- c(1, 1, 1, 2, 2, -2, 3, 3, 3, 3, 3, 9)
#' x <- set_labels(x, c("Refused", "One", "Two", "Three", "Missing"))
#' x
#'
#' x <- set_na(x, c(-2, 9), as.attr = TRUE)
#' x
#' summary(as_labelled(x))
#'
#'
#' # set labels via named vector,
#' # not using all possible values
#' data(efc)
#' get_labels(efc$e42dep)
#'
#' x <- set_labels(efc$e42dep, c(`1` = "independent",
#'                               `4` = "severe dependency",
#'                               `9` = "missing value"))
#' get_labels(x, attr.only = TRUE, include.values = "p")
#'
#'
#' # setting same value labels to multiple vectors
#' # create a set of dummy variables
#' dummy1 <- sample(1:4, 40, replace = TRUE)
#' dummy2 <- sample(1:4, 40, replace = TRUE)
#' dummy3 <- sample(1:4, 40, replace = TRUE)
#' # put them in list-object
#' dummies <- list(dummy1, dummy2, dummy3)
#' # and set same value labels for all three dummies
#' dummies <- set_labels(dummies, c("very low", "low", "mid", "hi"))
#' # see result...
#' get_labels(dummies)
#'
#' @export
set_labels <- function(x,
                           labels,
                           force.labels = FALSE,
                           force.values = TRUE) {
  return(set_labels_helper(x, labels, force.labels, force.values))
}


#' @name set_val_labels
#' @rdname set_labels
#' @export
set_val_labels <- function(x,
                       labels,
                       force.labels = FALSE,
                       force.values = TRUE) {
  .Deprecated("set_labels")
  return(set_labels(x, labels, force.labels, force.values))
}


set_labels_helper <- function(x, labels, force.labels, force.values) {
  # ---------------------------------------
  # any valid labels? if not, return vector
  # ---------------------------------------
  if (is.null(labels)) return(x)
  # ---------------------------------------
  # convert single vector
  # ---------------------------------------
  if (!is.list(x) && (is.vector(x) || is.atomic(x))) {
    return(set_values_vector(x,
                             labels,
                             NULL,
                             force.labels,
                             force.values))
  } else if (is.data.frame(x) || is.matrix(x) || is.list(x)) {
    # ---------------------------------------
    # get length of data frame or list, i.e.
    # determine number of variables
    # ---------------------------------------
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    for (i in 1:nvars) {
      # ---------------------------------------
      # list of labels makes sense if multiple variable
      # should be labelled with different labels
      # ---------------------------------------
      if (is.list(labels)) {
        # ---------------------------------------
        # check for valid length of supplied label-list
        # ---------------------------------------
        if (i <= length(labels)) {
          x[[i]] <- set_values_vector(x[[i]],
                                      labels[[i]],
                                      colnames(x)[i],
                                      force.labels,
                                      force.values)
        }
      } else if (is.vector(labels)) {
        # ---------------------------------------
        # user supplied only one vector of labels.
        # so each variable gets the same labels
        # ---------------------------------------
        x[[i]] <- set_values_vector(x[[i]],
                                    labels,
                                    colnames(x)[i],
                                    force.labels,
                                    force.values)
      } else {
        warning("'labels' must be a list of same length as 'ncol(x)' or a vector.", call. = F)
      }
    }
    return(x)
  }
}


#' @importFrom stats na.omit
get_value_range <- function(x) {
  # ---------------------------------------
  # check if var is a factor
  # ---------------------------------------
  if (is.factor(x)) {
    # ---------------------------------------
    # check if we have numeric levels
    # ---------------------------------------
    if (!is_num_fac(x)) {
      # ---------------------------------------
      # retrieve levels. since levels are numeric, we
      # have minimum and maximum values
      # ---------------------------------------
      minval <- 1
      maxval <- length(levels(x))
    } else {
      # ---------------------------------------
      # levels are not numeric. we need to convert them
      # first to retrieve minimum level, as numeric
      # ---------------------------------------
      minval <- min(as.numeric(levels(x)), na.rm = T)
      # ---------------------------------------
      # check range, add minimum, so we have max
      # ---------------------------------------
      maxval <- diff(range(as.numeric(levels(x)))) + minval
    }
  } else if (is.character(x)) {
    # ---------------------------------------
    # if we have a character vector, we don't have
    # min and max values. instead, we count the
    # amount of unique string values
    # ---------------------------------------
    minval <- 1
    maxval <- length(unique(stats::na.omit(x)))
  } else {
    # retrieve values
    minval <- min(x, na.rm = TRUE)
    maxval <- max(x, na.rm = TRUE)
  }
  # determine value range
  valrange <- maxval - minval + 1
  # return all
  return(list(minval = minval,
              maxval = maxval,
              valrange = valrange))
}


#' @importFrom stats na.omit
set_values_vector <- function(x, labels, var.name, force.labels, force.values) {
  # ---------------------------------------
  # auto-detect variable label attribute
  # ---------------------------------------
  attr.string <- getValLabelAttribute(x)
  # do we have any label attributes?
  if (is.null(attr.string)) attr.string <- "labels"
  # check for null
  if (!is.null(labels)) {
    # ---------------------------------------
    # if labels is empty string, remove labels
    # attribute
    # ---------------------------------------
    if (length(labels) == 1 && nchar(labels) == 0) {
      attr(x, attr.string) <- NULL
    } else if (is.null(x) || is.character(x)) {
      # ---------------------------------------
      # string variables can't get value labels
      # ---------------------------------------
      warning("can't attach value labels to string or NULL vectors.", call. = F)
    } else {
      # ---------------------------------------
      # determine value range
      # ---------------------------------------
      vr <- get_value_range(x)
      # copy values to variables
      valrange <- vr$valrange
      minval <- vr$minval
      maxval <- vr$maxval
      # check for unlisting
      if (is.list(labels)) labels <- unlist(labels)
      # ---------------------------------------
      # determine amount of labels and unique values
      # ---------------------------------------
      lablen <- length(labels)
      values <- sort(unique(stats::na.omit(as.vector(x))))
      # ---------------------------------------
      # set var name string
      # ---------------------------------------
      if (is.null(var.name) || nchar(var.name) < 1) {
        name.string <- "x"
      } else {
        name.string <- var.name
      }
      if (is.infinite(valrange)) {
        warning("can't set value labels. Infinite value range.", call. = F)
      # ---------------------------------------
      # check if we have named vector. in this
      # case, just add these values
      # ---------------------------------------
      } else if (!is.null(names(labels))) {
        # ---------------------------------------
        # set attributes
        # ---------------------------------------
        attr(x, attr.string) <- names(labels)
        names(attr(x, attr.string)) <- labels
      # ---------------------------------------
      # check for valid length of labels
      # if amount of labels and values are equal,
      # we assume matching labels
      # ---------------------------------------
      } else if (length(values) == lablen) {
        # ---------------------------------------
        # set attributes
        # ---------------------------------------
        attr(x, attr.string) <- as.character(values)
        names(attr(x, attr.string)) <- labels
      # ---------------------------------------
      # check for valid length of labels
      # here, we have a smaller value range (i.e. less values)
      # than amount of labels
      # ---------------------------------------
      } else if (valrange < lablen) {
        # ---------------------------------------
        # do we want to force to set labels, even if we have more labels
        # than values in variable?
        # ---------------------------------------
        if (force.labels) {
          attr(x, attr.string) <- as.character(c(1:lablen))
          names(attr(x, attr.string)) <- labels
        } else {
          # ---------------------------------------
          # we have more labels than values, so just take as many
          # labes as values are present
          # ---------------------------------------
          message(sprintf("More labels than values of \"%s\". Using first %i labels.", name.string, valrange))
          attr(x, attr.string) <- as.character(c(minval:maxval))
          names(attr(x, attr.string)) <- labels[1:valrange]
        }
      # ---------------------------------------
      # value range is larger than amount of labels. we may
      # have not continuous value range, e.g. "-2" as filter and
      # 1 to 4 as valid values, i.e. -1 and 0 are missing
      # ---------------------------------------
      } else if (valrange > lablen) {
        # ---------------------------------------
        # check if user wants to add missing values
        # ---------------------------------------
        if (force.values) {
          # get amount of unique values
          valrange <- length(values)
          # ---------------------------------------
          # still no match?
          # ---------------------------------------
          if (valrange != lablen) {
            # ---------------------------------------
            # check which one is longer, and get missing values
            # ---------------------------------------
            add_values <- ifelse(valrange > lablen, valrange[-lablen], lablen[-valrange])
            # add missing values to labels
            labels <- c(labels, as.character(add_values))
            # tell user about modification
            message(sprintf("More values in \"%s\" than length of \"labels\". Additional values were added to labels.", name.string))
          }
          # ---------------------------------------
          # set attributes
          # ---------------------------------------
          attr(x, attr.string) <- as.character(c(1:valrange))
          names(attr(x, attr.string)) <- labels
        } else {
          # ---------------------------------------
          # tell user about modification
          # ---------------------------------------
          message(sprintf("\"%s\" has more values than \"labels\", hence not all values are labelled.", name.string))
          # drop values with no associated labels
          attr(x, attr.string) <- as.character(c(1:length(labels)))
          names(attr(x, attr.string)) <- labels
        }
      } else {
        attr(x, attr.string) <- as.character(c(minval:maxval))
        names(attr(x, attr.string)) <- labels
      }
    }
  }
  return(x)
}


#' @title Add value labels to variables
#' @name add_labels
#'
#' @description This function adds additional labels as attribute to a variable
#'                or vector \code{x}, resp. to a set of variables in a
#'                \code{data.frame} or \code{list}-object. Unlike \code{\link{set_labels}},
#'                \code{add_labels} does not replace existing value labels, but add
#'                \code{labels} to the existing value labels of \code{x}.
#'
#' @seealso \code{\link{set_label}} to manually set variable labels or
#'            \code{\link{get_label}} to get variable labels; \code{\link{set_labels}} to
#'            add value labels, replacing the existing ones.
#'
#' @param x variable (vector), \code{list} of variables or a \code{data.frame}
#'          where value label attributes should be added. Does not replaces former
#'          value labels.
#' @param labels named character vector of labels that will be added to \code{x} as
#'          \code{"labels"} or \code{"value.labels"} attribute. If \code{x} is
#'          a data frame, \code{labels} may also be a \code{\link{list}} of
#'          named character vectors. If \code{labels} is a list, it must have
#'          the same length as number of columns of \code{x}. If \code{labels}
#'          is a vector and \code{x} is a data frame, \code{labels} will be applied
#'          to each column of \code{x}.
#'
#' @return \code{x} with additional value labels.
#'
#' @note Existing labelled values will be replaced by new labelled values
#'         in \code{labels}. See 'Examples'.
#'
#' @examples
#' data(efc)
#' get_labels(efc$e42dep)
#'
#' x <- add_labels(efc$e42dep, c(`5` = "nothing"))
#' get_labels(x)
#'
#' x <- add_labels(efc$e42dep, c(`5` = "nothing", `0` = "zero value"))
#' get_labels(x, include.values = "p")
#'
#' # replace old values
#' x <- add_labels(efc$e42dep, c(`4` = "not so dependent", `5` = "lorem ipsum"))
#' get_labels(x, include.values = "p")
#'
#'
#' @export
add_labels <- function(x, labels) {
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # dichotomize all
    for (i in 1:nvars) x[[i]] <- add_labels_helper(x[[i]], labels)
    return(x)
  } else {
    return(add_labels_helper(x, labels))
  }
}


add_labels_helper <- function(x, labels) {
  # ----------------------------------------
  # get current labels of `x`
  # ----------------------------------------
  current.labels <- get_labels(x,
                               attr.only = T,
                               include.values = "n",
                               include.non.labelled = F)
  # ----------------------------------------
  # if we had already labels, append new ones
  # ----------------------------------------
  if (!is.null(current.labels)) {
    # -------------------------
    # remove double values labels
    # -------------------------
    doubles <- names(current.labels) %in% names(labels)
    # -------------------------
    # update all labels
    # -------------------------
    all.labels <- c(current.labels[!doubles], labels)
    # -------------------------
    # tell user
    # -------------------------
    warning(sprintf("label '%s' was replaced with new value label.", current.labels[doubles]), call. = F)
  } else {
    all.labels <- labels
  }
  # ----------------------------------------
  # sort labels by values
  # ----------------------------------------
  all.labels <- all.labels[order(names(all.labels))]
  # ----------------------------------------
  # set back labels
  # ----------------------------------------
  x <- set_labels(x, labels = all.labels)
  return(x)
}


#' @title Set NA for specific variable values
#' @name set_na
#'
#' @description This function sets specific values of a variable, data frame
#'                or list of variables as missings (\code{NA}).
#'
#' @seealso \code{\link{replace_na}} to replace \code{\link{NA}}'s with specific
#'            values, \code{\link{rec}} for general recoding of variables and
#'            \code{\link{recode_to}} for re-shifting value ranges. See
#'            \code{\link{get_na}} to get values of missing values in
#'            labelled vectors and \code{\link{to_na}} to convert missing value
#'            codes into \code{NA}.
#'
#' @param x variable (vector), \code{data.frame} or \code{list} of variables where new
#'          missing values should be defined. If \code{x} is a \code{data.frame}, each
#'          column is assumed to be a new variable, where missings should be defined.
#' @param values numeric vector with values that should be replaced with \code{\link{NA}}'s.
#'          Thus, for each variable in \code{x}, \code{values} are replaced by \code{NA}'s.
#'          Or: a logical vector describing which values should be translated
#'          to missing values. See 'Details' and 'Examples'.
#' @param as.attr logical, if \code{TRUE}, \code{values} of \code{x} will \strong{not}
#'          be converted to \code{NA}. Rather, the missing code values of \code{values}
#'          will be added as missing-attribute \code{is_na} to the vector. See
#'          \code{\link[haven]{labelled}} for more details, and 'Examples'.
#'
#' @return \code{x}, where each value of \code{values} is replaced by an \code{NA}.
#'
#' @note Value and variable label attributes (see, for instance, \code{\link{get_labels}}
#'         or \code{\link{set_labels}}) are preserved.
#'
#' @details \code{set_na} converts those values to \code{NA} that are
#'            specified in the function's \code{values} argument; hence,
#'            by default, \code{set_na} ignores any missing code attributes
#'            like \code{is_na}. \code{\link{to_na}}, by contrast, converts
#'            values to \code{NA}, which are defined as missing through the
#'            \code{is_na}-attribute of a vector (see \code{\link[haven]{labelled}}).
#'            \cr \cr
#'            If \code{as.attr = TRUE}, \code{values} in \code{x} will \strong{not}
#'            be converted to \code{NA}. Instead, the attribute \code{is_na}
#'            will be added to \code{x}, indicating which values should be coded
#'            as missing. \code{values} may either be numeric, with each number
#'            indicating a value that should be defined as missing; or a vector
#'            of logicals, describing which values should be translated to
#'            missing values (see 'Examples').
#'            \cr \cr
#'            Furthermore, see 'Details' in \code{\link{get_na}}.
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
#' # create random variable
#' dummy <- sample(1:5, 100, replace = TRUE)
#' # declare missing values, but only as attribute
#' dummy <- set_na(dummy, c(3, 5), as.attr = TRUE)
#'
#' str(dummy)
#' table(dummy)
#' get_na(dummy)
#'
#' # create random variable
#' dummy <- sample(1:5, 100, replace = TRUE)
#' # declare missing values, but only as attribute
#' # missing code definition may be logical indices
#' dummy <- set_na(dummy,
#'                 c(FALSE, FALSE, FALSE, TRUE, TRUE),
#'                 as.attr = TRUE)
#'
#' str(dummy)
#' table(dummy)
#' get_na(dummy)
#'
#' @export
set_na <- function(x, values, as.attr = FALSE) {
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # dichotomize all
    for (i in 1:nvars) x[[i]] <- set_na_helper(x[[i]], values, as.attr)
    return(x)
  } else {
    return(set_na_helper(x, values, as.attr))
  }
}


#' @importFrom stats na.omit
set_na_helper <- function(x, values, as.attr = FALSE) {
  # ----------------------------
  # does user want to add missing codes as is_na attribute?
  # if yes, do so here...
  # ----------------------------
  if (as.attr) {
    x <- set_na_attr(x, values)
  } else {
    # ----------------------------
    # check if we have any values at all?
    # ----------------------------
    if (is.null(values)) return(x)
    # ----------------------------
    # auto-detect variable label attribute
    # ----------------------------
    attr.string <- getValLabelAttribute(x)
    # check if x has label attributes
    if (!is.null(attr.string)) {
      # retrieve value labels
      vl <- attr(x, attr.string, exact = T)
      # retrieve label names
      ln <- names(vl)
    } else {
      # ---------------------------------------
      # if x has no label attributes, use values
      # as labels
      # ---------------------------------------
      vl <- as.character(sort(unique(stats::na.omit(x))))
      ln <- vl
    }
    # ---------------------------------------
    # find associated values in x
    # and set them to NA
    # ---------------------------------------
    x[x %in% values] <- NA
    # ---------------------------------------
    # check if value labels exist, and if yes, remove them
    # ---------------------------------------
    labelpos <- suppressWarnings(which(as.numeric(vl) %in% values))
    # ---------------------------------------
    # remove NA label
    # ---------------------------------------
    if (length(labelpos > 0)) {
      vl <- vl[-labelpos]
      ln <- ln[-labelpos]
    } else {
      # ---------------------------------------
      # if vl were not numeric convertable, try character conversion
      # check if value labels exist, and if yes, remove them
      # ---------------------------------------
      labelpos <- suppressWarnings(which(as.character(vl) %in% values))
      # remove NA label
      if (length(labelpos > 0)) {
        vl <- vl[-labelpos]
        ln <- ln[-labelpos]
      }
    }
    # ---------------------------------------
    # set back updated label attribute
    # ---------------------------------------
    if (!is.null(attr.string)) {
      # ---------------------------------------
      # do we have any labels left?
      # ---------------------------------------
      if (length(vl) > 0) {
        # if yes, set back label attribute
        attr(x, attr.string) <- vl
        names(attr(x, attr.string)) <- ln
        # ---------------------------------------
        # shorten is_na attribute
        # ---------------------------------------
        na.flag <- get_na_flags(x)
        if (!is.null(na.flag)) {
          # ---------------------------------------
          # do we have is_na attribute? if yes,
          # shorten it to length of labels
          # ---------------------------------------
          na.flag <- na.flag[1:length(vl)]
          attr(x, getNaAttribute()) <- na.flag
        }
      } else {
        # ---------------------------------------
        # else remove attribute
        # ---------------------------------------
        attr(x, attr.string) <- NULL
        # remove is_na attribute, no longer needed
        attr(x, getNaAttribute()) <- NULL
        # ---------------------------------------
        # unclass labelled, because it may result
        # in errors when printing a labelled-class-vector
        # without labelled and is_na attribute
        # ---------------------------------------
        if (is_labelled(x)) x <- unclass(x)
      }
    }
  }
  return(x)
}


set_na_attr <- function(x, na.values) {
  # get values
  all.values <- get_values(x, sort.val = FALSE, drop.na = FALSE)
  # do we have value attributes?
  if (is.null(all.values)) {
    # we assume a simple numeric vector, so let's add
    # some label attributes
    all.values <- sort(unique(stats::na.omit(x)))
    x <- set_labels(x, as.character(all.values))
  }
  if (is.null(na.values)) {
    # is na.values NULL? Then set FALSE (no missing)
    # for all value codes
    na.values <- rep(FALSE, length(all.values))
  } else if (!is.logical(na.values)) {
    # if we do not have logical indices,
    # set TRUE for all NA-codes and FALSE for all other
    na.values <- !is.na(match(all.values, na.values))
  }
  # same length?
  if (length(na.values) != length(all.values))
    # If not, warn user
    warning("Length of logical indices for missing codes did not match length of values.", call. = F)
  # set is_na attribute
  attr(x, getNaAttribute()) <- na.values
  return(x)
}
