#' @title Retrieve variable label(s) of a data frame or variable
#' @name get_var_labels
#'
#' @description This function retrieves the variable labels of an imported
#'                SPSS, SAS or STATA data set (via \code{\link{read_spss}},
#'                \code{\link{read_sas}} or \code{\link{read_stata}}) and
#'                \itemize{
#'                  \item if \code{x} is a data frame or a list of variables, returns the all variable labels as names character vector of length \code{ncol(x)}.
#'                  \item or, if \code{x} is a vector, returns the variable label as string.
#'                  }
#'
#' @seealso The sjPlot manual on \href{http://www.strengejacke.de/sjPlot/datainit/}{data initialization} or
#'            \href{http://www.strengejacke.de/sjPlot/view_spss/}{inspecting (SPSS imported) data frames} for
#'            more details; \code{\link{set_label}} to manually set variable labels or \code{\link{get_labels}}
#'            to get value labels.

#' @param x a \code{data.frame} with variables that have attached variable labels (e.g.
#'          from an imported SPSS, SAS or STATA data set, via \code{\link{read_spss}},
#'          \code{\link{read_sas}} or \code{\link{read_stata}}); a variable
#'          (vector) with attached variable label; or a \code{list} of variables
#'          with attached variable labels. See 'Examples'.
#'
#' @return A named char vector with all variable labels from the data frame or list;
#'           or a simple char vector (of length 1) with the variable label, if \code{x} is a variable.
#'
#' @details See 'Details' in \code{\link{get_labels}}.
#'
#' @note See 'Note' in \code{\link{get_labels}}.
#'
#' @examples
#' # import SPSS data set
#' # mydat <- read_spss("my_spss_data.sav", enc="UTF-8")
#'
#' # retrieve variable labels
#' # mydat.var <- get_label(mydat)
#'
#' # retrieve value labels
#' # mydat.val <- get_labels(mydat)
#'
#' data(efc)
#'
#' # get variable lable
#' get_label(efc$e42dep)
#'
#' # alternative way
#' get_label(efc)["e42dep"]
#'
#' # simple barplot
#' barplot(table(efc$e42dep))
#' # get value labels to annotate barplot
#' barplot(table(efc$e42dep),
#'         names.arg = get_labels(efc$e42dep),
#'         main = get_label(efc$e42dep))
#'
#' # get labels from multiple variables
#' get_label(list(efc$e42dep,
#'                efc$e16sex,
#'                efc$e15relat))
#'
#' @export
get_var_labels <- function(x) {
  # .Deprecated("get_label")
  # ----------------------------
  # auto-detect variable label attribute
  # ----------------------------
  attr.string <- getVarLabelAttribute(x)
  # do we have a df?
  if (is.data.frame(x) || is.matrix(x)) {
    # if yes, check if we have attached label table
    # from foreign import
    labels <- attr(x, "variable.labels", exact = T)
    # if not, get labels from each single vector
    if (is.null(labels) && !is.null(attr.string)) {
      # return value
      all.labels <- c()
      # iterate df
      for (i in 1:ncol(x)) {
        # get label
        label <- attr(x[[i]], attr.string, exact = T)
        # any label?
        if (!is.null(label)) {
          # name label
          names(label) <- colnames(x)[i]
          # append to return result
          all.labels <- c(all.labels, label)
        } else {
          all.labels <- c(all.labels, "")
        }
      }
      return(all.labels)
    } else {
      return(attr(x, "variable.labels", exact = T))
    }
  } else if (is.list(x)) {
    # nothing found? then leave...
    if (is.null(attr.string)) return(NULL)
    # return attribute of all variables
    return(unlist(lapply(x, attr, attr.string, exact = T)))
  } else {
    # nothing found? then leave...
    if (is.null(attr.string)) return(NULL)
    # else return attribute
    return(attr(x, attr.string, exact = T))
  }
}


#' @name get_label
#' @rdname get_var_labels
#' @export
get_label <- function(x) {
  return(get_var_labels(x))
}


#' @title Retrieve value labels of a data frame or variable
#' @name get_val_labels
#'
#' @description This function retrieves the value labels of an imported
#'                SPSS, SAS or STATA data set (via \code{\link{read_spss}},
#'                \code{\link{read_sas}} or \code{\link{read_stata}}) and
#'                \itemize{
#'                  \item if \code{x} is a data frame or list of variables, returns the all variable's value labels as \code{\link{list}}
#'                  \item or, if \code{x} is a vector, returns the label as string.
#'                  }
#'
#' @seealso The sjPlot manual on \href{http://www.strengejacke.de/sjPlot/datainit/}{data initialization},
#'            \href{http://www.strengejacke.de/sjPlot/labelleddata/}{working with labelled data} or
#'            \href{http://www.strengejacke.de/sjPlot/view_spss/}{inspecting (SPSS imported) data frames} for
#'            more details; \code{\link{set_labels}} to manually set value labels, \code{\link{get_label}}
#'            to get variable labels and \code{\link{get_values}} to retrieve value label associated values.
#'
#' @param x a \code{data.frame} with variables that have attached value labels (e.g.
#'          from an imported SPSS, SAS or STATA data set, via \code{\link{read_spss}},
#'          \code{\link{read_sas}} or \code{\link{read_stata}}); a variable
#'          (vector) with attached value labels; or a \code{list} of variables
#'          with attached values labels. If \code{x} has no label \code{\link{attributes}},
#'          factor \code{\link{levels}} are returned. See 'Examples'.
#' @param include.values string, indicating whether the values associated with the
#'          value labels are returned as well. If \code{include.values = "as.name"}
#'          (or \code{include.values = "n"}), values are set as \code{\link{names}}
#'          attribute of the returned object. If \code{include.values = "as.prefix"}
#'          (or \code{include.values = "p"}), values are includes as prefix
#'          to each label. See 'Examples'.
#' @param attr.only logical, if \code{TRUE}, labels are only searched for
#'          in the the vector's \code{\link{attributes}}; else, if \code{x} has no
#'          label attributes, factor levels or string values are returned. See
#'          'Examples'.
#' @param include.non.labelled logical, if \code{TRUE}, values without labels will
#'          also be included in the returned labels.
#' @return Either a list with all value labels from all variables if \code{x}
#'           is a \code{data.frame} or \code{list}; a string with the value
#'           labels, if \code{x} is a variable;
#'           or \code{NULL} if no value label attribute was found.
#'
#' @details This package can add (and read) value and variable labels either in \code{foreign}
#'            package style (attributes are named \emph{value.labels} and \emph{variable.label})
#'            or in \code{haven} package style (attributes are named \emph{labels} and
#'            \emph{label}). By default, the \code{haven} package style is used.
#'            \cr \cr
#'            Working with labelled data is a key element of the \code{sjPlot} package,
#'            which accesses these attributes to automatically read label attributes
#'            for labelling axis categories and titles or table rows and columns.
#'            \cr \cr
#'            When working with labelled data, you can, e.g., use
#'            \code{\link{get_label}} or \code{\link{get_labels}}
#'            to get a vector of value and variable labels, which can then be
#'            used with other functions like \code{\link{barplot}} etc.
#'            See 'Examples'.
#'            \cr \cr
#'            Furthermore, value and variable labels are used when saving data, e.g. to SPSS
#'            (see \code{\link{write_spss}}), which means that the written SPSS file
#'            contains proper labels for each variable.
#'            \cr \cr
#'            You can set a default label style (i.e. the names of the label
#'            attributes, see above) via \code{options(value_labels = "haven")}
#'            or \code{options(value_labels = "foreign")}.
#'
#' @note This function only works with vectors that have value and variable
#'        labels attached. This is automatically done by importing data sets
#'        with the \code{\link{read_spss}}, \code{\link{read_sas}} or \code{\link{read_stata}}
#'        function. Labels can also manually be added using the \code{\link{set_labels}}
#'        and \code{\link{set_label}} functions.
#'        \cr \cr
#'        With attached value and variable labels, most functions of the \code{sjPlot} package
#'        automatically detect labels and uses them as axis, legend or title labels
#'        in plots (\code{sjp.}-functions) respectively as column or row headers
#'        in table outputs (\code{sjt.}-functions).  See
#'        \href{http://www.strengejacke.de/sjPlot/datainit/}{this} and
#'        \href{http://www.strengejacke.de/sjPlot/labelleddata/}{this}
#'        online manuals for more details.
#'        \cr \cr
#'        Use \code{options(autoSetValueLabels = FALSE)}
#'        and \code{options(autoSetVariableLabels = FALSE)} to turn off automatic
#'        label detection.
#'
#' @examples
#' # import SPSS data set
#' # mydat <- read_spss("my_spss_data.sav")
#'
#' # retrieve variable labels
#' # mydat.var <- get_label(mydat)
#'
#' # retrieve value labels
#' # mydat.val <- get_labels(mydat)
#'
#' data(efc)
#' get_labels(efc$e42dep)
#'
#' # simple barplot
#' barplot(table(efc$e42dep))
#' # get value labels to annotate barplot
#' barplot(table(efc$e42dep),
#'         names.arg = get_labels(efc$e42dep),
#'         main = get_label(efc$e42dep))
#'
#' # include associated values
#' get_labels(efc$e42dep, include.values = "as.name")
#'
#' # include associated values
#' get_labels(efc$e42dep, include.values = "as.prefix")
#'
#' # get labels from multiple variables
#' get_labels(list(efc$e42dep,
#'                 efc$e16sex,
#'                 efc$e15relat))
#'
#'
#' # create a dummy factor
#' f1 <- factor(c("hi", "low", "mid"))
#' # search for label attributes only
#' get_labels(f1, attr.only = TRUE)
#' # search for factor levels as well
#' get_labels(f1)
#'
#' # same for character vectors
#' c1 <- c("higher", "lower", "mid")
#' # search for label attributes only
#' get_labels(c1, attr.only = TRUE)
#' # search for string values as well
#' get_labels(c1)
#'
#'
#' # create vector
#' x <- c(1, 2, 3, 2, 4, NA)
#' # add less labels than values
#' x <- set_labels(x, c("yes", "maybe", "no"), force.values = FALSE)
#' # get labels for labelled values only
#' get_labels(x)
#' # get labels for all values
#' get_labels(x, include.non.labelled = TRUE)
#'
#'
#' @export
get_val_labels <- function(x,
                           attr.only = FALSE,
                           include.values = NULL,
                           include.non.labelled = FALSE) {
  # .Deprecated("get_labels")
  if (is.data.frame(x) || is.matrix(x) || is.list(x)) {
    a <- lapply(x, FUN = get_labels_helper,
                attr.only,
                include.values,
                include.non.labelled)
  } else {
    a <- get_labels_helper(x,
                           attr.only,
                           include.values,
                           include.non.labelled)
  }
  return(a)
}


#' @name get_labels
#' @rdname get_val_labels
#' @export
get_labels <- function(x,
                       attr.only = FALSE,
                       include.values = NULL,
                       include.non.labelled = FALSE) {
  return(get_val_labels(x, attr.only, include.values, include.non.labelled))
}


# Retrieve value labels of a data frame or variable
# See 'get_labels'
get_labels_helper <- function(x,
                              attr.only = TRUE,
                              include.values = NULL,
                              include.non.labelled = FALSE) {
  labels <- NULL
  # haven or sjPlot?
  attr.string <- getValLabelAttribute(x)
  # nothing found? then check for factor levels
  if (is.null(attr.string)) {
    # does user want to look everywhere?
    if (!attr.only) {
      # get levels of vector
      lv <- levels(x)
      # do we have any levels?
      if (!is.null(lv)) {
        labels <- lv
      } else if (is.character(x)) {
        # finally, if we even don't have values, check for
        # character elements
        labels <- unique(x)
      }
    }
  } else {
    # retrieve named labels
    lab <- attr(x, attr.string, exact = T)
    # check if we have anything
    if (!is.null(lab)) {
      # retrieve values associated with labels
      if (is.character(x))
        values <- unname(attr(x, attr.string, exact = T))
      else
        values <- as.numeric(unname(attr(x, attr.string, exact = T)))
      # retrieve order of value labels
      reihenfolge <- order(values)
      # retrieve label values in correct order
      labels <- names(lab)[reihenfolge]
      # do we want to include non-labelled values as well?
      if (include.non.labelled) {
        # check if we have more values than labels
        vr <- get_value_range(x)
        if (vr$valrange > length(labels)) {
          # get missing values
          add_vals <- seq_len(vr$valrange)[-seq_len(length(labels))]
          # add to labels
          labels <- c(labels, as.character(add_vals))
        }
      }
      # include associated values?
      if (!is.null(include.values)) {
        # for backwards compatibility, we also accept "TRUE"
        # here we set values as names-attribute
        if ((is.logical(include.values) &&
             include.values == TRUE) ||
            include.values == "as.name" || include.values == "n") {
          names(labels) <- values[reihenfolge]
        }
        # here we include values as prefix of labels
        if (include.values == "as.prefix" || include.values == "p") {
          if (is.numeric(values))
            labels <- sprintf("[%i] %s", values[reihenfolge], labels)
          else
            labels <- sprintf("[%s] %s", values[reihenfolge], labels)
        }
      }
    }
  }
  # return them
  return(labels)
}


#' @title Retrieve values of labelled variables
#' @name get_values
#'
#' @description This function retrieves the values associated with value labels
#'                of an imported SPSS, SAS or STATA data set (via \code{\link{read_spss}},
#'                \code{\link{read_sas}} or \code{\link{read_stata}}).
#'
#' @seealso \code{\link{get_labels}} for getting value labels and \code{\link{get_na}}
#'            to get values for missing values.
#'
#' @param x a variable (vector) with attached value labels.
#' @param sort.val logical, if \code{TRUE} (default), values of associated value labels
#'          are sorted.
#' @return The values associated with value labels from \code{x},
#'           or \code{NULL} if \code{x} has no label attributes.
#'
#' @details Labelled vectors are numeric by default (when imported with read-functions
#'            like \code{\link{read_spss}}) and have variable and value labels attached.
#'            The value labels are associated with those values from the labelled vector.
#'            This function returns the values associated with the vector's value labels,
#'            which may differ from actual values in the vector (e.g. due to missings)
#'            or are not represented in sorted order.
#'
#' @examples
#' data(efc)
#' str(efc$e42dep)
#' get_values(efc$e42dep)
#' get_labels(efc$e42dep)
#'
#' @export
get_values <- function(x, sort.val = FALSE) {
  # haven or sjPlot?
  attr.string <- getValLabelAttribute(x)
  # nothing found? then leave...
  if (is.null(attr.string)) return(NULL)
  # get values
  if (is.character(x))
    values <- unname(attr(x, attr.string, exact = T))
  else
    values <- as.numeric(unname(attr(x, attr.string, exact = T)))
  # sort values
  if (sort.val) values <- sort(values)
  # return sorted
  return(values)
}


#' @title Retrieve missing values of labelled variables
#' @name get_na
#'
#' @description This function retrieves the value codes associated with missing values
#'                of variables of an imported SPSS, SAS or STATA data set (via \code{\link{read_spss}},
#'                \code{\link{read_sas}} or \code{\link{read_stata}}), where missing
#'                values have not been replaced with \code{NA}s after import.
#'
#' @seealso \code{\link{get_labels}} to get value labels, or \code{\link{get_values}}
#'            to get values associated with labels; see \code{\link{set_na}} to
#'            replace specific values with \code{NA} and \code{\link{to_na}} to
#'            convert missing value codes into \code{NA}.
#'
#' @param x a variable (vector) with attached value labels and missing values
#'          (see \code{\link[haven]{labelled}}).
#' @return The missing values associated with value labels from \code{x},
#'           or \code{NULL} if \code{x} has no missing value attribute.
#'
#' @details See 'Details' in \code{\link{get_values}}
#'
#' @examples
#' library(haven)
#'
#' # create labelled factor, with missing flag
#' x <- labelled(c("M", "M", "F", "X", "N/A"),
#'               c(Male = "M", Female = "F",
#'                 Refused = "X", "Not applicable" = "N/A"),
#'               c(FALSE, FALSE, TRUE, TRUE))
#' get_na(x)
#'
#' # create labelled integer, with missing flag
#' x <- labelled(c(1, 2, 1, 3, 4, 1),
#'              c(Male = 1, Female = 2, Refused = 3, "N/A" = 4),
#'              c(FALSE, FALSE, TRUE, TRUE))
#' get_na(x)
#'
#' @export
get_na <- function(x) {
  # get values
  values <- get_values(x, sort.val = FALSE)
  # get NA logicals
  na.flag <- attr(x, getNaAttribute(), exact = T)
  # do we have missing flag?
  if (is.null(na.flag)) {
    message("Variable has no assigned missing value codes.")
    return(NULL)
  }
  # return missing values
  return(values[na.flag])
}
