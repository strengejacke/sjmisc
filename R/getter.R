#' @title Retrieve variable label(s) of labelled data
#' @name get_label
#'
#' @description This function retrieves the value labels of labelled data, which
#'                was created with the \pkg{labelled} or \pkg{haven} package, or
#'                imported from SPSS, SAS or STATA files (via \code{\link{read_spss}},
#'                \code{\link{read_sas}} or \code{\link{read_stata}}) and
#'                \itemize{
#'                  \item if \code{x} is a data frame or a list of variables, returns the all variable labels as names character vector of length \code{ncol(x)}.
#'                  \item or, if \code{x} is a vector, returns the variable label as string.
#'                  }
#'
#' @seealso See package vignettes or \href{http://www.strengejacke.de/sjPlot/}{online documentation}
#'            for more details; \code{\link{set_label}} to manually set variable labels or \code{\link{get_labels}}
#'            to get value labels.

#' @param x \code{data.frame} with variables that have label attributes (e.g.
#'          from an imported SPSS, SAS or STATA data set, via \code{\link{read_spss}},
#'          \code{\link{read_sas}} or \code{\link{read_stata}}); a variable
#'          (vector) with variable label attribute; or a \code{list} of variables
#'          with variable label attributes. See 'Examples'.
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
get_label <- function(x) {
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


#' @name get_var_labels
#' @rdname get_label
#' @export
get_var_labels <- function(x) {
  .Deprecated("get_label")
  return(get_label(x))
}


#' @title Retrieve value labels of labelled data
#' @name get_labels
#'
#' @description This function retrieves the value labels of labelled data, which
#'                was created with the \pkg{labelled} or \pkg{haven} package, or
#'                imported from SPSS, SAS or STATA files (via \code{\link{read_spss}},
#'                \code{\link{read_sas}} or \code{\link{read_stata}}) and
#'                \itemize{
#'                  \item if \code{x} is a data frame or list of variables, returns all variables' value labels as \code{\link{list}}
#'                  \item or, if \code{x} is a vector, returns the value labels as string.
#'                  }
#'
#' @seealso See package vignettes or \href{http://www.strengejacke.de/sjPlot/}{online documentation}
#'            for more details; \code{\link{set_labels}} to manually set value labels, \code{\link{get_label}}
#'            to get variable labels and \code{\link{get_values}} to retrieve value label associated values.
#'
#' @param x \code{data.frame} with variables that have value label attributes (e.g.
#'          from an imported SPSS, SAS or STATA data set, via \code{\link{read_spss}},
#'          \code{\link{read_sas}} or \code{\link{read_stata}}); a variable
#'          (vector) with value label attributes; or a \code{list} of variables
#'          with values label attributes. If \code{x} has no label \code{\link{attributes}},
#'          factor \code{\link{levels}} are returned. See 'Examples'.
#' @param include.values string, indicating whether the values associated with the
#'          value labels are returned as well. If \code{include.values = "as.name"}
#'          (or \code{include.values = "n"}), values are set as \code{\link{names}}
#'          attribute of the returned object. If \code{include.values = "as.prefix"}
#'          (or \code{include.values = "p"}), values are included as prefix
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
#' @details This package can add (and read) value and variable labels either in \pkg{foreign}
#'            package style (attributes are named \emph{value.labels} and \emph{variable.label})
#'            or in \pkg{haven} package style (attributes are named \emph{labels} and
#'            \emph{label}). By default, the \pkg{haven} package style is used.
#'            \cr \cr
#'            Working with labelled data is a key element of the \pkg{sjPlot} package,
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
#' @note This function works with vectors that have value and variable
#'        label attributes (as provided, for instance, by \code{\link[haven]{labelled}}
#'        objects). Adding label attributes is automatically done when importing data sets
#'        with the \code{\link{read_spss}}, \code{\link{read_sas}} or \code{\link{read_stata}}
#'        functions. Labels can also manually be added using the \code{\link{set_labels}}
#'        and \code{\link{set_label}} functions. If vectors \strong{do not} have
#'        label attributes, either factor-\code{\link{levels}} or the numeric values
#'        of the vector are returned as labels.
#'        \cr \cr
#'        Most functions of the \pkg{sjPlot} package access value and variable label
#'        attributes to automatically detect labels in order to set them as axis,
#'        legend or title labels in plots (\code{sjp.}-functions) respectively as
#'        column or row headers in table outputs (\code{sjt.}-functions). See
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
get_labels <- function(x,
                           attr.only = FALSE,
                           include.values = NULL,
                           include.non.labelled = FALSE) {
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


#' @name get_val_labels
#' @rdname get_labels
#' @export
get_val_labels <- function(x,
                       attr.only = FALSE,
                       include.values = NULL,
                       include.non.labelled = FALSE) {
  .Deprecated("get_labels")
  return(get_labels(x, attr.only, include.values, include.non.labelled))
}


# Retrieve value labels of a data frame or variable
# See 'get_labels'
get_labels_helper <- function(x, attr.only, include.values, include.non.labelled) {
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
        values <- unname(lab)
      else
        values <- as.numeric(unname(lab))
      # retrieve label values in correct order
      labels <- names(lab)
      # do we want to include non-labelled values as well?
      if (include.non.labelled) {
        # get values of variable
        valid.vals <- sort(unique(stats::na.omit((as.vector(x)))))
        # check if we have more values than labels
        if (length(valid.vals) > length(labels)) {
          # We now need to know, which values of "x" don't
          # have labels. In case "x" is a character, we simply
          # subtract amount of labelled value codes from the
          # total value range of "x".
          # If "x" is numeric, we remove all valid values - as returned
          # by "get_values()" - from the value range.
          if (is.character(x))
            add_vals <- seq_len(length(valid.vals))[-seq_len(length(labels))]
          else
            add_vals <- valid.vals[!valid.vals %in% values]
          # add to labels
          labels <- c(labels, as.character(add_vals))
          # fix value prefix
          new_vals <- c(as.character(values), as.character(add_vals))
          # sort values and labels
          labels <- labels[order(c(as.numeric(values), add_vals))]
          new_vals <- new_vals[order(c(as.numeric(values), add_vals))]
          # set back new values
          values <- new_vals
        }
      }
      # include associated values?
      if (!is.null(include.values)) {
        # for backwards compatibility, we also accept "TRUE"
        # here we set values as names-attribute
        if ((is.logical(include.values) &&
             include.values == TRUE) ||
            include.values == "as.name" || include.values == "n") {
          names(labels) <- values
        }
        # here we include values as prefix of labels
        if (include.values == "as.prefix" || include.values == "p") {
          if (is.numeric(values))
            labels <- sprintf("[%i] %s", values, labels)
          else
            labels <- sprintf("[%s] %s", values, labels)
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
#'                \code{\link{read_sas}} or \code{\link{read_stata}}),
#'                or of a \code{\link[haven]{labelled}} vector.
#'
#' @seealso \code{\link{get_labels}} for getting value labels and \code{\link{get_na}}
#'            to get values for missing values.
#'
#' @param x variable (vector) with value label attributes.
#' @param sort.val logical, if \code{TRUE} (default), values of associated value labels
#'          are sorted.
#' @param drop.na logical, if \code{TRUE}, missing code values are excluded from
#'          the return value. See 'Examples' and \code{\link{get_na}}.
#' @return The values associated with value labels from \code{x},
#'           or \code{NULL} if \code{x} has no label attributes.
#'
#' @details \code{\link[haven]{labelled}} vectors are numeric by default (when imported with read-functions
#'            like \code{\link{read_spss}}) and have variable and value labels attributes.
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
#' library(haven)
#' # create labelled integer, with missing flag
#' x <- labelled(c(1, 2, 1, 3, 4, 1),
#'               c(Male = 1, Female = 2, Refused = 3, "N/A" = 4),
#'               c(FALSE, FALSE, TRUE, TRUE))
#' # get all values
#' get_values(x)
#' # drop NA
#' get_values(x, , TRUE)
#'
#'
#' @export
get_values <- function(x, sort.val = FALSE, drop.na = FALSE) {
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
  # remove missing value codes?
  if (drop.na) {
    # get NA logicals
    na.flag <- get_na_flags(x)
    # do we have missing flag? if yes, remove missing code value
    if (!is.null(na.flag)) values <- values[!na.flag]
  }
  # return sorted
  return(values)
}


#' @title Retrieve missing values of labelled variables
#' @name get_na
#'
#' @description This function retrieves the value codes associated with missing values
#'                of variables of an imported SPSS, SAS or STATA data set (via \code{\link{read_spss}},
#'                \code{\link{read_sas}} or \code{\link{read_stata}}), where missing
#'                values have not been replaced with \code{NA}s after import,
#'                or of \code{\link[haven]{labelled}} vectors.
#'
#' @seealso \code{\link{get_labels}} to get value labels, or \code{\link{get_values}}
#'            to get values associated with labels; see \code{\link{set_na}} to
#'            replace specific values with \code{NA} and \code{\link{to_na}} to
#'            convert missing value codes into \code{NA}; see \code{\link{get_na_flags}}
#'            to get a logical vector of missing flags.
#'
#' @param x variable (vector) with value label attributes, including
#'          missing value codes (see \code{\link[haven]{labelled}})
#' @return The missing values associated with value labels from \code{x},
#'           or \code{NULL} if \code{x} has no missing value attribute.
#'
#' @details Other statistical software packages (like 'SPSS') allow to define
#'            multiple missing values, e.g. \emph{not applicable}, \emph{refused answer}
#'            or "real" missing. These missing types may be assigned with
#'            different values, so it is possible to distinguish between these
#'            missing types. In R, multiple declared missings cannot be represented
#'            in a similar way. However, \code{\link[haven]{labelled}} vectors
#'            allow to indicate different missings through the
#'            \code{is_na}-\code{\link{attr}}. Technically, these "missings" are
#'            stored as normal values. Thus, the \code{\link{table}} command,
#'            for instance, would include these values by default. The
#'            \pkg{sjmisc} package offers capabilities to deal with multiple
#'            declared missings and enhances the possibilities to work with
#'            labelled data, allowing for easy access of multiple declared
#'            missings or conversion into \code{NA} etc.
#'            \cr \cr
#'            Furthermore, see 'Details' in \code{\link{get_values}}.
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
#'               c(Male = 1, Female = 2, Refused = 3, "N/A" = 4),
#'               c(FALSE, FALSE, TRUE, TRUE))
#' get_na(x)
#'
#' @export
get_na <- function(x) {
  # get values
  values <- get_values(x, sort.val = FALSE, drop.na = FALSE)
  # get NA logicals
  na.flag <- get_na_flags(x)
  # do we have missing flag?
  if (is.null(na.flag)) {
    message("Variable has no assigned missing value codes.")
    return(NULL)
  }
  # copy NA-codes to new vector, so we can check length
  nas <- values[na.flag]
  # set return value to NULL, if no missing values
  if (length(nas) == 0) nas <- NULL
  # return missing values
  return(nas)
}


getNaAttribute <- function() return("is_na")


#' @title Retrieve missing value flags of labelled variables
#' @name get_na_flags
#'
#' @description This function retrieves the logical missing flags for a
#'                \code{\link[haven]{labelled}} variable.
#'
#' @seealso \code{\link{get_na}} to get value codes of labelled missing values;
#'            \code{\link{get_values}} to get values associated with labels;
#'            see \code{\link{set_na}} to replace specific values with \code{NA}
#'            and \code{\link{to_na}} to convert missing value codes into \code{NA}.
#'
#' @param x variable (vector) with value label attributes, including
#'          missing value codes (see \code{\link[haven]{labelled}})
#' @return A logical vector with missing flags that indicate which labelled value
#'           is considered as missing.
#'
#' @details See 'Details' in \code{\link{get_na}}.
#'
#' @examples
#' library(haven)
#'
#' # create labelled integer, with missing flag
#' x <- labelled(c(1, 2, 1, 3, 4, 1),
#'               c(Male = 1, Female = 2, Refused = 3, "N/A" = 4),
#'               c(FALSE, FALSE, TRUE, TRUE))
#' get_na_flags(x)
#'
#' @export
get_na_flags <- function(x) return(attr(x, getNaAttribute(), exact = T))
