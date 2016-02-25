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
#' @param include.values String, indicating whether the values associated with the
#'          value labels are returned as well. If \code{include.values = "as.name"}
#'          (or \code{include.values = "n"}), values are set as \code{\link{names}}
#'          attribute of the returned object. If \code{include.values = "as.prefix"}
#'          (or \code{include.values = "p"}), values are included as prefix
#'          to each label. See 'Examples'.
#' @param attr.only Logical, if \code{TRUE}, labels are only searched for
#'          in the the vector's \code{\link{attributes}}; else, if \code{x} has no
#'          label attributes, factor levels or string values are returned. See
#'          'Examples'.
#' @param include.non.labelled Logical, if \code{TRUE}, values without labels will
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
        # check if we have different amount values than labels
        # or, if we have same amount of values and labels, whether
        # values and labels match or not
        if (length(valid.vals) != length(labels) || anyNA(match(values, valid.vals))) {
          # We now need to know, which values of "x" don't
          # have labels.
          add_vals <- valid.vals[!valid.vals %in% values]
          # add to labels
          labels <- c(labels, as.character(add_vals))
          # fix value prefix
          new_vals <- c(as.character(values), as.character(add_vals))
          # check if values are numeric or not. if not,
          # make sure it's character, so we can order
          # consistently
          if (suppressWarnings(anyNA(as.numeric(values)))) {
            orderpart <- as.character(values)
          } else {
            orderpart <- as.numeric(values)
          }
          # sort values and labels
          labels <- labels[order(c(orderpart, add_vals))]
          new_vals <- new_vals[order(c(orderpart, add_vals))]
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
  # foreign? then reverse order
  if (is_foreign(attr.string)) labels <- rev(labels)
  # return them
  return(labels)
}
