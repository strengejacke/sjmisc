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
#' @param def.value Optional, a character string which will be returned as label
#'          if \code{x} has no label attribute. By default, \code{NULL} is returned.
#'
#' @return A named char vector with all variable labels from the data frame or list;
#'           or a simple char vector (of length 1) with the variable label, if \code{x} is a variable.
#'           If \code{x} is a single vector and has no label attribute, the value
#'           of \code{def.value} will be returned (which is by default \code{NULL}).
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
get_label <- function(x, def.value = NULL) {
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
    if (is.null(attr.string)) return(def.value)
    # else return attribute
    retat <- attr(x, attr.string, exact = T)
    # still NULL? than use default return value
    if (is.null(retat)) retat <- def.value
    return(retat)
  }
}
