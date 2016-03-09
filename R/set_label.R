#' @title Add variable label(s) to variables
#' @name set_label
#'
#' @description This function adds variable labels as attribute
#'                (named \code{"label"} or \code{"variable.label"}) to a variable
#'                or vector \code{x}, resp. to a set of variables in a
#'                \code{data.frame} or a \code{list}-object. Most functions of the
#'                \pkg{sjPlot} package can automatically retrieve the variable
#'                labels to use it as axis labels or plot title (see 'Details').
#'
#' @seealso The sjPlot manual on \href{http://www.strengejacke.de/sjPlot/datainit/}{data initialization} or
#'            \href{http://www.strengejacke.de/sjPlot/view_spss/}{inspecting (SPSS imported) data frames} for
#'            more details; \code{\link{set_labels}} to manually set value labels or \code{\link{get_label}}
#'            to get variable labels.
#'
#' @param x Variable (vector), \code{list} of variables or a \code{data.frame}
#'          where variables labels should be added as attribute
#' @param lab If \code{x} is a vector (single variable), use a single character string with
#'          the variable label for \code{x}. If \code{x} is a data frame, use a
#'          vector with character labels of same length as \code{ncol(x)}.
#'          Use \code{lab = ""} to remove labels-attribute from \code{x}, resp.
#'          set any value of vector \code{lab} to \code{""} to remove specific variable
#'          label attributes from a data frame's variable.
#' @param value See \code{lab}.
#' @param attr.string Attribute string for the variable label. \strong{Note:}
#'          Usually, this argument should be ignored. It is only used internally
#'          for the \code{\link{write_spss}} and \code{\link{write_stata}} functions.
#' @return \code{x}, with variable label attribute(s), which contains the
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
#' dummy <- sample(1:4, 40, replace = TRUE)
#' dummy <- set_labels(dummy, c("very low", "low", "mid", "hi"))
#' dummy <- set_label(dummy, "Dummy-variable")
#'
#' # or use:
#' # set_label(dummy) <- "Dummy-variable"
#'
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
#' dummy1 <- sample(1:4, 40, replace = TRUE)
#' dummy2 <- sample(1:4, 40, replace = TRUE)
#' dummy3 <- sample(1:4, 40, replace = TRUE)
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
        message("Argument `lab` must be of same length as numbers of columns in `x`.")
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


#' @rdname set_label
#' @export
`set_label<-` <- function(x, attr.string = NULL, value) {
  UseMethod("set_label<-")
}

#' @export
`set_label<-.default` <- function(x, attr.string = NULL, value) {
  x <- set_label(x, value, attr.string)
  x
}
