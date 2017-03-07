#' @title Add variable label(s) to variables
#' @name set_label
#'
#' @description This function adds variable labels as attribute
#'                (named \code{"label"}) to the variable \code{x}, resp. to a
#'                set of variables in a data frame or a list-object. \code{var_labels()}
#'                is intended for use within pipe-workflows and has a tidyverse-consistent
#'                syntax (see 'Examples').
#'
#' @seealso See vignette \href{../doc/intro_sjmisc.html}{Labelled Data and the sjmisc-Package}
#'            for more details; \code{\link{set_labels}} to manually set value labels or \code{\link{get_label}}
#'            to get variable labels.
#'
#' @param x Variable (vector), list of variables or a data frame where variables
#'          labels should be added as attribute. For \code{var_labels()}, \code{x}
#'          must be a data frame only.
#' @param ... Pairs of named vectors, where the name equals the variable name,
#'          which should be labelled, and the value is the new variable label.
#' @param label If \code{x} is a vector (single variable), use a single character string with
#'          the variable label for \code{x}. If \code{x} is a data frame, use a
#'          vector with character labels of same length as \code{ncol(x)}.
#'          Use \code{label = ""} to remove labels-attribute from \code{x}, resp.
#'          set any value of vector \code{label} to \code{""} to remove specific variable
#'          label attributes from a data frame's variable.
#' @param value See \code{label}.
#' @param lab Deprecated. Please use \code{label} instead.
#' @param attr.string Attribute string for the variable label. \strong{Note:}
#'          Usually, this argument should be ignored. It is only used internally
#'          for the \code{\link{write_spss}} and \code{\link{write_stata}} functions.
#'
#' @return \code{x}, with variable label attribute(s), which contains the
#'           variable name(s); or with removed label-attribute if
#'           \code{label = ""}.
#'
#' @details See 'Details' in \code{\link{get_labels}}
#'
#' @note See 'Note' in \code{\link{get_labels}}
#'
#' @examples
#' # manually set value and variable labels
#' dummy <- sample(1:4, 40, replace = TRUE)
#' dummy <- set_labels(dummy, labels = c("very low", "low", "mid", "hi"))
#' dummy <- set_label(dummy, label = "Dummy-variable")
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
#' dummy <- set_label(dummy, c("Variable A", "Variable B", "Variable C"))
#' str(dummy)
#'
#' # remove one variable label
#' dummy <- set_label(dummy, c("Variable A", "", "Variable C"))
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
#'
#' # use 'var_labels()' to set labels within a pipe-workflow, and
#' # when you need "tidyverse-consistent" api.
#' # Set variable labels for data frame
#' dummy <- data.frame(a = sample(1:4, 10, replace = TRUE),
#'                     b = sample(1:4, 10, replace = TRUE),
#'                     c = sample(1:4, 10, replace = TRUE))
#'
#' dummy %>%
#'   var_labels(a = "First variable", c = "third variable") %>%
#'   get_label()
#'
#' @export
set_label <- function(x, label, attr.string = NULL, lab) {

  # check deprecated arguments
  if (!missing(lab)) {
    message("Argument `lab` is deprecated. Please use `label` instead.")
    label <- lab
  }

  # auto-detect variable label attribute
  if (is.null(attr.string)) attr.string <- getVarLabelAttribute(x)
  # still nothing found? then leave...
  if (is.null(attr.string)) attr.string <- "label"

  # do we have all necessary arguments?
  if (!is.null(label) && !is.null(x)) {
    # if we have a data frame, we need a variable label
    # for each column (variable) of the data frame
    if (is.data.frame(x) || is.list(x)) {
      # get length of data frame or list, i.e.
      # determine number of variables
      if (is.data.frame(x))
        nvars <- ncol(x)
      else
        nvars <- length(x)

      # check for matching length of supplied labels
      if (nvars != length(label)) {
        message("Argument `label` must be of same length as numbers of columns in `x`.")
      } else {
        # do we have a data frame? If yes, save column names
        if (is.data.frame(x)) cnames <- colnames(x)

        # iterate all columns / list elements
        for (i in seq_len(nvars)) {
          if (sjmisc::is_empty(label[i])) {
            # empty label value means, remove
            # the label attribute
            attr(x[[i]], attr.string) <- NULL
          } else {
            # set variable label
            attr(x[[i]], attr.string) <- label[i]
            # set names attribute. equals variable name
            if (is.data.frame(x)) names(attr(x[[i]], attr.string)) <- cnames[i]
          }
        }
      }
    } else {
      if (sjmisc::is_empty(label))
        # empty label, so remove label attribute
        attr(x, attr.string) <- NULL
      else
        # set label attribute
        attr(x, attr.string) <- label
    }
  }
  x
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
