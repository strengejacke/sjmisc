#' @title Add value labels to variables
#' @name set_labels
#'
#' @description This function adds character \code{labels} as attribute
#'                (named \code{"labels"} or \code{"value.labels"}) to a variable
#'                or vector \code{x}, resp. to a set of variables in a
#'                \code{data.frame} or a \code{list}-object. These value labels will be accessed
#'                by functions of the \pkg{sjPlot} package, in order to automatically set values
#'                or legend labels, however, \pkg{sjmisc} provides functions to
#'                quickly access these attributes for other purposes.
#'
#' @seealso See package vignettes or \href{http://www.strengejacke.de/sjPlot/}{online documentation}
#'            for more details; \code{\link{set_label}} to manually set variable labels or
#'            \code{\link{get_label}} to get variable labels; \code{\link{add_labels}} to
#'            add additional value labels without replacing the existing ones.
#'
#' @param x Variable (vector), \code{list} of variables or a \code{data.frame}
#'          where value label attributes should be added. Replaces former value labels.
#' @param labels (Named) character vector of labels that will be added to \code{x} as
#'          \code{"labels"} or \code{"value.labels"} attribute.
#'          \itemize{
#'            \item if \code{labels} is \strong{not} a \emph{named vector}, its length must equal the value range of \code{x}, i.e. if \code{x} has values from 1 to 3, \code{labels} should have a length of 3;
#'            \item if length of \code{labels} is intended to differ from length of unique values of \code{x}, a warning is given. You can still add missing labels with the \code{force.labels} or \code{force.values} arguments; see 'Note'.
#'            \item if \code{labels} \strong{is} a \emph{named vector}, value labels will be set accordingly, even if \code{x} has a different length of unique values. See 'Note' and 'Examples'.
#'            \item if \code{x} is a data frame, \code{labels} may also be a \code{\link{list}} of (named) character vectors;
#'            \item if \code{labels} is a \code{list}, it must have the same length as number of columns of \code{x};
#'            \item if \code{labels} is a vector and \code{x} is a data frame, \code{labels} will be applied to each column of \code{x}.
#'            }
#'          Use \code{labels = ""} to remove labels-attribute from \code{x}.
#' @param value See \code{labels},
#' @param force.labels Logical; if \code{TRUE}, all \code{labels} are added as value label
#'          attribute, even if \code{x} has less unique values then length of \code{labels}
#'          or if \code{x} has a smaller range then length of \code{labels}. See 'Examples'.
#'          This parameter will be ignored, if \code{labels} is a named vector.
#' @param force.values Logical, if \code{TRUE} (default) and \code{labels} has less
#'          elements than unique values of \code{x}, additional values not covered
#'          by \code{labels} will be added as label as well. See 'Examples'.
#'          This parameter will be ignored, if \code{labels} is a named vector.
#' @return \code{x} with value label attributes; or with removed label-attributes if
#'            \code{labels = ""}.
#'
#' @details See 'Details' in \code{\link{get_labels}}.
#'
#' @note \itemize{
#'         \item if \code{labels} is a named vector, \code{force.labels} and \code{force.values} will be ignored, and only values defined in \code{labels} will be labelled;
#'         \item if \code{x} has less unique values than \code{labels}, redundant labels will be dropped, see \code{force.labels};
#'         \item if \code{x} has more unique values than \code{labels}, only matching values will be labelled, other values remain unlabelled, see \code{force.values};
#'         }
#'         If you only want to change partial value labels, use \code{\link{add_labels}} instead.
#'         Furthermore, see 'Note' in \code{\link{get_labels}}.
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
#'
#' # or use:
#' # set_labels(x) <- c("1", "2", "3")
#'
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
#' frq(as_labelled(x))
#'
#'
#' # set labels via named vector,
#' # not using all possible values
#' data(efc)
#' get_labels(efc$e42dep)
#'
#'x <- set_labels(efc$e42dep, c(`independent` = 1,
#'                              `severe dependency` = 2,
#'                              `missing value` = 9))
#' get_labels(x, include.values = "p")
#'
#' get_labels(x, include.values = "p", include.non.labelled = TRUE)
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
      warning("can't add value labels to string or NULL vectors.", call. = F)
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
        # check names and value attributes. value labels
        # and values might be reversed
        # ---------------------------------------
        if (!anyNA(suppressWarnings(as.numeric(names(labels)))) &&
            anyNA(suppressWarnings(as.numeric(labels))) &&
            !anyNA(suppressWarnings(as.numeric(values)))) {
          dummy.lab.values <- as.numeric(names((labels)))
          dummy.lab.labels <- as.character(labels)
          labels <- dummy.lab.values
          names(labels) <- dummy.lab.labels
        }
        # ---------------------------------------
        # set attributes
        # ---------------------------------------
        if (anyNA(suppressWarnings(as.numeric(labels)))) {
          # here we have also non-numeric labels, so we set
          # names as character string
          attr(x, attr.string) <- labels
        } else {
          # we have only numeric labels, so we set them
          # as numeric values
          attr(x, attr.string) <- as.numeric(labels)
        }
        names(attr(x, attr.string)) <- as.character(names(labels))
        # ---------------------------------------
        # check for valid length of labels
        # if amount of labels and values are equal,
        # we assume matching labels
        # ---------------------------------------
      } else if (length(values) == lablen) {
        # ---------------------------------------
        # set attributes
        # check whether values is numeric, or - if character -
        # only has numeric character values. If yes, add values
        # as numeric labels-attribute
        # ---------------------------------------
        if (is.numeric(values) || !anyNA(suppressWarnings(as.numeric(values))))
          attr(x, attr.string) <- as.numeric(values)
        else
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
          attr(x, attr.string) <- as.numeric(c(1:lablen))
          names(attr(x, attr.string)) <- labels
        } else {
          # ---------------------------------------
          # we have more labels than values, so just take as many
          # labes as values are present
          # ---------------------------------------
          message(sprintf("More labels than values of \"%s\". Using first %i labels.", name.string, valrange))
          attr(x, attr.string) <- as.numeric(c(minval:maxval))
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
          attr(x, attr.string) <- as.numeric(c(1:valrange))
          names(attr(x, attr.string)) <- labels
        } else {
          # ---------------------------------------
          # tell user about modification
          # ---------------------------------------
          message(sprintf("\"%s\" has more values than \"labels\", hence not all values are labelled.", name.string))
          # drop values with no associated labels
          attr(x, attr.string) <- as.numeric(c(1:length(labels)))
          names(attr(x, attr.string)) <- labels
        }
      } else {
        attr(x, attr.string) <- as.numeric(c(minval:maxval))
        names(attr(x, attr.string)) <- labels
      }
    }
  }
  return(x)
}

#' @rdname set_labels
#' @export
`set_labels<-` <- function(x, force.labels = FALSE, force.values = TRUE, value) {
  UseMethod("set_labels<-")
}

#' @export
`set_labels<-.default` <- function(x, force.labels = FALSE, force.values = TRUE, value) {
  x <- set_labels(x, value, force.labels, force.values)
  x
}
