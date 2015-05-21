#' @title Import SPSS dataset as data frame into R
#' @name read_spss
#'
#' @description Import data from SPSS, including NA's, value and variable labels.
#'
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data initialization}
#'            \item \href{http://www.strengejacke.de/sjPlot/view_spss/}{sjPlot manual: inspecting (SPSS imported) data frames}
#'            \item \code{\link{write_spss}}
#'            }
#'
#' @param path The file path to the SPSS dataset.
#' @param enc The file encoding of the SPSS dataset. \emph{Not needed if \code{option = "haven"} (default).}
#' @param autoAttachVarLabels if \code{TRUE}, variable labels will automatically be
#'          attached to each variable as \code{"variable.label"} attribute. Use this
#'          parameter, if \code{option = "foreign"}, where variable labels are attached
#'          as list-attribute to the imported data frame.
#'          \emph{Not needed if \code{option = "haven"} (default).}
#' @param atomic.to.fac Logical, if \code{TRUE}, factor variables imported from
#'          SPSS (which are imported as \code{\link{atomic}}) will be converted
#'          to \code{\link{factor}}s.
#' @param option string, indicating which package will be used to read the SPSS data file.
#'          By default, \code{option = "haven"}, which means, the \code{read_spss} function
#'          from the \code{haven} package is used. Use \code{option = "foreign"} to
#'          use foreign's \code{\link[foreign]{read.spss}} function. Use \code{options(read_spss = "foreign")}
#'          to make this function always use the foreign-package \code{\link[foreign]{read.spss}} function.
#' @return A data frame containing the SPSS data. Retrieve value labels with \code{\link{get_val_labels}}
#'   and variable labels with \code{\link{get_var_labels}}.
#'
#' @note This is a wrapper function for \code{\link[haven]{read_spss}} of the
#'         \code{haven} package and \code{\link[foreign]{read.spss}} of the
#'         \code{foreign} package. This function adds value and variable
#'         labels as attributes to the imported variables of the data frame.
#'         \cr \cr
#'         With attached value and variable labels, most functions of the
#'         \code{sjPlot} package automatically detect labels and uses them as axis,
#'         legend or title labels in plots (\code{sjp.}-functions) respectively
#'         as column or row headers in table outputs (\code{sjt.}-functions). See
#'         \href{http://www.strengejacke.de/sjPlot/datainit/}{online manual}
#'         for more details.
#'         \cr \cr
#'         When working with other packages, you can, e.g., use
#'         \code{\link{get_var_labels}} or \code{\link{get_val_labels}}
#'         to get a vector of value and variable labels, which can then be
#'         used with other functions like \code{\link{barplot}} etc.
#'         See 'Examples' from \code{\link{get_val_labels}}.
#'
#' @examples
#' \dontrun{
#' # import SPSS data set. uses haven's read function
#' # by default
#' mydat <- read_spss("my_spss_data.sav")
#'
#' # use foreign's read function
#' mydat <- read_spss("my_spss_data.sav",
#'                    enc = "UTF-8",
#'                    option = "foreign")
#'
#' # use haven's read function, convert atomic to factor
#' mydat <- read_spss("my_spss_data.sav", atomic.to.fac = TRUE)
#'
#' # retrieve variable labels
#' mydat.var <- get_var_labels(mydat)
#'
#' # retrieve value labels
#' mydat.val <- get_val_labels(mydat)}
#'
#' @export
read_spss <- function(path,
                      enc = NA,
                      autoAttachVarLabels = FALSE,
                      atomic.to.fac = FALSE,
                      option = "haven") {
  # --------------------------------------------------------
  # check read_spss option
  # --------------------------------------------------------
  if (is.null(option)) {
    opt <- getOption("read_spss")
    if (is.null(opt) || opt == "foreign") {
      option <- "foreign"
    } else {
      option <- "haven"
    }
  }
  # -------------------------------------
  # check parameter
  # -------------------------------------
  if (!is.null(option) && option != "foreign" && option != "haven") {
    warning("'option' must be either 'foreign' or 'haven'. Defaulting to 'foreign'.", call. = F)
    option <- "foreign"
  }
  # -------------------------------------
  # foreign import
  # -------------------------------------
  if (option == "foreign") {
    # ------------------------
    # check if suggested package is available
    # ------------------------
    if (!requireNamespace("foreign", quietly = TRUE)) {
      stop("Package 'foreign' needed for this function to work. Please install it.", call. = FALSE)
    }
    # import data as data frame
    data.spss <- suppressWarnings(foreign::read.spss(path,
                                                     to.data.frame = TRUE,
                                                     use.value.labels = FALSE,
                                                     reencode = enc))
    # convert atomic values to factors
    if (atomic.to.fac) data.spss <- atomic_to_fac(data.spss, getValLabelAttribute(data.spss))
    # auto attach labels
    if (autoAttachVarLabels) {
      message("Attaching variable labels. Please wait...\n")
      data.spss <- set_var_labels(data.spss, get_var_labels(data.spss))
    }
  } else {
    # ------------------------
    # check if suggested package is available
    # ------------------------
    if (!requireNamespace("haven", quietly = TRUE)) {
      stop("Package 'haven' needed for this function to work. Please install it.", call. = FALSE)
    }
    # read data file
    data.spss <- haven::read_spss(path)
    # convert to sjPlot
    data.spss <- to_sjPlot(data.spss)
    # convert atomic values to factors
    if (atomic.to.fac) data.spss <- atomic_to_fac(data.spss, getValLabelAttribute(data.spss))
  }
  # return data frame
  return(data.spss)
}


# converts atomic numeric vectors into factors with
# numerical factor levels
atomic_to_fac <- function(data.spss, attr.string) {
  # check for valid attr.string
  if (!is.null(attr.string)) {
    # -------------------------------------
    # create progress bar
    # -------------------------------------
    pb <- txtProgressBar(min = 0,
                         max = ncol(data.spss),
                         style = 3)
    # tell user...
    message("Converting atomic to factors. Please wait...\n")
    # iterate all columns
    for (i in 1:ncol(data.spss)) {
      # copy column to vector
      x <- data.spss[[i]]
      # is atomic, which was factor in SPSS?
      if (is.atomic(x) && !is.null(attr(x, attr.string, exact = T))) {
        # so we have value labels (only typical for factors, not
        # continuous variables) and a variable of type "atomic" (SPSS
        # continuous variables would be imported as numeric) - this
        # indicates we have a factor variable. now we convert to
        # factor, but need to capture labels attribute first
        labs <- attr(x, attr.string)
        # to factor
        x <- as.factor(x)
        # set back labels attribute
        attr(x, attr.string) <- labs
        # copy vector back to data frame
        data.spss[[i]] <- x
      }
      # update progress bar
      setTxtProgressBar(pb, i)
    }
    close(pb)
  }
  return(data.spss)
}


#' @title Import SAS dataset as data frame into R
#' @name read_sas
#'
#' @description Imports data from SAS (\code{.sas7bdat}), including NA's,
#'                value and variable labels.
#'
#' @seealso \code{\link{read_spss}}
#'
#' @param path The file path to the SAS data file.
#' @param path.cat optional, the file path to the SAS catalog file.
#' @param atomic.to.fac Logical, if \code{TRUE}, factor variables imported from
#'          SAS (which are imported as \code{\link{atomic}}) will be converted
#'          to \code{\link{factor}}s.
#' @return A data frame containing the SAS data. Retrieve value labels with \code{\link{get_val_labels}}
#'   and variable labels with \code{\link{get_var_labels}}.
#'
#' @note This is a wrapper function for \code{\link[haven]{read_sas}} function of the
#'         \code{haven} package. This function converts the imported data
#'         into a sjPlot friendly format (see \code{\link{to_sjPlot}}).
#'
#' @export
read_sas <- function(path, path.cat = NULL, atomic.to.fac = FALSE) {
  # ------------------------
  # check if suggested package is available
  # ------------------------
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("Package 'haven' needed for this function to work. Please install it.", call. = FALSE)
  }
  # read data file
  data <- haven::read_sas(path, path.cat)
  # convert to sjPlot
  data <- to_sjPlot(data)
  # convert atomic values to factors
  if (atomic.to.fac) data <- atomic_to_fac(data, getValLabelAttribute(data))
  # return data frame
  return(data)
}


#' @title Import STATA dataset as data frame into R
#' @name read_stata
#'
#' @description Imports data from STATA dta-files, including NA's,
#'                value and variable labels.
#'
#' @seealso \code{\link{read_spss}}
#'
#' @param path The file path to the STATA data file.
#' @param atomic.to.fac Logical, if \code{TRUE}, factor variables imported from
#'          STATA (which are imported as \code{\link{atomic}}) will be converted
#'          to \code{\link{factor}}s.
#' @return A data frame containing the STATA data. Retrieve value labels with \code{\link{get_val_labels}}
#'   and variable labels with \code{\link{get_var_labels}}.
#'
#' @note This is a wrapper function for \code{\link[haven]{read_dta}} function of the
#'         \code{haven} package. This function converts the imported data
#'         into a sjPlot friendly format (see \code{\link{to_sjPlot}}).
#'
#' @export
read_stata <- function(path, atomic.to.fac = FALSE) {
  # ------------------------
  # check if suggested package is available
  # ------------------------
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("Package 'haven' needed for this function to work. Please install it.", call. = FALSE)
  }
  # read data file
  data <- haven::read_dta(path)
  # convert to sjPlot
  data <- to_sjPlot(data)
  # convert atomic values to factors
  if (atomic.to.fac) data <- atomic_to_fac(data, getValLabelAttribute(data))
  # return data frame
  return(data)
}


#' @title Write content of data frame to SPSS sav-file
#' @name write_spss
#'
#' @description This function saves the content of a data frame to an SPSS sav-file.
#'
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data initialization}
#'            \item \href{http://www.strengejacke.de/sjPlot/view_spss/}{sjPlot manual: inspecting (SPSS imported) data frames}
#'            \item \code{\link{read_spss}}
#'            }
#'
#' @note You don't need to take care whether variables have been imported with
#'         the \code{\link{read_spss}} function from this package or from \code{haven}
#'         or even the \code{foreign} package, or if you have imported SPSS data and
#'         created new variables. This function does all necessary data preparation
#'         to write a properly labelled SPSS sav file.
#'
#' @param x data frame that should be saved as SPSS sav-file.
#' @param path file path to the SPSS dataset.
#'
#' @export
write_spss <- function(x, path) {
  write_data(x, path, "spss")
}


#' @title Write content of data frame to STATA dta-file
#' @name write_stata
#'
#' @description This function saves the content of a data frame to an STATA dta-file.
#'
#' @seealso \code{\link{write_spss}}
#'
#' @note You don't need to take care whether variables have been imported with
#'         the \code{\link{read_stata}} function from this package or from \code{haven},
#'         or if you have imported STATA data and
#'         created new variables. This function does all necessary data preparation
#'         to write a properly labelled STATA file.
#'
#' @param x data frame that should be saved as STATA-file.
#' @param path file path to the STATA dataset.
#'
#' @export
write_stata <- function(x, path) {
  write_data(x, path, "stata")
}


write_data <- function(x, path, type = "spss") {
  # ------------------------
  # check if suggested package is available
  # ------------------------
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("Package 'haven' needed for this function to work. Please install it.", call. = FALSE)
  }
  # -------------------------------------
  # create progress bar
  # -------------------------------------
  pb <- txtProgressBar(min = 0,
                       max = ncol(x),
                       style = 3)
  # tell user...
  message(sprintf("Prepare writing %s file. Please wait...\n", type))
  # check if variables should be converted to factors
  for (i in 1:ncol(x)) {
    # haven labelled objects don't need conversion
    if (!is_labelled(x[[i]])) {
      # get variable value
      var.lab <- get_var_labels(x[[i]])
      # convert variable to labelled factor, so it can be saved
      x[[i]] <- suppressWarnings(to_label(x[[i]]))
      # set back variable label
      x[[i]] <- set_var_labels(x[[i]], var.lab, "label")
    }
    # update progress bar
    setTxtProgressBar(pb, i)
  }
  # hide pb
  close(pb)
  if (type == "spss") {
    # tell user
    message(sprintf("Writing %s file to '%s'. Please wait...\n", type, path))
    # write SPSS
    haven::write_sav(x, path)
  } else if (type == "stata") {
    # tell user
    message(sprintf("Writing %s file to '%s'. Please wait...\n", type, path))
    # write SPSS
    haven::write_dta(x, path)
  }
}


# this function returns TRUE, if a vector is
# of class "labelled" (haven package)
is_labelled <- function(x) {
  # check if object has multiple class attributes
  if (length(class(x)) > 1) return(any(class(x) == "labelled"))
  # return if labelled
  return(class(x) == "labelled")
}


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
    pb <- txtProgressBar(min = 0,
                         max = ncol(x),
                         style = 3)
    # tell user...
    message("Converting from haven to sjPlot. Please wait...\n")
    for (i in 1:ncol(x)) {
      # remove labelled class
      if (is_labelled(x[[i]])) x[[i]] <- unclass(x[[i]])
      # update progress bar
      setTxtProgressBar(pb, i)
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


#' @title Retrieve value labels of a variable or an imported data frame
#' @name get_val_labels
#'
#' @description This function retrieves the value labels of an imported
#'                SPSS, SAS or STATA data set (via \code{\link{read_spss}},
#'                \code{\link{read_sas}} or \code{\link{read_stata}}) and
#'                \itemize{
#'                  \item if \code{x} is a data frame, returns the all variable's value labels as \code{\link{list}} object
#'                  \item or, if \code{x} is a vector, returns the label as string.
#'                  }
#'
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data initialization}
#'            \item \href{http://www.strengejacke.de/sjPlot/view_spss/}{sjPlot manual: inspecting (SPSS imported) data frames}
#'            \item \code{\link{set_val_labels}}
#'            }
#'
#' @param x a data frame with variables that have attached value labels (e.g.
#'          from an imported SPSS, SAS or STATA data set, via \code{\link{read_spss}},
#'          \code{\link{read_sas}} or \code{\link{read_stata}}) or a variable
#'          (vector) with attached value labels.
#' @return Either a list with all value labels from the data frame's variables,
#'           a string with the value labels, if \code{x} is a variable,
#'           or \code{NULL} if no value label attribute was found.
#'
#' @details This package can add (and read) value and variable labels either in \code{foreign}
#'            package style (\emph{value.labels} and \emph{variable.label}) or in
#'            \code{haven} package style (\emph{labels} and \emph{label}). By default,
#'            the \code{haven} package style is used.
#'            \cr \cr
#'            The \code{sjPlot} package accesses
#'            these attributes to automatically read label attributes for labelling
#'            axes categories and titles or table rows and columns.
#'            \cr \cr
#'            When working with other packages, you can, e.g., use
#'            \code{\link{get_var_labels}} or \code{\link{get_val_labels}}
#'            to get a vector of value and variable labels, which can then be
#'            used with other functions like \code{\link{barplot}} etc.
#'            See 'Examples' from \code{\link{get_val_labels}}.
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
#'        function or labels can manually be added using the \code{\link{set_val_labels}}
#'        and \code{\link{set_var_labels}} functions.
#'        \cr \cr
#'        With attached value and variable labels, most functions of the \code{sjPlot} package
#'        automatically detect labels and uses them as axis, legend or title labels
#'        in plots (\code{sjp.}-functions) respectively as column or row headers
#'        in table outputs (\code{sjt.}-functions).  See
#'        \href{http://www.strengejacke.de/sjPlot/datainit/}{online manual}
#'        for more details.
#'        \cr \cr
#'        Use \code{options(autoSetValueLabels = FALSE)}
#'        and \code{options(autoSetVariableLabels = FALSE)} to turn off automatic
#'        label detection.
#'
#' @examples
#' # import SPSS data set
#' # mydat <- read_spss("my_spss_data.sav", enc="UTF-8")
#'
#' # retrieve variable labels
#' # mydat.var <- get_var_labels(mydat)
#'
#' # retrieve value labels
#' # mydat.val <- get_val_labels(mydat)
#'
#' data(efc)
#' get_val_labels(efc$e42dep)
#'
#' # simple barplot
#' barplot(table(efc$e42dep))
#' # get value labels to annotate barplot
#' barplot(table(efc$e42dep),
#'         names.arg = get_val_labels(efc$e42dep),
#'         main = get_var_labels(efc$e42dep))
#'
#' @export
get_val_labels <- function(x) {
  if (is.data.frame(x) || is.matrix(x)) {
    a <- lapply(x, FUN = sji.getValueLabel)
  } else {
    a <- sji.getValueLabel(x)
  }
  return(a)
}


sji.getValueLabel <- function(x) {
  labels <- NULL
  # haven or sjPlot?
  attr.string <- getValLabelAttribute(x)
  # nothing found? then leave...
  if (is.null(attr.string)) return(NULL)
  # retrieve named labels
  lab <- attr(x, attr.string, exact = T)
  # check if we have anything
  if (!is.null(lab)) {
    # retrieve order of value labels
    reihenfolge <- order(as.numeric(unname(attr(x, attr.string, exact = T))))
    # retrieve label values in correct order
    labels <- names(lab)[reihenfolge]
  }
  # return them
  return(labels)
}


sji.getValueLabelValues <- function(x) {
  # haven or sjPlot?
  attr.string <- getValLabelAttribute(x)
  # nothing found? then leave...
  if (is.null(attr.string)) return(NULL)
  # sort values
  val.sort <- sort(as.numeric(unname(attr(x, attr.string, exact = T))))
  # return sorted
  return(val.sort)
}


#' @title Attach value labels to a variable or vector
#' @name set_val_labels
#'
#' @description This function attaches character labels as \code{"value.labels"} attribute
#'                to a variable or vector \code{"x"}, resp. to all variables of a data frame
#'                if \code{"x"} is a \code{\link{data.frame}}. These value labels will be accessed
#'                by functions of the \emph{sjPlot} package, in order to automatically set values
#'                or legend labels.
#'
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data initialization}
#'            \item \href{http://www.strengejacke.de/sjPlot/view_spss/}{sjPlot manual: inspecting (SPSS imported) data frames}
#'            \item \code{\link{get_val_labels}}
#'            }
#'
#' @param x a variable (vector) or a data frame where labels should be attached. Replaces former value labels.
#' @param labels a character vector of labels that will be attached to \code{x} by setting
#'          the \code{"labels"} or \code{"value.labels"} attribute. The length of this character vector must equal
#'          the value range of \code{x}, i.e. if \code{x} has values from 1 to 3,
#'          \code{labels} should have a length of 3.
#'          If \code{x} is a data frame, \code{labels} may also be a \code{\link{list}} of
#'          character vectors. If \code{labels} is a list, it must have the same length as
#'          number of columns of \code{x}. If \code{labels} is a vector and \code{x} is a data frame,
#'          the \code{labels} will be applied to each column of \code{x}.
#'          Use \code{labels = ""} to remove labels-attribute from \code{x}.
#' @return \code{x} with attached value labels; or with removed label-attribute if
#'            \code{labels = ""}.
#'
#' @details See 'Details' in \code{\link{get_val_labels}}
#'
#' @note See 'Note' in \code{\link{get_val_labels}}
#'
#' @examples
#' \dontrun{
#' library(sjPlot)
#' dummy <- sample(1:4, 40, replace=TRUE)
#' sjp.frq(dummy)
#'
#' dummy <- set_val_labels(dummy, c("very low", "low", "mid", "hi"))
#' sjp.frq(dummy)}
#'
#' @export
set_val_labels <- function(x, labels) {
  return(sji.setValueLabelNameParam(x, labels))
}


sji.setValueLabelNameParam <- function(x, labels) {
  # any valid labels? if not, return vector
  if (is.null(labels)) return(x)
  if (is.vector(x) || is.atomic(x)) {
    return(sji.setValueLabel.vector(x, labels))
  } else if (is.data.frame(x) || is.matrix(x)) {
    for (i in 1:ncol(x)) {
      if (is.list(labels)) {
        x[[i]] <- sji.setValueLabel.vector(x[[i]], labels[[i]], colnames(x)[i])
      } else if (is.vector(labels)) {
        x[[i]] <- sji.setValueLabel.vector(x[[i]], labels, colnames(x)[i])
      } else {
        warning("'labels' must be a list of same length as 'ncol(x)' or a vector.", call. = F)
      }
    }
    return(x)
  }
}


sji.setValueLabel.vector <- function(var, labels, var.name = NULL) {
  # auto-detect variable label attribute
  attr.string <- getValLabelAttribute(var)
  # do we have any label attributes?
  if (is.null(attr.string)) attr.string <- "labels"
  # check for null
  if (!is.null(labels)) {
    # if labels is empty string, remove labels
    # attribute
    if (length(labels) == 1 && nchar(labels) == 0) {
      attr(var, attr.string) <- NULL
    } else if (is.null(var) || is.character(var)) {
    # string varibles can't get value labels
      warning("Can't attach value labels to string or NULL vectors.", call. = F)
    } else {
      # check if var is a factor
      if (is.factor(var)) {
        # check if we have numeric levels
        if (!is_num_fac(var)) {
          # retrieve levels. since levels are numeric, we
          # have minimum and maximum values
          minval <- 1
          maxval <- length(levels(var))
        } else {
          # levels are not numeric. we need to convert them
          # first to retrieve minimum level, as numeric
          minval <- min(as.numeric(levels(var)), na.rm = T)
          # check range, add minimum, so we have max
          maxval <- diff(range(as.numeric(levels(var)))) + minval
        }
      } else {
        # retrieve values
        minval <- min(var, na.rm = TRUE)
        maxval <- max(var, na.rm = TRUE)
      }
      # check for unlisting
      if (is.list(labels)) labels <- as.vector(unlist(labels))
      # determine amount of labels
      lablen <- length(labels)
      # determine value range
      valrange <- maxval - minval + 1
      # set var name string
      if (is.null(var.name) || nchar(var.name) < 1) {
        name.string <- "var"
      } else {
        name.string <- var.name
      }
      if (is.infinite(valrange)) {
        warning("Can't set value labels. Infinite value range.", call. = F)
      # check for valid length of labels
      } else if (valrange < lablen) {
        # we have more labels than values, so just take as many
        # labes as values are present
        message(sprintf("More labels than values of \"%s\". Using first %i labels.", name.string, valrange))
        attr(var, attr.string) <- as.character(c(minval:maxval))
        names(attr(var, attr.string)) <- labels[1:valrange]
      # value range is larger than amount of labels. we may
      # have not continuous value range, e.g. "-2" as filter and
      # 1 to 4 as valid values, i.e. -1 and 0 are missing
      } else if (valrange > lablen) {
        # value range is not continuous. get all unique values
        values <- sort(unique(na.omit((var))))
        # get amount of unique values
        valrange <- length(values)
        # still no match?
        if (valrange != lablen) {
          warning(sprintf("Can't set value labels. Value range of \"%s\" is longer than length of \"labels\".", name.string), call. = F)
        } else {
          # else, set attributes
          attr(var, attr.string) <- as.character(c(1:valrange))
          names(attr(var, attr.string)) <- labels
        }
      } else {
        attr(var, attr.string) <- c(as.character(c(minval:maxval)))
        names(attr(var, attr.string)) <- labels
      }
    }
  }
  return(var)
}


#' @title Check whether a factor has numeric levels only
#' @name is_num_fac
#' @description This function checks whether a factor has only numeric or
#'                any non-numeric factor levels.
#'
#' @param x a \code{\link{factor}}.
#' @return Logical, \code{TRUE} if factor has numeric factor levels only,
#'           \code{FALSE} otherwise.
#'
#' @examples
#' # numeric factor levels
#' f1 <- factor(c(NA, 1, 3, NA, 2, 4))
#' is_num_fac(f1)
#'
#' # not completeley numeric factor levels
#' f2 <- factor(c(NA, "C", 1, 3, "A", NA, 2, 4))
#' is_num_fac(f2)
#'
#' # not completeley numeric factor levels
#' f3 <- factor(c("Justus", "Bob", "Peter"))
#' is_num_fac(f3)
#'
#' @export
is_num_fac <- function(x) {
  # check if we have numeric levels
  return(!anyNA(suppressWarnings(as.numeric(levels(x)))))
}


#' @title Retrieve variable labels of a data frame or a variable
#' @name get_var_labels
#'
#' @description This function retrieves the value labels of an imported
#'                SPSS, SAS or STATA data set (via \code{\link{read_spss}},
#'                \code{\link{read_sas}} or \code{\link{read_stata}}) and
#'                \itemize{
#'                  \item if \code{x} is a data frame, returns the all variable labels as names character vector of length \code{ncol(x)}.
#'                  \item or, if \code{x} is a vector, returns the variable label as string.
#'                  }
#'
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data initialization}
#'            \item \href{http://www.strengejacke.de/sjPlot/view_spss/}{sjPlot manual: inspecting (SPSS imported) data frames}
#'            \item \code{\link{set_var_labels}}
#'            }
#'
#' @param x A data frame or a vector with \code{"label"} or \code{"variable.label"} attribute.
#'
#' @return A named char vector with all variable labels from the data frame,
#'           or a simple char vector (of length 1) with the variable label, if \code{x} is a variable.
#'
#' @details See 'Details' in \code{\link{get_val_labels}}
#'
#' @note See 'Note' in \code{\link{get_val_labels}}
#'
#' @examples
#' # import SPSS data set
#' # mydat <- read_spss("my_spss_data.sav", enc="UTF-8")
#'
#' # retrieve variable labels
#' # mydat.var <- get_var_labels(mydat)
#'
#' # retrieve value labels
#' # mydat.val <- get_val_labels(mydat)
#'
#' data(efc)
#'
#' # get variable lable
#' get_var_labels(efc$e42dep)
#'
#' # alternative way
#' get_var_labels(efc)["e42dep"]
#'
#' # simple barplot
#' barplot(table(efc$e42dep))
#' # get value labels to annotate barplot
#' barplot(table(efc$e42dep),
#'         names.arg = get_val_labels(efc$e42dep),
#'         main = get_var_labels(efc$e42dep))
#'
#' @export
get_var_labels <- function(x) {
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
  } else {
    # nothing found? then leave...
    if (is.null(attr.string)) return(NULL)
    # else return attribute
    return(attr(x, attr.string, exact = T))
  }
}


#' @title Attach variable label(s) to a single variable or data frame
#' @name set_var_labels
#' @description This function sets variable labels to a single variable or to
#'                a set of variables in a data frame. To each variable, the
#'                attribute \code{"label"} or \code{"variable.label"} with the related variable
#'                name is attached. Most functions of the \emph{sjPlot} package can automatically
#'                retrieve the variable name to use it as axis labels or plot title
#'                (see details).
#'
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data initialization}
#'            \item \href{http://www.strengejacke.de/sjPlot/view_spss/}{sjPlot manual: inspecting (SPSS imported) data frames}
#'            \item \code{\link{get_var_labels}}
#'            }
#'
#' @param x A single variable (vector) or data frame with variables.
#' @param lab If \code{x} is a vector (single variable), use a single character string with
#'          the variable label for \code{x}. If \code{x} is a \code{\link{data.frame}}, use a
#'          vector with character labels of same length as \code{ncol(x)}.
#'          Use \code{lab = ""} to remove labels-attribute from \code{x}, resp.
#'          set any value of vector \code{lab} to \code{""} to remove specific variable
#'          label attributes from a data frame's variable.
#' @param attr.string The attribute string for the variable label. To ensure
#'          compatibility to the \code{foreign}-package, use the default string
#'          \code{"variable.label"}. If you want to save data with the \code{haven}
#'          package, use \code{attr.string = "label"}. There is a wrapper function
#'          \code{\link{write_spss}} to save SPSS files, so you don't need to take
#'          care of this.
#' @return \code{x}, with attached variable label attribute(s), which contains the
#'           variable name(s); or with removed label-attribute if
#'            \code{lab = ""}.
#'
#' @details See 'Details' in \code{\link{get_val_labels}}
#'
#' @note See 'Note' in \code{\link{get_val_labels}}
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
#' # ---------------------------------------------
#' # manually set value and variable labels
#' # ---------------------------------------------
#' dummy <- sample(1:4, 40, replace=TRUE)
#' dummy <- set_val_labels(dummy, c("very low", "low", "mid", "hi"))
#' dummy <- set_var_labels(dummy, "Dummy-variable")
#' # auto-detection of value labels by default, auto-detection of
#' # variable labels if parameter "title" set to NULL.
#' \dontrun{
#' library(sjPlot)
#' sjp.frq(dummy, title = NULL)}
#'
#' # ---------------------------------------------
#' # Set variable labels for data frame
#' # ---------------------------------------------
#' dummy <- data.frame(a = sample(1:4, 10, replace = TRUE),
#'                     b = sample(1:4, 10, replace = TRUE),
#'                     c = sample(1:4, 10, replace = TRUE))
#' dummy <- set_var_labels(dummy,
#'                         c("Variable A",
#'                           "Variable B",
#'                           "Variable C"))
#' str(dummy)
#'
#' # remove one variable label
#' dummy <- set_var_labels(dummy,
#'                         c("Variable A",
#'                           "",
#'                           "Variable C"))
#' str(dummy)
#'
#' @export
set_var_labels <- function(x, lab, attr.string = NULL) {
  # ----------------------------
  # auto-detect variable label attribute
  # ----------------------------
  if (is.null(attr.string)) attr.string <- getVarLabelAttribute(x)
  # still nothing found? then leave...
  if (is.null(attr.string)) attr.string <- "label"
  # do we have all necessary parameters?
  if (!is.null(lab) && !is.null(x)) {
    # if we have a data frame, we need a variable label
    # for each column (variable) of the data frame
    if (is.data.frame(x)) {
      if (ncol(x) != length(lab)) {
        message("Parameter \"lab\" must be of same length as numbers of columns in \"x\".")
      } else {
        # -------------------------------------
        # create progress bar
        # -------------------------------------
        pb <- txtProgressBar(min = 0,
                             max = ncol(x),
                             style = 3)
        for (i in 1:ncol(x)) {
          if (nchar(lab[i]) == 0) {
            attr(x[[i]], attr.string) <- NULL
          } else {
            # set variable label
            attr(x[[i]], attr.string) <- lab[i]
            # set names attribute. equals variable name
            names(attr(x[[i]], attr.string)) <- colnames(x)[i]
          }
          # update progress bar
          setTxtProgressBar(pb, i)
        }
        close(pb)
      }
    } else {
      if (nchar(lab) == 0)
        attr(x, attr.string) <- NULL
      else
        attr(x, attr.string) <- lab
    }
  }
  return(x)
}


#' @title Converts variable into factor and replaces values with associated value labels
#' @name to_label
#'
#' @description This function converts (replaces) variable values (also of factors)
#'                with their associated value labels. Might be helpful for factor variables.
#'                For instance, if you have a Gender variable with 0/1 value, and associated
#'                labels are male/female, this function would convert all 0 to male and
#'                all 1 to female and returns the new variable as \code{\link{factor}}.
#'
#' @seealso \code{\link{to_fac}} to convert a numeric variable into a factor (and
#'            retain labels) and \code{\link{to_value}} to convert a factor into
#'            a numeric variable.
#'
#' @param x A variable of type \code{\link{numeric}}, \code{\link{atomic}},
#'          \code{\link{factor}} or \code{\link[haven]{labelled}} (see \code{haven} package)
#'          \emph{with associated value labels} (see \code{\link{set_val_labels}}),
#'          respectively a data frame with such variables.
#' @return A factor variable with the associated value labels as factor levels, or a
#'           data frame with such factor variables (if \code{x} was a data frame).
#'
#' @note Value and variable label attributes (see, for instance, \code{\link{get_val_labels}}
#'         or \code{\link{set_val_labels}}) will be removed  when converting variables to factors.
#'
#' @examples
#' data(efc)
#' print(get_val_labels(efc)['c161sex'])
#' head(efc$c161sex)
#' head(to_label(efc$c161sex))
#'
#' print(get_val_labels(efc)['e42dep'])
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
#' @export
to_label <- function(x) {
  if (is.matrix(x) || is.data.frame(x)) {
    for (i in 1:ncol(x)) x[[i]] <- to_label_helper(x[[i]])
    return(x)
  } else {
    return(to_label_helper(x))
  }
}


to_label_helper <- function(x) {
  # check if factor has numeric factor levels
  if (is.factor(x) && !is_num_fac(x)) {
    # if not, stop here
    warning("'x' may have numeric factor levels only.", call. = F)
    return(x)
  }
  # get value labels
  vl <- get_val_labels(x)
  # check if we have any labels, else
  # return variable "as is"
  if (!is.null(vl)) {
    # get associated values for value labels
    vn <- sji.getValueLabelValues(x)
    # replace values with labels
    if (is.factor(x)) {
      levels(x) <- vl
    } else {
      for (i in 1:length(vl)) x[x == vn[i]] <- vl[i]
      # to factor
      x <- factor(x, levels = vl)
    }
  }
  # return as factor
  return(x)
}


#' @title Convert variable into factor and keep value labels
#' @name to_fac
#'
#' @description This function converts a variable into a factor, but keeps
#'                variable and value labels, if these are attached as attributes
#'                to the variale. See examples.
#'
#' @seealso \code{\link{to_value}} to convert a factor into a numeric value and
#'            \code{\link{to_label}} to convert a value into a factor with labelled
#'            factor levels.
#'
#' @param x A (numeric or atomic) variable or a data frame with
#'          (numeric or atomic) variables.
#' @return A factor variable, including variable and value labels, respectively
#'           a data frame with factor variables (including variable and value labels)
#'           if \code{x} was a data frame.
#'
#' @note This function is intended for use with vectors that have value and variable
#'        labels attached. Unlike \code{\link{as.factor}}, \code{to_fac} converts
#'        a variable into a factor and retains the value and variable label attributes.
#'        \cr \cr
#'        Attaching labels is automatically done by importing data sets
#'        with one of the \code{read_*}-functions, like \code{\link{read_spss}}.
#'        Else, value and variable labels can be manually added to vectors
#'        with \code{\link{set_val_labels}} and \code{\link{set_var_labels}}.
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
#' efc$e42dep <- to_fac(efc$e42dep)
#' sjt.frq(efc$e42dep)}
#'
#' @export
to_fac <- function(x) {
  if (is.matrix(x) || is.data.frame(x)) {
    for (i in 1:ncol(x)) x[[i]] <- to_fac_helper(x[[i]])
    return(x)
  } else {
    return(to_fac_helper(x))
  }
}


to_fac_helper <- function(x) {
  # retrieve value labels
  lab <- get_val_labels(x)
  # retrieve variable labels
  varlab <- get_var_labels(x)
  # convert variable to factor
  x <- as.factor(x)
  # set back value labels
  x <- set_val_labels(x, lab)
  # set back variable labels
  x <- set_var_labels(x, varlab)
  return(x)
}


#' @title Converts factors to numeric variables
#' @name to_value
#'
#' @description This function converts (replaces) factor values with the
#' related factor level index number, thus the factor is converted to
#' a numeric variable.
#'
#' @seealso \code{\link{to_label}} to convert a value into a factor with labelled
#'            factor levels and \code{\link{to_fac}} to convert a numeric variable
#'            into a factor (and retain labels)
#'
#' @param x A (factor) variable or a data frame with (factor) variables.
#' @param startAt the starting index, i.e. the lowest numeric value of the variable's
#'          value range. By default, this parameter is \code{NULL}, hence the lowest
#'          value of the returned numeric variable corresponds to the lowest factor
#'          level (if factor is \code{\link{numeric}}) or to \code{1} (if factor levels
#'          are not numeric).
#' @param keep.labels logical, if \code{TRUE}, former factor levels will be attached as
#'          value labels. See \code{\link{set_val_labels}} for more details.
#' @return A numeric variable with values ranging either from \code{startAt} to
#'           \code{startAt} + length of factor levels, or to the corresponding
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
#' # non-numeric factor is converted to numeric
#' # starting at 1
#' dummy <- factor(c("D", "F", "H"))
#' table(to_value(dummy))
#'
#' @export
to_value <- function(x, startAt = NULL, keep.labels = TRUE) {
  if (is.matrix(x) || is.data.frame(x)) {
    for (i in 1:ncol(x)) x[[i]] <- to_value_helper(x[[i]], startAt, keep.labels)
    return(x)
  } else {
    return(to_value_helper(x, startAt, keep.labels))
  }
}


to_value_helper <- function(x, startAt, keep.labels) {
  # retrieve "value labels"
  labels <- levels(x)
  # check if we have numeric factor levels
  if (is_num_fac(x)) {
    # convert to numeric via as.vector
    new_value <- as.numeric(as.vector((x)))
    # new minimum value?
    if (!is.null(startAt) && is.numeric(startAt)) {
      # check if lowest value of variable differs from
      # requested minimum conversion value
      val_diff <- startAt - min(new_value, na.rm = T)
      # adjust new_value
      new_value <- new_value + val_diff
    }
  } else {
    # check startAt value
    if (is.null(startAt)) startAt <- 1
    # get amount of categories
    l <- length(levels(x))
    # determine highest category value
    end <- startAt + l - 1
    # replace labels with numeric values
    levels(x) <- c(startAt:end)
    # convert to numeric
    new_value <- as.numeric(as.character(x))
  }
  # check if we should attach former labels as value labels
  if (keep.labels) new_value <- set_val_labels(new_value, labels)
  return(new_value)
}
