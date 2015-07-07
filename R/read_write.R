#' @title Import SPSS dataset as data frame into R
#' @name read_spss
#'
#' @description Import data from SPSS, including NA's, value and variable labels.
#'
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data initialization}
#'            \item \href{http://www.strengejacke.de/sjPlot/labelleddata/}{sjPlot-manual: working with labelled data}
#'            \item \href{http://www.strengejacke.de/sjPlot/view_spss/}{sjPlot manual: inspecting (SPSS imported) data frames}
#'            \item \code{\link{write_spss}}
#'            }
#'
#' @param path The file path to the SPSS dataset.
#' @param enc The file encoding of the SPSS dataset. \emph{Not needed if \code{option = "haven"} (default).}
#' @param attach.var.labels if \code{TRUE}, variable labels will automatically be
#'          attached to each variable as \code{"variable.label"} attribute. Use this
#'          parameter, if \code{option = "foreign"}, where variable labels are attached
#'          as list-attribute to the imported data frame.
#'          \emph{Not needed if \code{option = "haven"} (default).}
#' @param atomic.to.fac Logical, if \code{TRUE}, factor variables imported from
#'          SPSS (which are imported as \code{\link{atomic}}) will be converted
#'          to \code{\link{factor}}s.
#' @param use.na logical, should information on user-defined missing values be
#'          used to set the corresponding values to NA?
#' @param option string, indicating which package will be used to read the SPSS data file.
#'          By default, \code{option = "haven"}, which means, the \code{read_spss} function
#'          from the \code{haven} package is used. Use \code{option = "foreign"} to
#'          use foreign's \code{\link[foreign]{read.spss}} function. Use \code{options(read_spss = "foreign")}
#'          to make this function always use the foreign-package \code{\link[foreign]{read.spss}} function.
#' @return A data frame containing the SPSS data. Retrieve value labels with \code{\link{get_labels}}
#'   and variable labels with \code{\link{get_label}}.
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
#'         When working with labelled data, you can, e.g., use
#'         \code{\link{get_label}} or \code{\link{get_labels}}
#'         to get a vector of value and variable labels, which can then be
#'         used with other functions like \code{\link{barplot}} etc.
#'         See 'Examples' from \code{\link{get_labels}}.
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
#' mydat.var <- get_label(mydat)
#'
#' # retrieve value labels
#' mydat.val <- get_labels(mydat)}
#'
#' @export
read_spss <- function(path,
                      enc = NA,
                      attach.var.labels = FALSE,
                      atomic.to.fac = FALSE,
                      use.na = TRUE,
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
                                                     use.missings = use.na,
                                                     reencode = enc))
    # convert atomic values to factors
    if (atomic.to.fac) data.spss <- atomic_to_fac(data.spss, getValLabelAttribute(data.spss))
    # auto attach labels
    if (attach.var.labels) {
      message("Attaching variable labels. Please wait...\n")
      data.spss <- set_label(data.spss, get_label(data.spss))
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
#' @importFrom utils txtProgressBar setTxtProgressBar
atomic_to_fac <- function(data.spss, attr.string) {
  # check for valid attr.string
  if (!is.null(attr.string)) {
    # -------------------------------------
    # create progress bar
    # -------------------------------------
    pb <- utils::txtProgressBar(min = 0,
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
        labs <- attr(x, attr.string, exact = T)
        # to factor
        x <- as.factor(x)
        # set back labels attribute
        attr(x, attr.string) <- labs
        # copy vector back to data frame
        data.spss[[i]] <- x
      }
      # update progress bar
      utils::setTxtProgressBar(pb, i)
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
#' @return A data frame containing the SAS data. Retrieve value labels with \code{\link{get_labels}}
#'   and variable labels with \code{\link{get_label}}.
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
#' @return A data frame containing the STATA data. Retrieve value labels with \code{\link{get_labels}}
#'   and variable labels with \code{\link{get_label}}.
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


#' @importFrom utils txtProgressBar setTxtProgressBar
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
  pb <- utils::txtProgressBar(min = 0,
                              max = ncol(x),
                              style = 3)
  # tell user...
  message(sprintf("Prepare writing %s file. Please wait...\n", type))
  # check if variables should be converted to factors
  for (i in 1:ncol(x)) {
    # haven labelled objects don't need conversion
    if (!is_labelled(x[[i]])) {
      # get variable value
      var.lab <- get_label(x[[i]])
      # convert variable to labelled factor, so it can be saved
      x[[i]] <- suppressWarnings(to_label(x[[i]]))
      # set back variable label
      x[[i]] <- set_label(x[[i]], var.lab, "label")
    }
    # update progress bar
    utils::setTxtProgressBar(pb, i)
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
