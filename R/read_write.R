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
#' @param path File path to the data file.
#' @param enc File encoding of the SPSS dataset. \emph{Not needed if \code{option = "haven"} (default).}
#' @param attach.var.labels Logical, if \code{TRUE}, variable labels will automatically be
#'          added to each variable as \code{"variable.label"} attribute. Use this
#'          parameter, if \code{option = "foreign"}, where variable labels are added
#'          as list-attribute to the imported data frame.
#'          \emph{Not needed if \code{option = "haven"} (default).}
#' @param atomic.to.fac Logical, if \code{TRUE}, factor variables imported from
#'          the dataset (which are imported as \code{\link{atomic}}) will be converted
#'          to \code{\link{factor}}s.
#' @param keep.na Logical, if \code{TRUE}, user-defined missing values will be
#'          left as their original codes. If \code{FALSE} (default), corresponding
#'          values are converted to \code{NA}.
#' @param option String, indicating which package will be used to read the SPSS data file.
#'          By default, \code{option = "haven"}, which means, the \code{read_spss} function
#'          from the \pkg{haven} package is used. Use \code{option = "foreign"} to
#'          use foreign's \code{\link[foreign]{read.spss}} function. Use \code{options(read_spss = "foreign")}
#'          to make this function always use the \pkg{foreign} package \code{\link[foreign]{read.spss}} function.
#' @return A data frame containing the SPSS data. Retrieve value labels with \code{\link{get_labels}}
#'   and variable labels with \code{\link{get_label}}.
#'
#' @note This is a wrapper function for \code{\link[haven]{read_spss}} of the
#'         \pkg{haven} package and \code{\link[foreign]{read.spss}} of the
#'         \pkg{foreign} package. This function adds value and variable
#'         labels as attributes to the imported variables of the data frame.
#'         \cr \cr
#'         Most functions of the \pkg{sjPlot} package access value and variable label
#'         attributes to automatically detect labels in order to set them as axis,
#'         legend or title labels in plots (\code{sjp.}-functions) respectively as
#'         column or row headers in table outputs (\code{sjt.}-functions).  See
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
#' @importFrom haven read_spss
#' @export
read_spss <- function(path,
                      enc = NA,
                      attach.var.labels = FALSE,
                      atomic.to.fac = FALSE,
                      keep.na = FALSE,
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
                                                     use.missings = !keep.na,
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
    # convert NA
    if (!keep.na) data.spss <- to_na(data.spss)
    # convert to sjPlot
    data.spss <- unlabel(data.spss)
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
      # capture labels attribute first
      labs <- attr(x, attr.string, exact = T)
      # is atomic, which was factor in SPSS?
      if (is.atomic(x) && !is.null(labs)) {
        # so we have value labels (only typical for factors, not
        # continuous variables) and a variable of type "atomic" (SPSS
        # continuous variables would be imported as numeric) - this
        # indicates we have a factor variable. now we convert to
        # factor
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
#' @param path.cat Optional, the file path to the SAS catalog file.
#' @return A data frame containing the SAS data. Retrieve value labels with \code{\link{get_labels}}
#'   and variable labels with \code{\link{get_label}}.
#'
#' @inheritParams read_spss
#'
#' @note This is a wrapper function for \code{\link[haven]{read_sas}} function of the
#'         \pkg{haven} package. This function converts the imported data
#'         into a common class format (see \code{\link{unlabel}}).
#'
#' @importFrom haven read_sas
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
  data <- unlabel(data)
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
#' @inheritParams read_spss
#'
#' @return A data frame containing the STATA data. Retrieve value labels with \code{\link{get_labels}}
#'   and variable labels with \code{\link{get_label}}.
#'
#' @note This is a wrapper function for \code{\link[haven]{read_dta}} function of the
#'         \pkg{haven} package. This function converts the imported data
#'         into a common class format (see \code{\link{unlabel}}).
#'
#' @importFrom haven read_dta
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
  data <- unlabel(data)
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
#'         the \code{\link{read_spss}} function from this package or from \pkg{haven}
#'         or even the \pkg{foreign} package, or if you have imported SPSS data and
#'         created new variables. This function does all necessary data preparation
#'         to write a properly labelled SPSS sav file.
#'
#' @param x \code{data.frame} that should be saved as file.
#' @param path File path of the output file.
#' @param enc.to.utf8 Logical, if \code{TRUE}, character encoding of variable and
#'          value labels will be converted to UTF-8.
#'
#' @export
write_spss <- function(x, path, enc.to.utf8 = TRUE) {
  write_data(x = x, path = path, type = "spss", enc.to.utf8 = enc.to.utf8)
}


#' @title Write content of data frame to STATA dta-file
#' @name write_stata
#'
#' @description This function saves the content of a data frame to an STATA dta-file.
#'
#' @seealso \code{\link{write_spss}}
#'
#' @note You don't need to take care whether variables have been imported with
#'         the \code{\link{read_stata}} function from this package or from \pkg{haven},
#'         or if you have imported STATA data and
#'         created new variables. This function does all necessary data preparation
#'         to write a properly labelled STATA file.
#'
#' @inheritParams write_spss
#'
#' @export
write_stata <- function(x, path, enc.to.utf8 = TRUE) {
  write_data(x = x, path = path, type = "stata", enc.to.utf8 = enc.to.utf8)
}


#' @importFrom haven write_sav write_dta
#' @importFrom utils txtProgressBar setTxtProgressBar
write_data <- function(x, path, type, enc.to.utf8) {
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
    # get value and variable labels
    val.lab <- get_labels(x[[i]], include.values = "n")
    var.lab <- get_label(x[[i]])
    # Encode to UTF-8
    if (enc.to.utf8) {
      if (!is.null(val.lab)) x[[i]] <- set_labels(x[[i]], enc2utf8(val.lab))
      if (!is.null(var.lab)) x[[i]] <- set_label(x[[i]], enc2utf8(var.lab))
    }
    # haven labelled objects don't need conversion
    if (!is_labelled(x[[i]])) {
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
