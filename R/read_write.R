#' @title Import data from other statistical software packages
#' @name read_spss
#'
#' @description Import data from SPSS, SAS or Stata, including NA's, value and variable
#'   labels.
#'
#' @seealso Vignette \href{../doc/intro_sjmisc.html}{Labelled Data and the sjmisc-Package}.
#'
#' @param path File path to the data file.
#' @param atomic.to.fac Logical, if \code{TRUE}, categorical variables imported
#'   from the dataset (which are imported as \code{\link{atomic}}) will be
#'   converted to factors.
#' @param tag.na Logical, if \code{TRUE}, missing values are imported
#'          as \code{\link[haven]{tagged_na}} values; else, missing values are
#'          converted to regular \code{NA} (default behaviour).
#' @param path.cat Optional, the file path to the SAS catalog file.
#' @param enc The character encoding used for the file. This defaults to the encoding
#'          specified in the file, or UTF-8. Use this argument to override the default
#'          encoding stored in the file.
#'
#' @return A data frame containing the imported, labelled data. Retrieve value labels with
#'   \code{\link{get_labels}} and variable labels with \code{\link{get_label}}.
#'
#' @note These are wrapper functions for \CRANpkg{haven}'s \code{read_*}-functions.
#'
#' @details These read-functions behave slightly differently from \pkg{haven}'s
#'   read-functions:
#'   \itemize{
#'     \item The vectors in the returned data frame are of class \code{atomic}, not of class \code{labelled}. The labelled-class might cause issues with other packages.
#'     \item When importing SPSS data, variables with user defined missings \emph{won't} be read into \code{labelled_spss} objects, but imported as \emph{tagged NA values}.
#'   }
#'   The \code{atomic.to.fac} option only
#'   converts those variables into factors that are of class \code{atomic} and
#'   which have value labels after import. Atomic vectors without value labels
#'   are considered as continuous and not converted to factors.
#'
#' @examples
#' \dontrun{
#' # import SPSS data set. uses haven's read function
#' mydat <- read_spss("my_spss_data.sav")
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
#' @importFrom haven read_spss read_sas read_dta
#' @export
read_spss <- function(path, atomic.to.fac = FALSE, tag.na = FALSE) {
  # read data file
  data.spss <- haven::read_spss(file = path, user_na = tag.na)
  # prepare tagged NA?
  if (tag.na) {
    # remember all-NA values
    all_missings <- c()
    # convert NA for all variables
    for (i in seq_len(ncol(data.spss))) {
      # get variable
      x <- data.spss[[i]]
      # has variable ONLY missings?
      if (all(is.na(x))) {
        all_missings <- c(all_missings, i)
      } else {
        # get NA values
        na.values <- attr(x, "na_values", exact = TRUE)
        na.range <- attr(x, "na_range", exact = TRUE)
        # has any NA values?
        if (!is.null(na.values)) {
          # get label attr
          labels <- attr(x, "labels", exact = TRUE)
          # create tagged NA
          tna <- haven::tagged_na(as.character(na.values))
          # replace values with tagged NA
          for (j in seq_len(length(na.values))) {
            x[x == na.values[j]] <- tna[j]
          }
          # do we have any labels?
          if (!is.null(labels)) {
            # get missing labels
            na.val.labels <- names(labels)[labels %in% na.values]
            # do we have any labels for missings? then name tagged
            # NA with value labels, else use values as labels
            empty_val_labels <- sjmisc::is_empty(na.val.labels)
            if (length(na.val.labels) > 0 && !empty_val_labels)
              names(tna) <- na.val.labels
            else
              names(tna) <- na.values
            # add/replace value labeld for tagged NA
            labels <- c(labels[!labels %in% na.values], tna)
          } else {
            # use values as names, if we don't have value labels
            names(tna) <- na.values
            labels <- tna
          }
          # set back attribute
          attr(x, "labels") <- labels
        }
        # do we have NA range?
        if (!is.null(na.range)) {
          # check if any of the missing range values actually exists in data
          min.range.start <- min(na.range[!is.infinite(na.range)], na.rm = T)
          max.range.end <- max(na.range[!is.infinite(na.range)], na.rm = T)
          # we start with range up to highest value
          if (any(na.range == Inf) && min.range.start <= max(x, na.rm = TRUE)) {
            x <- set_na(x, na = sort(stats::na.omit(unique(x[x >= min.range.start]))), as.tag = TRUE)
          }
          # we start with range up to highest value
          if (any(na.range == -Inf) && max.range.end >= min(x, na.rm = TRUE)) {
            x <- set_na(x, na = sort(stats::na.omit(unique(x[x <= max.range.end]))), as.tag = TRUE)
          }
          # here we have no infinite value range
          if (!any(is.infinite(na.range))) {
            x <- set_na(x, na = sort(stats::na.omit(unique(c(
              na.range[!is.infinite(na.range)], x[x >= min.range.start & x <= max.range.end]
            )))), as.tag = TRUE)
          }
        }
        # finally, copy x back to data frame
        if (!is.null(na.range) || !is.null(na.values)) data.spss[[i]] <- x
      }
    }
    # do we have any "all-missing-variables"?
    if (!sjmisc::is_empty(all_missings)) {
      message(sprintf("Following %i variables have only missing values:", length(all_missings)))
      cat(paste(all_missings, collapse = ", "))
      cat("\n")
    }
  }
  # convert to sjPlot
  data.spss <- unlabel(data.spss)
  # convert atomic values to factors
  if (atomic.to.fac) data.spss <- atomic_to_fac(data.spss, getValLabelAttribute(data.spss))
  # return data frame
  data.spss
}


# converts atomic numeric vectors into factors with
# numerical factor levels
#' @importFrom utils txtProgressBar setTxtProgressBar
atomic_to_fac <- function(data.spss, attr.string) {
  # check for valid attr.string
  if (!is.null(attr.string)) {
    # create progress bar
    pb <- utils::txtProgressBar(min = 0, max = ncol(data.spss), style = 3)
    # tell user...
    message("Converting atomic to factors. Please wait...\n")
    # iterate all columns
    for (i in seq_len(ncol(data.spss))) {
      # copy column to vector
      x <- data.spss[[i]]
      # capture value labels attribute first
      labs <- attr(x, attr.string, exact = T)
      # and save variable label, if any
      lab <- attr(x, "label", exact = T)
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
        # any variable label?
        if (!is.null(lab)) attr(x, "label") <- lab
        # copy vector back to data frame
        data.spss[[i]] <- x
      }
      # update progress bar
      utils::setTxtProgressBar(pb, i)
    }
    close(pb)
  }

  data.spss
}


#' @rdname read_spss
#' @export
read_sas <- function(path, path.cat = NULL, atomic.to.fac = FALSE, enc = NULL) {
  # read data file
  data <- haven::read_sas(data_file = path, catalog_file = path.cat, encoding = enc)

  # find all-NA values
  len <- nrow(data)
  all_missings <- names(which(unlist(lapply(data, function(x) sum(is.na(x)) == len)) == TRUE))

  # do we have any "all-missing-variables"?
  if (!sjmisc::is_empty(all_missings)) {
    message(sprintf("Following %i variables have only missing values:", length(all_missings)))
    cat(paste(all_missings, collapse = ", "))
    cat("\n")
  }

  # convert to sjPlot
  data <- unlabel(data)

  # convert atomic values to factors
  if (atomic.to.fac) data <- atomic_to_fac(data, getValLabelAttribute(data))

  # return data frame
  data
}


#' @rdname read_spss
#' @export
read_stata <- function(path, atomic.to.fac = FALSE, enc = NULL) {
  # read data file
  data <- haven::read_dta(file = path, encoding = enc)

  # find all-NA values
  len <- nrow(data)
  all_missings <- names(which(unlist(lapply(data, function(x) sum(is.na(x)) == len)) == TRUE))

  # do we have any "all-missing-variables"?
  if (!sjmisc::is_empty(all_missings)) {
    message(sprintf("Following %i variables have only missing values:", length(all_missings)))
    cat(paste(all_missings, collapse = ", "))
    cat("\n")
  }

  # convert to sjPlot
  data <- unlabel(data)

  # convert atomic values to factors
  if (atomic.to.fac) data <- atomic_to_fac(data, getValLabelAttribute(data))

  # return data frame
  data
}


#' @title Write data to other statistical software packages
#' @name write_spss
#'
#' @description These functions write the content of a data frame to an SPSS, SAS or
#'                Stata-file.
#'
#' @note You don't need to take care whether variables have been imported with
#'         the \code{read_*} function from this package or from \pkg{haven}
#'         or even the \pkg{foreign} package, or if you have imported data and
#'         created new variables. These functions do all necessary data preparation
#'         to write a properly labelled data file.
#'
#' @param x A data frame that should be saved as file.
#' @param path File path of the output file.
#' @param version File version to use. Supports versions 8-14.
#'
#' @inheritParams to_label
#'
#' @export
write_spss <- function(x, path, drop.na = FALSE) {
  write_data(x = x, path = path, type = "spss", version = 14, drop.na = drop.na)
}


#' @rdname write_spss
#' @export
write_stata <- function(x, path, drop.na = FALSE, version = 14) {
  write_data(x = x, path = path, type = "stata", version = version, drop.na = drop.na)
}


#' @rdname write_spss
#' @export
write_sas <- function(x, path, drop.na = FALSE) {
  write_data(x = x, path = path, type = "sas", version = 14, drop.na = drop.na)
}


#' @importFrom haven write_sav write_dta write_sas
write_data <- function(x, path, type, version, drop.na) {
  # convert data to labelled
  x <- to_label(x, add.non.labelled = T, drop.na = drop.na)

  # check for correct column names
  for (i in seq_len(ncol(x))) {
    # check column name
    end.point <- colnames(x)[i]
    # if it ends with a dot, add a char. dot is invalid last char for SPSS
    if (substr(end.point, nchar(end.point), nchar(end.point)) == ".") {
      colnames(x)[i] <- paste0(end.point, i)
    }
  }

  # tell user
  message(sprintf("Writing %s file to '%s'. Please wait...", type, path))

  if (type == "spss") {
    # write SPSS
    haven::write_sav(data = x, path = path)
  } else if (type == "stata") {
    # write Stata
    haven::write_dta(data = x, path = path, version = version)
  } else if (type == "sas") {
    # write Stata
    haven::write_sas(data = x, path = path)
  }
}
