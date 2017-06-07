#' @title Convert labelled vectors into normal classes
#' @name unlabel
#'
#' @description This function converts \code{\link[haven]{labelled}} class vectors
#'                into a generic data format, which means that simply all \code{\link[haven]{labelled}}
#'                class attributes will be removed, so all vectors / variables will most
#'                likely become \code{\link{atomic}}. Additionally, \code{tbl_df} and
#'                \code{tbl} class attributes will be removed from data frames, and
#'                a \code{\link{lbl_df}} class attribute will be added. See 'Note'.
#'
#' @param x A data frame, which contains \code{\link[haven]{labelled}} class
#'          vectors or a single vector of class \code{labelled}.
#'
#' @return A data frame or single vector (depending on \code{x}) with common object classes.
#'
#' @note This function is currently only used to avoid possible compatibility issues
#'         with \code{\link[haven]{labelled}} class vectors and \code{tbl_df} resp.
#'         \code{tbl} class attributes for data frames. Some known issues with \code{\link[haven]{labelled}}
#'         class vectors have already been fixed, so it might be that this function
#'         will become redundant in the future. Currently, data frames with \code{tbl_df} and
#'         \code{tbl} class attributes may cause difficulties when indexing columns,
#'         because the \pkg{haven} package's \code{read_*}-functions return a \code{tibble},
#'         which never simplifies data frame when indexing like \code{data.frame[, colnr]}.
#'         Thus, packages or functions that expect \code{data.frame[, colnr]} to return a
#'         vector, might throw an error - only \code{data.frame[[colnr]]} is safe
#'         when accessing data frame columns from within function calls (see
#'         \code{\link[tibble]{tibble-package}} for details).
#'         \cr \cr
#'         Data frames will be converted into labelled data frames (see \code{\link{lbl_df}}).
#'
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export
unlabel <- function(x) {
  .Deprecated("unlabel", package = "sjlabelled", msg = "This function will be removed in future versions of sjmisc and has been moved to package 'sjlabelled'. Please use sjlabelled::unlabel() instead.")

  # check if complete data frame or only single
  # vector should be converted
  if (is.data.frame(x)) {
    # create progress bar
    pb <- utils::txtProgressBar(min = 0, max = ncol(x), style = 3)
    # tell user...
    message("Converting labelled-classes. Please wait...\n")
    for (i in seq_len(ncol(x))) {
      # remove labelled class
      if (is_labelled(x[[i]])) x[[i]] <- unclass(x[[i]])
      # update progress bar
      utils::setTxtProgressBar(pb, i)
    }
    close(pb)
    # remove redundant class attributes
    class(x) <- c("lbl_df", "data.frame")
  } else {
    # remove labelled class
    if (is_labelled(x)) x <- unclass(x)
  }

  x
}
