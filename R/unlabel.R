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
#' @seealso \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data initialization}
#'
#' @param x data frame, which contains \code{\link[haven]{labelled}} class vectors or a single vector
#'          of class \code{labelled}.
#' @return a data frame or single vector (depending on \code{x}) with common object classes.
#'
#' @note This function is currently only used to avoid possible compatibility issues
#'         with \code{\link[haven]{labelled}} class vectors and \code{tbl_df} resp.
#'         \code{tbl} class attributes for data frames. Some known issues with \code{\link[haven]{labelled}}
#'         class vectors have already been fixed, so it might be that this function
#'         will become redundant in the future. Currently, data frames with \code{tbl_df} and
#'         \code{tbl} class attributes may cause difficulties when indexing columns
#'         like \code{data.frame[, colnr]} - only \code{data.frame[[colnr]]} seems
#'         to be safe when accessing data frame columns from within function calls.
#'         \cr \cr
#'         Data frames will be converted into labelled data frames (see \code{\link{lbl_df}}).
#'
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export
unlabel <- function(x) {
  # -------------------------------------
  # check if complete data frame or only single
  # vector should be converted
  # -------------------------------------
  if (is.data.frame(x) || is.matrix(x)) {
    # -------------------------------------
    # create progress bar
    # -------------------------------------
    pb <- utils::txtProgressBar(min = 0,
                                max = ncol(x),
                                style = 3)
    # tell user...
    message("Converting labelled-classes. Please wait...\n")
    for (i in 1:ncol(x)) {
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
  return(x)
}
