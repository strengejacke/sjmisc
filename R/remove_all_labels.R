#' @title Remove value and variable labels from vector or data frame
#' @name remove_all_labels
#'
#' @description This function removes value and variable label attributes
#'                from a vector or data frame. These attributes are typically
#'                added to variables when importing foreign data (see
#'                \code{\link{read_spss}}) or manually adding label attributes
#'                with \code{\link{set_labels}}.
#'
#' @seealso \href{http://www.strengejacke.de/sjPlot/labelleddata/}{sjPlot-manual}
#'            on working with labelled data, and \code{\link{copy_labels}} for
#'            adding label attributes (subsetted) data frames.
#'
#' @param x Vector or \code{data.frame} with variable and/or value label attributes
#' @return \code{x} with removed value and variable label attributes.
#'
#' @examples
#' data(efc)
#' str(efc)
#' str(remove_all_labels(efc))
#'
#' @export
remove_all_labels <- function(x) {
  if (is.data.frame(x) || is.list(x))
    x <- tibble::as_tibble(lapply(x, FUN = remove_all_labels_helper))
  else
    x <- remove_all_labels_helper(x)
  return(x)
}


remove_all_labels_helper <- function(x) {
  # find label-attribute string
  attr.string <- getValLabelAttribute(x)
  # remove attributes
  if (!is.null(attr.string)) attr(x, attr.string) <- NULL
  # find label-attribute string
  attr.string <- getVarLabelAttribute(x)
  # remove attributes
  if (!is.null(attr.string)) attr(x, attr.string) <- NULL
  # unclass, if labelled. labelled class may throw
  # errors / warnings, when not havin label attributes
  if (haven::is.labelled(x)) x <- unclass(x)
  # return var
  return(x)
}
