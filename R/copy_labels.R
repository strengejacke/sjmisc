#' @title Copy value and variable labels to (subsetted) data frames
#' @name copy_labels
#'
#' @description Subsetting-functions usually drop value and variable labels from
#'                subsetted data frames (if the original data frame has value and variable
#'                label attributes). This function copies these value and variable
#'                labels back to subsetted data frames that have been subsetted, for instance,
#'                with \code{\link{subset}}.
#'                \cr \cr
#'                In case \code{df_origin = NULL}, all possible label attributes
#'                from \code{df_new} are removed.
#'
#' @seealso \href{http://www.strengejacke.de/sjPlot/labelleddata/}{sjPlot-manual}
#'            on working with labelled data, and \code{\link{remove_all_labels}} for
#'            removing label attributes from data frames.
#'
#' @param df_new The new, subsetted data frame.
#' @param df_origin The original data frame where the subset (\code{df_new}) stems from;
#'          use \code{NULL}, if value and variable labels from \code{df_new} should be removed.
#' @return Returns \code{df_new} with either removed value and variable label attributes
#'           (if \code{df_origin = NULL}) or with copied value and variable label
#'           attributes (if \code{df_origin} was the original subsetted data frame).
#'
#' @note In case \code{df_origin = NULL}, all possible label attributes
#'         from \code{df_new} are removed. dplyr >= 0.4.2 no longer drops
#'         vector attributes; you'll only need to copy labels when using
#'         dplyr up to 0.4.1.
#'
#' @examples
#' data(efc)
#' efc.sub <- subset(efc, subset = e16sex == 1, select = c(4:8))
#' str(efc.sub)
#'
#' efc.sub <- copy_labels(efc.sub, efc)
#' str(efc.sub)
#'
#' efc.sub <- copy_labels(efc.sub)
#' str(efc.sub)
#'
#' @export
copy_labels <- function(df_new, df_origin = NULL) {
  # check if old df is NULL. if so, we remove all labels
  # from the data frame.
  if (is.null(df_origin)) {
    # tell user
    message("Removing all variable and value labels from data frame.")
    # remove all labels
    df_new <- remove_all_labels(df_new)
  } else {
    # check params
    if (is.data.frame(df_new) && is.data.frame(df_origin)) {
      # retrieve variables of subsetted data frame
      cn <- colnames(df_new)
      # check for valid colnames, i.e. if all column
      # names really match the original column names.
      if (sum(cn %in% colnames(df_origin) == F) > 0) {
        # if not, return only matching colnames
        cn <- cn[cn %in% colnames(df_origin)]
      }
      # get var-labels of original data frame, and select only those
      # labels from variables that appear in the new (subsetted) data frame
      df_new <- set_label(df_new, get_label(df_origin[, cn]))
      # same for value labels
      df_new <- set_labels(df_new, get_labels(df_origin[, cn],
                                              attr.only = TRUE,
                                              include.values = NULL,
                                              include.non.labelled = FALSE))
    } else {
      warning("both 'df_origin' and 'df_new' must be of class 'data.frame'.", call. = F)
    }
  }
  return(df_new)
}
