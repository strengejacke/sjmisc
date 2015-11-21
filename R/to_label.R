#' @title Convert variable into factor and replaces values with associated value labels
#' @name to_label
#'
#' @description This function converts (replaces) variable values (also of factors)
#'                with their associated value labels. Might be helpful for factor variables.
#'                For instance, if you have a Gender variable with 0/1 value, and associated
#'                labels are male/female, this function would convert all 0 to male and
#'                all 1 to female and returns the new variable as \code{\link{factor}}.
#'
#' @seealso \code{\link{to_factor}} to convert a numeric variable into a factor (and
#'            preserve labels) and \code{\link{to_value}} to convert a factor into
#'            a numeric variable.
#'
#' @param x variable of type \code{\link{numeric}}, \code{\link{atomic}},
#'          \code{\link{factor}} or \code{\link[haven]{labelled}}
#'          \emph{with associated value labels} (see \code{\link{set_labels}}),
#'          respectively a data frame with such variables.
#' @param add.non.labelled logical, if \code{TRUE}, values without associated
#'          value label will also be converted to labels (as is). See 'Examples'.
#' @param drop.na logical, if \code{TRUE}, all types of missing value codes are
#'          converted into NA before \code{x} is converted as factor. If
#'          \code{FALSE}, missing values will be left as their original codes.
#'          See 'Examples' and \code{\link{get_na}}.
#' @return A factor variable with the associated value labels as factor levels, or a
#'           data frame with such factor variables (if \code{x} was a data frame).
#'
#' @note Value and variable label attributes (see, for instance, \code{\link{get_labels}}
#'         or \code{\link{set_labels}}) will be removed  when converting variables to factors.
#'         \cr \cr
#'         Factors with non-numeric factor-levels won't be changed and returned "as is"
#'         (see 'Examples').
#'
#' @details See 'Details' in \code{\link{get_na}}.
#'
#' @examples
#' data(efc)
#' print(get_labels(efc)['c161sex'])
#' head(efc$c161sex)
#' head(to_label(efc$c161sex))
#'
#' print(get_labels(efc)['e42dep'])
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
#' \dontrun{
#' # factor with non-numeric levels won't be changed, either,
#' # however, a warning is produced
#' to_label(factor(c("a", "b", "c")))}
#'
#' # create vector
#' x <- c(1, 2, 3, 2, 4, NA)
#' # add less labels than values
#' x <- set_labels(x, c("yes", "maybe", "no"),
#'                 force.labels = FALSE,
#'                 force.values = FALSE)
#' # convert to label w/o non-labelled values
#' to_label(x)
#' # convert to label, including non-labelled values
#' to_label(x, add.non.labelled = TRUE)
#'
#'
#' # create labelled integer, with missing flag
#' x <- labelled(c(1, 2, 1, 3, 4, 1),
#'               c(Male = 1, Female = 2, Refused = 3, "N/A" = 4),
#'               c(FALSE, FALSE, TRUE, TRUE))
#' # to labelled factor, with missing labels
#' to_label(x, drop.na = FALSE)
#' # to labelled factor, missings removed
#' to_label(x, drop.na = TRUE)
#'
#' @export
to_label <- function(x, add.non.labelled = FALSE, drop.na = TRUE) {
  if (is.matrix(x) || is.data.frame(x)) {
    for (i in 1:ncol(x)) x[[i]] <- to_label_helper(x[[i]],
                                                   add.non.labelled,
                                                   drop.na)
    return(x)
  } else {
    return(to_label_helper(x,
                           add.non.labelled,
                           drop.na))
  }
}


to_label_helper <- function(x, add.non.labelled, drop.na) {
  # check if factor has numeric factor levels
  if (is.factor(x) && !is_num_fac(x)) {
    # if not, stop here - factor levels are already "labelled".
    warning("'x' may have numeric factor levels only.", call. = F)
    return(x)
  }
  # remove missings?
  if (drop.na) x <- to_na(x)
  # get value labels
  vl <- get_labels(x,
                   attr.only = TRUE,
                   include.values = NULL,
                   include.non.labelled = add.non.labelled)
  # check if we have any labels, else
  # return variable "as is"
  if (!is.null(vl)) {
    # get associated values for value labels
    vn <- get_values(x, sort.val = FALSE, drop.na = FALSE)
    # replace values with labels
    if (is.factor(x)) {
      # set new levels
      levels(x) <- vl
      # remove attributes
      x <- remove_all_labels(x)
    } else {
      for (i in 1:length(vl)) x[x == vn[i]] <- vl[i]
      # to factor
      x <- factor(x, levels = vl)
    }
  }
  # return as factor
  return(x)
}
