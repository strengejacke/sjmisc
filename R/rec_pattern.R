#' @title Create recode pattern for 'rec' function
#' @name rec_pattern
#'
#' @description Convenient function to create a recode pattern for the
#'                \code{\link{rec}} function, which recodes (numeric)
#'                vectors into smaller groups.
#'
#' @param from Minimum value that should be recoded.
#' @param to Maximum value that should be recoded.
#' @param width Numeric, indicating the range of each group.
#' @param other String token, indicating how to deal with all other values
#'          that have not been captured by the recode pattern. See 'Details'
#'          on the \code{else}-token in \code{\link{rec}}.
#' @return A list with two values:
#'           \describe{
#'            \item{\code{pattern}}{string pattern that can be used as \code{recodes} argument for the \code{\link{rec}}-function.}
#'            \item{\code{labels}}{the associated values labels that can be used with \code{\link{set_labels}}.}
#'           }
#'
#' @seealso \code{\link{group_var}} for recoding variables into smaller groups, and
#'           \code{\link{group_labels}} to create the asssociated value labels.
#'
#' @examples
#' rp <- rec_pattern(1, 100)
#' rp
#'
#' # sample data, inspect age of carers
#' data(efc)
#' table(efc$c160age, exclude = NULL)
#' table(rec(efc$c160age, rp$pattern), exclude = NULL)
#'
#' # recode carers age into groups of width 5
#' x <- rec(efc$c160age, rp$pattern)
#' # add value labels to new vector
#' set_labels(x) <- rp$labels
#' # watch result
#' frq(as_labelled(x))
#'
#' @export
rec_pattern <- function(from, to, width = 5, other = NULL){
  # init variables
  rec.pat <- c()
  rec.labels <- c()
  # create sequence of recode-groups
  values <- seq(from, to + width, by = width)
  # create pattern for each group
  for (x in 1:(length(values) - 1)) {
    rec.pat <- paste0(rec.pat,
                      sprintf("%i:%i=%i", values[x], values[x + 1] - 1, x),
                      sep = ";")
    # also create associated labels
    rec.labels <- c(rec.labels, sprintf("%i-%i", values[x], values[x + 1] - 1))
  }
  # do we have an "else"-token?
  if (!is.null(other) && !is_empty(other))
    rec.pat <- paste0(rec.pat, "else=", other, sep = "")
  # name labels
  names(rec.labels) <- c(1:(length(values) - 1))
  # return results
  list(pattern = rec.pat, labels = rec.labels)
}
