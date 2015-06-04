#' @docType data
#' @title Sample dataset from the EUROFAMCARE project
#' @name efc
#' @keywords data
#'
#' @description A SPSS sample data set, read with the \code{\link[haven]{read_spss}}
#'                function and "converted" with \code{\link{to_sjPlot}}.
#'
#' @references \url{http://www.uke.de/eurofamcare/}
#'
#' @examples
#' # Attach EFC-data
#' data(efc)
#'
#' # Show structure
#' str(efc)
#'
#' # show first rows
#' head(efc)
#'
#' # show variables
#' \dontrun{
#' library(sjPlot)
#' view_spss(efc)
#'
#' # show variable labels
#' get_var_labels(efc)
#'
#' # plot efc-data frame summary
#' sjt.df(efc, alternateRowColor=TRUE)}
#'
NULL

