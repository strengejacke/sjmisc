#' @title Weight a variable
#' @name weight2
#'
#' @description This function weights the variable \code{x} by
#'                a specific vector of \code{weights}. It's an
#'                alternative weight calculation to \code{\link{weight}},
#'                though \code{\link{weight}} usage is recommended.
#'
#' @seealso \code{\link{weight}}
#'
#' @inheritParams weight
#'
#' @return The weighted \code{x}.
#'
#' @details This function sums up all \code{weights} values of the associated
#'            categories of \code{x}, whereas the \code{\link{weight}} function
#'            uses a \code{\link{xtabs}} formula to weight cases. Thus, this function
#'            may return a vector of different length than \code{x}.
#'
#' @note See 'Note' in \code{\link{weight}}
#'
#' @examples
#' v <- sample(1:4, 20, TRUE)
#' table(v)
#' w <- abs(rnorm(20))
#' table(weight2(v, w))
#'
#' @export
weight2 <- function(x, weights) {
  items <- unique(x)
  newvar <- c()
  for (i in 1:length(items)) {
    newcount = round(sum(weights[which(x == items[i])]))
    newvar <- c(newvar, rep(items[i], newcount))
  }
  return(newvar)
}


#' @title Weight a variable
#' @name weight
#' @description This function weights the variable \code{x} by
#'                a specific vector of \code{weights}.
#'
#' @seealso \code{\link{weight2}}
#'
#' @param x (Unweighted) variable
#' @param weights Vector with same length as \code{x}, which
#'          contains weight factors. Each value of \code{x} has a
#'          specific assigned weight in \code{weights}.
#' @param digits Numeric value indicating the number of decimal places to be
#'          used for rounding the weighted values. By default, this value is
#'          \code{0}, i.e. the returned values are integer values.
#'
#' @return The weighted \code{x}.
#'
#' @note The values of the returned vector are in sorted order, whereas the values'
#'        order of the original \code{x} may be spread randomly. Hence, \code{x} can't be
#'        used, for instance, for further cross tabulation. In case you want to have
#'        weighted contingency tables or (grouped) box plots etc., use the \code{weightBy}
#'        argument of most functions.
#'
#' @examples
#' v <- sample(1:4, 20, TRUE)
#' table(v)
#' w <- abs(rnorm(20))
#' table(weight(v, w))
#'
#' @importFrom stats na.pass xtabs
#' @export
weight <- function(x, weights, digits = 0) {
  # init values
  weightedvar <- c()
  wtab <- round(stats::xtabs(weights ~ x,
                             data = data.frame(weights = weights, x = x),
                             na.action = stats::na.pass,
                             exclude = NULL),
                digits = digits)
  # iterate all table values
  for (w in 1:length(wtab)) {
    # retrieve count of each table cell
    w_count <- wtab[[w]]
    # retrieve "cell name" which is identical to the variable value
    w_value <- as.numeric(names(wtab[w]))
    # append variable value, repeating it "w_count" times.
    weightedvar <- c(weightedvar, rep(w_value, w_count))
  }
  return(weightedvar)
}
