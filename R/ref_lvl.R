#' @title Change reference level of (numeric) factors
#' @name ref_lvl
#'
#' @description Changes the reference level of numeric factor. See 'Details'.
#'
#' @seealso \code{\link{to_factor}} to convert numeric vectors into factors;
#'            \code{\link{rec}} to recode variables.
#'
#' @param x \code{\link{factor}} with numeric levels where a new reference
#'          level should be set.
#' @param value Numeric, the new reference level.
#' @return \code{x} with new reference level. See 'Details'.
#'
#' @details Unlike \code{\link[stats]{relevel}}, this function a) only accepts
#'            numeric factors and b) changes the reference level by recoding
#'            the factor's values using the \code{\link{rec}} function. Hence,
#'            all values from lowest up to the reference level indicated by
#'            \code{value} are recoded, with \code{value} starting as lowest
#'            factor value. See 'Examples'.
#'
#'
#' @examples
#' data(efc)
#' x <- to_factor(efc$e42dep)
#' str(x)
#' table(x)
#'
#' ref_lvl(x) <- 3
#' str(x)
#' table(x)
#'
#' @export
ref_lvl <- function(x, value = NULL) {
  # check correct arguments
  if (is.null(x)) {
    warning("`x` is NULL.", call. = F)
    return(x)
  }
  if (!is.factor(x)) {
    warning("`x` needs to be a factor.", call. = F)
    return(x)
  }
  if (!is_num_fac(x)) {
    warning("`x` needs to be a factor with numeric factor levels.", call. = F)
    return(x)
  }
  # get values from factor
  vals <- as.numeric(levels(x))
  # check if ref-lvl exists in values
  if (!value %in% vals) {
    warning("`x` has no factor level indicated by the reference level `value`.", call. = F)
    return(x)
  }
  # get value labels
  val.labs <- get_labels(x)
  # get variable label
  var.lab <- get_label(x)
  # find position of reference level
  refpos <- which(vals == value)
  # new order of factor levels, if reference level
  # is on first position
  neword <- c(vals[refpos], vals[-refpos])
  # now recode variable. therefore, we need a string pattern
  # for the recoding
  rec.pattern <- paste0(sprintf("%i=%i;", neword, vals), collapse = "")
  # recode now
  x <- rec(x, rec.pattern, as.fac = TRUE)
  # set back labels
  if (!is.null(var.lab) && !is_empty(var.lab)) {
    set_label(x) <- var.lab
  }
  if (!is.null(val.labs)) {
    # we need "order" twice here, because "neword" refers to the actual
    # values of "x", so "neword" might have negative values, or zero.
    # so we first need the "order" function to have numeric values from
    # 1 to length(x) - and a second "order" call to get the correct order
    # of these values.
    set_labels(x) <- val.labs[order(order(neword))]
  }
  return(x)
}

#' @rdname ref_lvl
#' @export
`ref_lvl<-` <- function(x, value) {
  UseMethod("ref_lvl<-")
}

#' @export
`ref_lvl<-.default` <- function(x, value) {
  x <- ref_lvl(x = x, value = value)
  x
}
