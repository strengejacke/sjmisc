#' @title Check whether two factors are crossed
#' @name is_crossed
#' @description This function checks whether two factors are crossed,
#'                i.e. if each level of one factor occurs in combination
#'                with each level of the other factor.
#'
#' @param f1 a numeric vector or \code{\link{factor}}.
#' @param f2 a numeric vector or \code{\link{factor}}.
#' @return Logical, \code{TRUE} if factors are crossed, \code{FALSE} otherwise.
#'
#' @seealso \code{\link{is_nested}}
#'
#' @references Grace, K. The Difference Between Crossed and Nested Factors. \href{http://www.theanalysisfactor.com/the-difference-between-crossed-and-nested-factors/}{(web)}
#'
#' @examples
#' # crossed factors, each category of
#' # x appears in each category of y
#' x <- c(1,4,3,2,3,2,1,4)
#' y <- c(1,1,1,2,2,1,2,2)
#' # show distribution
#' table(x, y)
#' # check if crossed
#' is_crossed(x, y)
#'
#' # not crossed factors
#' x <- c(1,4,3,2,3,2,1,4)
#' y <- c(1,1,1,2,1,1,2,2)
#' # show distribution
#' table(x, y)
#' # check if crossed
#' is_crossed(x, y)
#'
#' @export
is_crossed <- function(f1, f2) {
  tab <- table(f1, f2)
  # for crossed factors, we should have no zeros in any rows
  # (i.e. each level of f1 also contains any level of f2)
  return(!any(apply(tab, 1, function(x) any(x == 0)) == TRUE))
}


#' @title Check whether two factors are nested
#' @name is_nested
#' @description This function checks whether two factors are nested,
#'                i.e. if each category of the first factor co-occurs
#'                with only one category of the other.
#'
#' @param f1 a numeric vector or \code{\link{factor}}.
#' @param f2 a numeric vector or \code{\link{factor}}.
#' @return Logical, \code{TRUE} if factors are nested, \code{FALSE} otherwise.
#'
#' @note If factors are nested, a message is displayed to tell whether \code{f1}
#'         is nested within \code{f2} or vice versa.
#'
#' @seealso \code{\link{is_crossed}}
#'
#' @references Grace, K. The Difference Between Crossed and Nested Factors. \href{http://www.theanalysisfactor.com/the-difference-between-crossed-and-nested-factors/}{(web)}
#'
#' @examples
#' # nested factors, each category of
#' # x appears in one category of y
#' x <- c(1,2,3,4,5,6,7,8,9)
#' y <- c(1,1,1,2,2,2,3,3,3)
#' # show distribution
#' table(x, y)
#' # check if nested
#' is_nested(x, y)
#' is_nested(y, x)
#'
#' # not nested factors
#' x <- c(1,2,3,4,5,6,7,8,9,1,2)
#' y <- c(1,1,1,2,2,2,3,3,3,2,3)
#' # show distribution
#' table(x, y)
#' # check if nested
#' is_nested(x, y)
#' is_nested(y, x)
#'
#' @export
is_nested <- function(f1, f2) {
  tab <- table(f1, f2)
  # cross tabulation of nested factors should have only 1 value per row
  # (or column) that is not zero. If we found more, factors are not nested
  # or rows and columns have to be swapped.
  # check if f1 is nested within f2
  nested <- !any(apply(tab, 1, function(x) sum(x != 0) > 1))
  if (nested) message("'f1' is nested within 'f2'")
  # swap rows and columns to check whether factors are nested
  # check whether f2 is nested within f1
  if (!nested) {
    nested <- !any(apply(tab, 2, function(x) sum(x != 0) > 1))
    if (nested) message("'f2' is nested within 'f1'")
  }
  return(nested)
}


#' @title Check whether value is even
#' @name is_even
#'
#' @param x numeric vector or single numeric value
#'
#' @return \code{TRUE} for each even value of \code{x}, \code{FALSE} for
#'           odd values.
#'
#' @seealso \code{\link{is_odd}}
#'
#' @examples
#' is_even(4)
#' is_even(5)
#' is_even(1:4)
#'
#' @export
is_even <- function(x) (x %% 2) == 0


#' @title Check whether value is odd
#' @name is_odd
#'
#' @param x numeric vector or single numeric value
#'
#' @return \code{TRUE} for each odd value of \code{x}, \code{FALSE} for
#'           even values.
#'
#' @seealso \code{\link{is_even}}
#'
#' @examples
#' is_odd(4)
#' is_odd(5)
#' is_odd(1:4)
#'
#' @export
is_odd <- function(x) (x %% 2) == 1


#' @title Check whether a factor has numeric levels only
#' @name is_num_fac
#' @description This function checks whether a factor has only numeric or
#'                any non-numeric factor levels.
#'
#' @param x a \code{\link{factor}}.
#' @return Logical, \code{TRUE} if factor has numeric factor levels only,
#'           \code{FALSE} otherwise.
#'
#' @examples
#' # numeric factor levels
#' f1 <- factor(c(NA, 1, 3, NA, 2, 4))
#' is_num_fac(f1)
#'
#' # not completeley numeric factor levels
#' f2 <- factor(c(NA, "C", 1, 3, "A", NA, 2, 4))
#' is_num_fac(f2)
#'
#' # not completeley numeric factor levels
#' f3 <- factor(c("Justus", "Bob", "Peter"))
#' is_num_fac(f3)
#'
#' @export
is_num_fac <- function(x) {
  # check if we have numeric levels
  return(!anyNA(suppressWarnings(as.numeric(levels(x)))))
}


#' @title Check whether string is empty
#' @name is_empty
#' @description This function checks whether a string or character vector (of
#'                length 1) is empty or not.
#'
#' @param x a string or character vector of length 1.
#' @return Logical, \code{TRUE} if \code{x} is empty, \code{FALSE} otherwise.
#'
#' @note \code{NULL}- or \code{NA}-values are also considered as "empty" (see
#'         'Examples') and will return \code{TRUE}.
#'
#' @examples
#' x <- "test"
#' is_empty(x)
#'
#' x <- ""
#' is_empty(x)
#'
#' x <- NA
#' is_empty(x)
#'
#' x <- NULL
#' is_empty(x)
#'
#' # string is not empty
#' is_empty(" ")
#'
#' # however, this trimmed string is
#' is_empty(trim(" "))
#'
#' @export
is_empty <- function(x) {
  if (!is.null(x) && length(x) > 1) warning("'x' must be of length 1. Evaluating first element only.", call. = F)
  return(is.null(x) || nchar(x) == 0 || is.na(x))
}


# this function returns TRUE, if a vector is
# of class "labelled" (haven package)
is_labelled <- function(x) {
  # check if object has multiple class attributes
  if (length(class(x)) > 1) return(any(class(x) == "labelled"))
  # return if labelled
  return(class(x) == "labelled")
}
