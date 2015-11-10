#' @title Create a labelled vector
#' @name labelled
#'
#' @description A labelled vector is a common data structure in other statistical
#' environments.
#'
#' @param x Vector to label. Must be either numeric or character.
#' @param labels Named vector. The vector should be the same type as
#'   \code{x}. Unlike factors, labels don't need to be exhaustive: only a fraction
#'   of the values might be labelled.
#' @param is_na Optionally, logical vector describing which levels should
#'   be translated to missing values
#'
#' @note This method is derived from the \code{\link[haven]{labelled}} method
#'         of the \pkg{haven} package. \pkg{haven} up to version 0.2 \emph{does not}
#'         support the \code{is_na} attribute, however, the current
#'         \href{github.com/hadley/haven}{dev-version} does. Some of the
#'         \pkg{sjmisc} functions make use of this feature in advance, assuming
#'         that the \code{labelled} class supported by the \pkg{haven} package
#'         will be enhanced accordingly in a forthcoming update. Once the
#'         \pkg{haven} package is updated and introducing the new \code{labelled}
#'         class, this method might be removed.
#'
#' @examples
#' # labelled vector with multiple types of missing values
#' x <- labelled(c("M", "M", "F", "X", "N/A"),
#'               c(Male = "M", Female = "F", Refused = "X", "Not applicable" = "N/A"),
#'               c(FALSE, FALSE, TRUE, TRUE))
#'
#' x <- labelled(c(1, 2, 1, 5, 1, 5, 9),
#'               c(Male = 1, Female = 2, Refused = 5, Missing = 9),
#'               c(FALSE, FALSE, TRUE, TRUE))
#'
#' @export
labelled <- function(x, labels, is_na = NULL) {
  if (!is.numeric(x) && !is.character(x)) {
    stop("`x` must be either numeric or a character vector", call. = FALSE)
  }
  if (typeof(x) != typeof(labels)) {
    stop("`x` and `labels` must be same type", call. = FALSE)
  }
  if (is.null(labels)) {
    stop("`labels` must be a named vector", call. = FALSE)
  }
  if (is.null(is_na)) {
    is_na <- rep(FALSE, length(labels))
  } else {
    if (!is.logical(is_na) || length(is_na) != length(labels)) {
      stop("`is_na` must be a logical vector the same length as `labels`",
           call. = FALSE)
    }
  }

  structure(x,
            labels = labels,
            is_na = is_na,
            class = "labelled"
  )
}
