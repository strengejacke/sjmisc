#' @title Retrieve notes (annotations) from labelled variables
#' @name get_note
#'
#' @description This function retrieves the value of the \code{note}-attribute
#'                of vectors.
#'
#' @param x Variable (vector) with note-attribute.
#' @return The the value of the \code{note}-attribute of \code{x}.
#'
#' @examples
#' # create labelled factor, with missing flag
#' x <- labelled(c("M", "M", "F", "X", "N/A"),
#'               c(Male = "M", Female = "F",
#'                 Refused = "X", "Not applicable" = "N/A"),
#'               c(FALSE, FALSE, TRUE, TRUE))
#'
#' set_label(x) <- "A labelled vector with note"
#' set_note(x) <- "Test annotation."
#' get_note(x)
#' x
#'
#' @export
get_note <- function(x) {
  return(attr(x, "note", exact = TRUE))
}


#' @title Add notes (annotations) to (labelled) variables
#' @name set_note
#'
#' @description This function adds a note (string) as \code{note}-attribute
#'                to \code{x}.
#'
#' @param x Variable (vector).
#' @param value The note (annotation) as character string that will be added as
#'          \code{note}-attribute to \code{x}.
#'
#' @return \code{x}, with \code{value} stored as attribute.
#'
#' @examples
#' # create labelled factor, with missing flag
#' x <- labelled(c("M", "M", "F", "X", "N/A"),
#'               c(Male = "M", Female = "F",
#'                 Refused = "X", "Not applicable" = "N/A"),
#'               c(FALSE, FALSE, TRUE, TRUE))
#'
#' set_label(x) <- "A labelled vector with note"
#' set_note(x) <- "Test annotation."
#' get_note(x)
#' x
#'
#' @export
set_note <- function(x, value = NULL) {
  if (is.null(value) || is_empty(value)) {
    attr(x, "note") <- NULL
  } else {
    attr(x, "note") <- value
  }
  x
}

#' @rdname set_note
#' @export
`set_note<-` <- function(x, value) {
  UseMethod("set_note<-")
}

#' @export
`set_note<-.default` <- function(x, value) {
  x <- set_note(x = x, value = value)
  x
}
