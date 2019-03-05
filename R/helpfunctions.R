#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


#' @importFrom sjlabelled set_na
#' @export
sjlabelled::set_na


#' @importFrom dplyr quos select
get_dot_data <- function(x, qs) {
  if (sjmisc::is_empty(qs))
    x
  else
    suppressMessages(dplyr::select(x, !!!qs))
}


data_frame <- function(...) {
  x <- data.frame(..., stringsAsFactors = FALSE)
  rownames(x) <- NULL
  x
}
