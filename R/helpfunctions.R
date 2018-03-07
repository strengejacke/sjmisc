#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


#' @importFrom tibble glimpse
#' @export
tibble::glimpse


#' @importFrom dplyr quos select
get_dot_data <- function(x, qs) {
  if (sjmisc::is_empty(qs))
    x
  else
    suppressMessages(dplyr::select(x, !!!qs))
}
