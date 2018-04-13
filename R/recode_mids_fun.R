#' @importFrom dplyr group_by
#' @importFrom tidyr nest
prepare_mids_recode <- function(x) {
  # check if suggested package is available
  if (!requireNamespace("mice", quietly = TRUE))
    stop("Package `mice` needed for this function to work. Please install it.", call. = FALSE)

  # check classes
  if (!inherits(x, "mids"))
    stop("`x` must be a `mids`-object, as returned by the `mice()`-function.", call. = F)


  # convert mids into long-data.frame

  long <- mice::complete(x, action = "long", include = TRUE)


  # group by imputation, so we can easily iterate each imputed dataset

  long %>%
    dplyr::group_by(.data$.imp) %>%
    tidyr::nest()
}


#' @importFrom purrr map
#' @importFrom tidyr unnest
final_mids_recode <- function(x) {
  # check if suggested package is available

  if (!requireNamespace("mice", quietly = TRUE))
    stop("Package `mice` needed for this function to work. Please install it.", call. = FALSE)


  # return mids-object. need to use "as.data.frame()",
  # because "as.mids()" can't cope with tibbles

  x %>%
    tidyr::unnest() %>%
    as.data.frame() %>%
    mice::as.mids()
}
