#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


#' @importFrom sjlabelled set_na
#' @export
sjlabelled::set_na


#' @importFrom dplyr quos enquos select
get_dot_data <- function(x, qs) {
  if (sjmisc::is_empty(qs))
    x
  else
    # In case dots are not empty, get subdataframe of dots selection in function build_dot_data
    build_dot_data(x, qs)
}


#' @importFrom dplyr bind_cols
build_dot_data <- function(x, qs) {
  # below lapply goes through each of the expressions in dots data
  # and the corresponding subdataframes got with build_col are saved in list out
  out <- lapply(qs, function(dot_expr) build_col(x, dot_expr))
  out <- dplyr::bind_cols(out)
  attr(out, "row.names") <- attr(x, "row.names")
  out
}

# depending on the dots expressions, old variables are selected or new ones are computed with transmute
build_col <- function(x, qs_expr) {
  if (check_qs(rlang::get_expr(qs_expr))) {
    suppressMessages(dplyr::select(x, !!qs_expr))
  } else {
    suppressMessages(dplyr::transmute(x, !!qs_expr))
  }
}

#' @importFrom tidyselect vars_select_helpers
# name or numeric expressions, regular sequences or those looked for with select helpers are selected
# otherwise, they are transmuted
check_qs <- function(is_expr) {
  if (class(is_expr) != "call") {
    TRUE
  } else if (as.character(is_expr)[1] == ":") {
    TRUE
  } else if (as.character(is_expr)[1] %in% paste0(c(rep("",length(tidyselect::vars_select_helpers)),rep("dplyr::",length(tidyselect::vars_select_helpers)),rep("tidyselect::",length(tidyselect::vars_select_helpers))),names(tidyselect::vars_select_helpers))) {
    TRUE
  } else {
    FALSE
  }
}


data_frame <- function(...) {
  x <- data.frame(..., stringsAsFactors = FALSE)
  rownames(x) <- NULL
  x
}



n_unique <- function(x, na.rm = TRUE) {
  x <- as.vector(x)
  if (na.rm) x <- stats::na.omit(x)
  length(unique(x))
}
