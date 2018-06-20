#' @title Check if variables have missing / infinite values
#' @name has_na
#'
#' @description This functions checks if variables in a data frame have \code{NA},
#'   \code{NaN} or \code{Inf} values.
#'
#' @param x A data frame.
#' @param out Output (return) format of the results. May be abbreviated.
#'
#' @inheritParams descr
#'
#' @return If \code{x} is a vector, returns \code{TRUE} if \code{x} has any missing
#'   or infinite values. If \code{x} is a data frame, returns \code{TRUE} for
#'   each variable that has any missing or infinite values. If \code{out = "table"},
#'   results are returned as data frame, with column number, variable name and
#'   label, and a logical vector indicating if a variable has missing values or
#'   not. However, it's printed in colors, with green rows indicating that a
#'   variable has no missings, while red rows indicate the presence of missings
#'   or infinite values. If \code{out = "index"}, a named vector is returned.
#'
#' @examples
#' data(efc)
#' has_na(efc$e42dep)
#' has_na(efc, e42dep, tot_sc_e, c161sex)
#' has_na(efc)
#'
#' has_na(efc, e42dep, tot_sc_e, c161sex, out = "index")
#' has_na(efc, out = "df")
#'
#' @importFrom purrr map_lgl
#' @importFrom dplyr quos
#' @importFrom sjlabelled get_label
#' @export
has_na <- function(x, ..., out = c("table", "df", "index")) {
  out <- match.arg(out)
  .dat <- get_dot_data(x, dplyr::quos(...))
  if (is.data.frame(x)) {
    tmp <- purrr::map_lgl(.dat, ~ anyNA(.x) | any(is.infinite(.x)))

    # return data frame?
    if (out == "df") {
      tmp <- as.data.frame(tmp)
    }

    # return variable labels?
    if (out == "table") {
      tmp <- data.frame(
        col = match(names(tmp), colnames(x)),
        name = names(tmp),
        label = shorten_string(sjlabelled::get_label(.dat, def.value = names(tmp)), 35),
        has.na = tmp,
        stringsAsFactors = FALSE
      )
      class(tmp) <- c("sj_has_na", class(tmp))
    }

    tmp

  } else {
    anyNA(x) | any(is.infinite(x))
  }
}
