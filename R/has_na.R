#' @title Check if variables or cases have missing / infinite values
#' @name has_na
#'
#' @description This functions checks if variables or observations in a data
#'   frame have \code{NA}, \code{NaN} or \code{Inf} values.
#'
#' @param x A data frame.
#' @param by Whether to check column- or row-wise for missing and infinite values.
#'   If \code{by = "col"}, \code{has_na()} checks for \code{NA}/\code{NaN}/\code{Inf}
#'   in \emph{columns}; If \code{by = "row"}, \code{has_na()} checks each row for
#'   these values.
#' @param out Output (return) format of the results. May be abbreviated.
#'
#' @inheritParams descr
#'
#' @return If \code{x} is a vector, returns \code{TRUE} if \code{x} has any missing
#'   or infinite values. If \code{x} is a data frame, returns \code{TRUE} for
#'   each variable (if \code{by = "col"}) or observation (if \code{by = "row"})
#'   that has any missing or infinite values. If \code{out = "table"}, results
#'   are returned as data frame, with column number, variable name and
#'   label, and a logical vector indicating if a variable has missing values or
#'   not. However, it's printed in colors, with green rows indicating that a
#'   variable has no missings, while red rows indicate the presence of missings
#'   or infinite values. If \code{out = "index"}, a named vector is returned.
#'
#' @note \code{complete_cases()} and \code{incomplete_cases()} are convenient
#'   shortcuts for \code{has_na(by = "row", out = "index")}, where the first
#'   only returns case-id's for all complete cases, and the latter only for
#'   non-complete cases. \cr \cr
#'   \code{complete_vars()} and \code{incomplete_vars()} are convenient shortcuts
#'   for \code{has_na(by = "col", out = "index")}, and again only return those
#'   column-id's for variables which are (in-)complete.
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
#' has_na(efc, by = "row")
#' has_na(efc, e42dep, tot_sc_e, c161sex, by = "row", out = "index")
#' has_na(efc, by = "row", out = "df")
#'
#' complete_cases(efc, e42dep, tot_sc_e, c161sex)
#' incomplete_cases(efc, e42dep, tot_sc_e, c161sex)
#' complete_vars(efc, e42dep, tot_sc_e, c161sex)
#' incomplete_vars(efc, e42dep, tot_sc_e, c161sex)
#'
#' @importFrom purrr map_lgl
#' @importFrom dplyr quos
#' @importFrom sjlabelled get_label
#' @export
has_na <- function(x, ..., by = c("col", "row"), out = c("table", "df", "index")) {
  out <- match.arg(out)
  by <- match.arg(by)

  .dat <- get_dot_data(x, dplyr::quos(...))
  if (is.data.frame(x)) {
    if (by == "row") {
      tmp <- apply(.dat, 1, function(.x) anyNA(.x) | any(is.infinite(.x)))
    } else {
      tmp <- purrr::map_lgl(.dat, ~ anyNA(.x) | any(is.infinite(.x)))
    }


    # return data frame?
    if (out == "df") {
      tmp <- as.data.frame(tmp)
    }

    # return variable labels?
    if (out == "table" && by == "col") {
      tmp <- data_frame(
        col = match(names(tmp), colnames(x)),
        name = names(tmp),
        label = shorten_string(sjlabelled::get_label(.dat, def.value = names(tmp)), 35),
        has.na = tmp
      )
      class(tmp) <- c("sj_has_na", class(tmp))
    }

    if (out == "table" && by == "row") {
      tmp <- data_frame(
        case = 1:nrow(.dat),
        has.na = tmp
      )
    }

    if (out == "index" && by == "row") tmp <- which(tmp)

    tmp

  } else {
    anyNA(x) | any(is.infinite(x))
  }
}


#' @rdname has_na
#' @export
incomplete_cases <- function(x, ...) {
  has_na(x, ..., by = "row", out = "index")
}


#' @rdname has_na
#' @export
complete_cases <- function(x, ...) {
  all.cases <- seq_len(nrow(x))
  na.cases <- has_na(x, ..., by = "row", out = "index")

  if (sjmisc::is_empty(na.cases))
    all.cases
  else
    all.cases[-na.cases]
}


#' @rdname has_na
#' @export
complete_vars <- function(x, ...) {
  which(!has_na(x, ..., by = "col", out = "index"))
}


#' @rdname has_na
#' @export
incomplete_vars <- function(x, ...) {
  which(has_na(x, ..., by = "col", out = "index"))
}
