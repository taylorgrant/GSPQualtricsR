#' Summarize the survey data
#'
#' This is a helper function for `summarize_survey()` and all arguments are passed into this via that higher order function.
#'
#' @param tbl Survey data with group and variable of interest
#' @param group Grouping variable (if used)
#' @param var Survey variable of interest
#' @param qname Question name pulled from the TOC
#' @param sublabel Sublabel pulled from the TOC
#'
#' @return Data frame with all grouped analysis
#' @export
#'
#' @examples
svy_summary <- function(tbl, group, var, qname, sublabel) {

  if (is.null(group)) {
    tbl |>
      dplyr::summarise(proportion = srvyr::survey_mean(vartype = "ci"),
                       n = srvyr::unweighted(dplyr::n())) |>
      dplyr::mutate(variable = var,
                    var_label = trimws(gsub("[\r\n\t]", "", haven::as_factor(!!rlang::sym(var)))),
                    var_label = haven::as_factor(var_label),
                    var_num = as.numeric(!!rlang::sym(var))) |>
      dplyr::select(-!!rlang::sym(var)) |>
      dplyr::relocate(c(var_num, var_label), .before = proportion) |>
      dplyr::mutate(question_text = qname,
                    sub_label = sublabel) |>
      dplyr::filter(!is.na(var_label))
  } else {
    tbl |>
      dplyr::summarise(proportion = srvyr::survey_mean(vartype = "ci"),
                       n = srvyr::unweighted(dplyr::n())) |>
      dplyr::mutate(variable = var,
                    group_variable = group,
                    var_label = trimws(gsub("[\r\n\t]", "", haven::as_factor(!!rlang::sym(var)))),
                    var_label = haven::as_factor(var_label),
                    var_num = as.numeric(!!rlang::sym(var)),
                    group_label = trimws(gsub("[\r\n\t]", "", haven::as_factor(!!rlang::sym(group)))),
                    group_label = haven::as_factor(group_label)) |>
      dplyr::select(-!!rlang::sym(var)) |>
      dplyr::ungroup() |>
      dplyr::select(-!!rlang::sym(group)) |>
      dplyr::relocate(c(var_num, var_label), .before = proportion) |>
      dplyr::relocate(group_label, .before = var_num) |>
      dplyr::mutate(question_text = qname,
                    sub_label = sublabel) |>
      dplyr::filter(!is.na(var_label)) |>
      dplyr::filter(!is.na(group_label))
  }
}
