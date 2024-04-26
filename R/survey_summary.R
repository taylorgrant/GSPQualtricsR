#' Summarize the survey data
#'
#' This is a helper function for `summarize_survey()` and all arguments are passed into this via that higher order function.
#'
#' @param tbl Survey data with group and variable of interest
#' @param block Survey block section
#' @param group Grouping variable (if used)
#' @param gsub Group sublabel if applicable
#' @param var Survey variable of interest
#' @param qname Question name pulled from the TOC
#' @param qsub qsub pulled from the TOC
#' @param selector Selector type - if Likert the app will allow for top/bottom box
#'
#' @return Data frame with all grouped analysis
#' @export
#'
#' @examples
#' \dontrun{
#' na
#' }
survey_summary <- function(tbl, block, group, gsub, var, qname, qsub, selector) {

  if (is.na(group)) {
    tbl <- tbl |>
      dplyr::summarise(proportion = srvyr::survey_mean(vartype = "ci"),
                       n = srvyr::unweighted(dplyr::n())) |>
      dplyr::mutate(block = block,
                    variable = var,
                    var_label = trimws(gsub("[\r\n\t]", "", haven::as_factor(!!rlang::sym(var)))),
                    var_label = haven::as_factor(var_label),
                    var_num = as.integer(!!rlang::sym(var)),
                    total_n = attr(tbl, "total_n")) |>
      dplyr::select(-!!rlang::sym(var)) |>
      dplyr::mutate(question_text = qname,
                    question_sub = qsub,
                    selector = selector) |>
      dplyr::filter(!is.na(var_label)) |>
      dplyr::relocate(c(question_text, question_sub, var_label), .before = proportion)

  } else {
    tbl <- tbl |>
      dplyr::summarise(proportion = srvyr::survey_mean(vartype = "ci", na.rm = FALSE),
                       n = srvyr::unweighted(dplyr::n())) |>
      dplyr::mutate(block = block,
                    variable = var,
                    group_variable = group,
                    var_label = trimws(gsub("[\r\n\t]", "", haven::as_factor(!!rlang::sym(var)))),
                    var_label = haven::as_factor(var_label),
                    var_num = as.integer(!!rlang::sym(var)),
                    group_label = trimws(gsub("[\r\n\t]", "", haven::as_factor(!!rlang::sym(group)))),
                    group_label = haven::as_factor(group_label),
                    total_n = attr(tbl, "total_n")) |>
      dplyr::ungroup() |>
      dplyr::select(-!!rlang::sym(var)) |>
      dplyr::select(-!!rlang::sym(group)) |>
      dplyr::mutate(question_text = qname,
                    question_sub = qsub,
                    group_sub = gsub,
                    selector = selector) |>
      dplyr::filter(!is.na(var_label)) |>
      dplyr::filter(!is.na(group_label)) |>
      dplyr::relocate(c(question_text, question_sub, group_label, group_sub, var_label), .before = proportion)
  }
  tbl
}
