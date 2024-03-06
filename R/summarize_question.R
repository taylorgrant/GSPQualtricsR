#' Summarize survey question of interest
#'
#' This is the wrapper function that summarizes all survey data
#'
#' @param parameters Parameters are a named list with the question ids for the grouping variable and variable of interest. The question ids are used to filter the table of contents to pull all potential columns of interest.
#' @param data Named list that is returned by the `fetch_survey_data()` function.
#'
#' @return Data frame with all proportions and confidence intervals for the variable of interest, split out by grouping variable (if one is selected).
#' @export
#'
#' @examples
#' \dontrun{
#' parameters = list(qid = "VARID", gid = "GROUPID")
#' tbl <- summarize_survey(parameters, data = SURVEYDATA)
#' }
summarize_question <- function(parameters, data) {
  qinfo <- data$toc |> dplyr::filter(question_id == parameters$qid)
  qvct <- dplyr::pull(qinfo, export_name)
  conf_level <- parameters$ci
  if (is.null(parameters$gid)) {
    group <- data.frame(group = NA, gsub = NA)
  } else {
    group <- data$toc |> dplyr::filter(question_id == parameters$gid) |>
      dplyr::select(group = export_name, gsub = sub)
  }
  if (is.null(parameters$fiq$filterQ)) {
    filters <- list(filterQ = NA, filter_choices = NA)
  } else {
    filters <- parameters$fiq
  }
  crossed <- tidyr::crossing(qinfo, group)
  out <- purrr::pmap_dfr(list(block = crossed$block, var = crossed$export_name, qname = crossed$question_text,
                       qsub = crossed$sub, selector = crossed$selector_type,
                       group = crossed$group, gsub = crossed$gsub),
                  resps = qvct, filters = filters,
                  get_responses, data = data)
  significance_test(out, conf_level)
}
