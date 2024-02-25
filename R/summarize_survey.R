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
#' tbl <- summarize_survey(parameters, data = out)
#' }
summarize_survey <- function(parameters, data) {
  qinfo <- data$toc |> dplyr::filter(question_id == parameters$qid)
  qvct <- dplyr::pull(qinfo, export_name)
  if (parameters$gid == "") {
    gvct <- NULL
  } else {
    ginfo <- data$toc |> dplyr::filter(question_id == parameters$gid)
    gvct <- dplyr::pull(ginfo, export_name)
  }
  purrr::pmap_dfr(list(qinfo$export_name, qinfo$question_text, qinfo$sub, group = ginfo$export_name), resps = qvct, get_responses, data = data)
}
