#' Get survey responses
#'
#' This is a helper function that is called by the `summarize_survey()` function.
#' As a result, all parameters are passed into it via the higher order function.
#'
#' @param data Survey data
#' @param group Grouping variable if used
#' @param gsub Group sublabel if applicable
#' @param var Survey variable of interest
#' @param qname Question name of variable
#' @param qsub Any sub-label or extra definition pulled from the API
#' @param resps Vector of all possible questions respondents could answer
#' @param selector Selector type - if Likert the app will allow for top/bottom box
#' @param filters List containing any Q columns to filter and the responses to keep
#'
#' @return Data frame with with all grouped data and the survey proportion
#' @export
#'
#' @examples
#'\dontrun{
#' na
#' }
get_responses <- function(data, group, gsub, var, qname, qsub, selector, resps, filters) {
  quo_var <- rlang::sym(var)

  if (is.na(filters$filterQ)) {
    tmp <- data$svy |>
      # can't get logic, but can drop anyone that didn't answer all questions tied to a specific ID
      dplyr::mutate(na_all = (rowSums(dplyr::across(dplyr::matches(resps), is.na))) == length(resps)) |>
      dplyr::filter(na_all == FALSE)
  } else {
    quo_filter <- rlang::sym(filters$filterQ)
    tmp <- data$svy |>
      # can't get logic, but can drop anyone that didn't answer all questions tied to a specific ID
      dplyr::mutate(na_all = (rowSums(dplyr::across(dplyr::matches(resps), is.na))) == length(resps)) |>
      dplyr::filter(na_all == FALSE) |>
      dplyr::filter(!!quo_filter %in% filters$filter_choices)
  }

  if (is.na(group)) {
    # --- Single Variable --- #
    tmp |>
      dplyr::group_by(!!quo_var) |>
      svy_summary(group, gsub, var, qname, qsub, selector) |>
      subquestion_clean(qsub)
  } else {
    # --- Grouped Variable --- #
    quo_group <- rlang::sym(group)
    tmp |>
      dplyr::group_by(!!quo_group , !!quo_var) |>
      svy_summary(group, gsub, var, qname, qsub, selector) |>
      subquestion_clean(qsub) |>
      subgroup_clean(gsub)
  }
}
