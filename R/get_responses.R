#' Get survey responses
#'
#' This is a helper function that is called by the `summarize_survey()` function.
#' As a result, all parameters are passed into it via the higher order function.
#'
#' @param data Survey data
#' @param block Survey block
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
get_responses <- function(data, block, group, gsub, var, qname, qsub, selector, resps, filters) {

  quo_var <- rlang::sym(var)

  # if user not using the global filter
  if (is.na(filters$filterQ)) {

    tmp <- data$svy |>
      # drop anyone that didn't answer all questions tied to a specific ID
      dplyr::mutate(na_all = (rowSums(dplyr::across(dplyr::matches(resps), is.na))) == length(resps)) |>
      dplyr::filter(na_all == FALSE)

    # respondent count as attr --> becomes column in `survey_summary()`
    attr(tmp, "total_n") <- nrow(tmp)

  } else {

    # if global filter is used
    quo_filter <- rlang::sym(filters$filterQ)

    tmp <- data$svy |>
      # drop anyone that didn't answer all questions tied to a specific ID
      dplyr::mutate(na_all = (rowSums(dplyr::across(dplyr::matches(resps), is.na))) == length(resps)) |>
      dplyr::filter(na_all == FALSE) |>
      dplyr::filter(!!quo_filter %in% filters$filter_choices)

    # respondent count as attr --> becomes column in `svy_summary()`
    attr(tmp, "total_n") <- nrow(tmp)

  }

  # if there is no group/crosstab
  if (is.na(group)) {

    # --- Single Variable --- #
    tmp |>
      dplyr::group_by(!!quo_var) |>
      survey_summary(block, group, gsub, var, qname, qsub, selector) |>
      subquestion_clean(qsub)

  } else {
    # --- + Grouped Variable --- #
    quo_group <- rlang::sym(group)

    # getting group counts to add into data for sig.testing; ensure that label is a factor
    group_counts <- tmp$variables |>
      dplyr::count(!!quo_group, name = "group_n") |>
      dplyr::mutate(group_label = factor(trimws(gsub("[\r\n\t]", "", haven::as_factor(!!quo_group)))))

    tmp |>
      dplyr::group_by(!!quo_group , !!quo_var) |>
      survey_summary(block, group, gsub, var, qname, qsub, selector) |>
      tbl_augment() |> # fill any rows that are missing
      subquestion_clean(qsub) |>
      subgroup_clean(gsub) |>
      dplyr::left_join(group_counts[,2:3]) # left join (dropping the <dbl+lbl> column)
  }
}
