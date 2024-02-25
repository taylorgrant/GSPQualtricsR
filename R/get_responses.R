#' Get survey responses
#'
#' This is a helper function that is called by the `summarize_survey()` function.
#' As a result, all parameters are passed into it via the higher order function.
#'
#' @param data Survey data
#' @param group Grouping variable if used
#' @param var Survey variable of interest
#' @param qname Question name of variable
#' @param sublabel Any sub-label or extra definition pulled from the API
#' @param resps Vector of all possible questions respondents could answer
#'
#' @return Data frame with with all grouped data and the survey proportion
#' @export
#'
#' @examples
get_responses <- function(data, group, var, qname, sublabel, resps) {
  quo_var <- rlang::sym(var)

  tmp <- data$svy |>
    # can't get logic, but can drop anyone that didn't answer all questions tied to a specific ID
    dplyr::mutate(na_all = (rowSums(dplyr::across(dplyr::matches(resps), is.na))) == length(resps)) |>
    dplyr::filter(na_all == FALSE)

  if (is.null(group)) {
    # --- Single Variable --- #
    tmp |>
      dplyr::group_by(!!quo_var) |>
      svy_summary(var, qname, sublabel) |>
      sublabel_clean(sublabel)
  } else {
    # --- Grouped Variable --- #
    quo_group <- rlang::sym(group)
    tmp |>
      dplyr::group_by(!!quo_group , !!quo_var) |>
      svy_summary(group, var, qname, sublabel) |>
      sublabel_clean(sublabel)
  }
}
