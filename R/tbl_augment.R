#' Augment a table with missing rows
#'
#' If a scale to a survey question has zero responses, it will be dropped from the analysis because the variables being passed into the `group_by()` function are SPSS <dbl+lbl> variable types, rather than factors. This is an absolute kludge of a solution to make sure the empty response rows are retained in the data.
#' @param tbl A dataframe that was processed by the `survey_summary()` function
#'
#' @return Data frame
#' @export
#'
#' @examples
#' \dontrun{
#' tbl_augment(tbl)
#' }
tbl_augment <- function(tbl) {
  var_fill <- data.frame(var_label = unique(tbl$var_label), var_num = unique(tbl$var_num))

  tbl |>
    dplyr::group_by(question_text, question_sub, group_label, group_sub) |>
    tidyr::complete(var_label) |>
    dplyr::select(-var_num) |>
    dplyr::left_join(var_fill, by = c("var_label")) |>
    dplyr::arrange(var_num, .by_group = TRUE) |>
    # dplyr::mutate(var_num = dplyr::row_number()) |>
    dplyr::ungroup()
}
