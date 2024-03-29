#' Calculate top and bottom box
#'
#' After summarizing the survey question, a user can then choose to summarize further into Top/Bottom box. Rather than re-running the analysis, this function just summarizes the table further. As a result, confidence intervals are lost.
#'
#' @param tbl Dataframe summary returned by the `summarize_survey()` function
#' @param top Vector of numbers for the top; the dataframe has variable numbers which can be used
#' @param bottom Vector of numbers for the top; the dataframe has variable numbers which can be used
#'
#' @return Dataframe with summed proportion and proper labels; CIs are dropped
#' @export
#'
#' @examples
#' \dontrun{
#' top <- c(3,4); bottom <- c(1,2)
#' likert_box(tbl, top, bottom)
#' }
likert_box <- function(tbl, top, bottom) {

  # allow for only top or bottom box selections
  if (is.null(bottom) && !is.null(top)) {
    tmptbl <- tbl |>
      dplyr::mutate(var_label = dplyr::case_when(var_num %in% top ~ glue::glue("Top {length(top)} Box"),
                                                 TRUE ~ as.character(var_label)),
                    var_num = dplyr::case_when(var_num %in% top ~ 10,
                                               TRUE ~ var_num),
                    var_label = forcats::fct_reorder(var_label, var_num))
  } else if (!is.null(bottom) && is.null(top)) {
    tmptbl <- tbl |>
      dplyr::mutate(var_label = dplyr::case_when(var_num %in% bottom ~ glue::glue("Bottom {length(bottom)} Box"),
                                                 TRUE ~ as.character(var_label)),
                    var_num = dplyr::case_when(var_num %in% bottom ~ 2,
                                               TRUE ~ var_num),
                    var_label = forcats::fct_reorder(var_label, var_num))
  } else {
    tmptbl <- tbl |>
      dplyr::mutate(var_label = dplyr::case_when(var_num %in% top ~ glue::glue("Top {length(top)} Box"),
                                                 var_num %in% bottom ~ glue::glue("Bottom {length(bottom)} Box"),
                                                 TRUE ~ as.character(var_label)),
                    var_num = dplyr::case_when(var_num %in% top ~ 10,
                                               var_num %in% bottom ~ 2,
                                               TRUE ~ var_num),
                    var_label = forcats::fct_reorder(var_label, var_num))
  }

  if (any(grepl("group_label", names(tbl)))) {
    tmptbl |>
      dplyr::group_by(question_text, question_sub, group_label, group_sub, var_num, var_label,
                      block, variable, group_variable, total_n, selector, group_n) |>
      dplyr::summarise(proportion = sum(proportion),
                n = sum(n)) |>
      dplyr::mutate(var_num = dplyr::row_number()) |>
      dplyr::ungroup()
  } else {
    tmptbl |>
      dplyr::group_by(question_text, question_sub, var_num, var_label, block, total_n, variable, selector ) |>
      dplyr::summarise(proportion = sum(proportion),
                n = sum(n)) |>
      dplyr::group_by(question_text, question_sub) |>
      dplyr::mutate(var_num = dplyr::row_number()) |>
      dplyr::ungroup()
  }
}
