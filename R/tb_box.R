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
#' tb_box(tbl, top, bottom)
#' }
tb_box <- function(tbl, top, bottom) {
  if (grepl("group_label", names(tbl))) {
    tbl |>
      dplyr::mutate(var_label = dplyr::case_when(var_num %in% top ~ glue::glue("Top {length(top)} Box"),
                                          var_num %in% bottom ~ glue::glue("Bottom {length(bottom)} Box"),
                                          TRUE ~ as.character(var_label)),
                    var_num = dplyr::case_when(var_num %in% top ~ max(var_num),
                                               var_num %in% bottom ~ min(var_num)+1,
                                               TRUE ~ var_num),
                    var_label = forcats::fct_reorder(var_label, var_num)) |>
      dplyr::group_by(question_text, group_label, sub_label, var_label, variable, ) |>
      dplyr::summarise(proportion = sum(proportion),
                n = sum(n))
  } else {
    tbl |>
      dplyr::mutate(var_label = dplyr::case_when(var_num %in% top ~ glue::glue("Top {length(top)} Box"),
                                          var_num %in% bottom ~ glue::glue("Bottom {length(bottom)} Box"),
                                          TRUE ~ as.character(var_label)),
                    var_num = dplyr::case_when(var_num %in% top ~ max(var_num),
                                               var_num %in% bottom ~ min(var_num)+1,
                                               TRUE ~ var_num),
                    var_label = forcats::fct_reorder(var_label, var_num)) |>
      dplyr::group_by(question_text, sub_label, var_label, variable, ) |>
      dplyr::summarise(proportion = sum(proportion),
                n = sum(n))
  }
}
