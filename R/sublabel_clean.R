#' Clean the sub-label of the question
#'
#' This is a helper function used in the `summarize_survey()` function. It is used to map any sub-labels pulled from the question wordings against the variable labels pulled from the SPSS data. In the event that there is a match, variable labels win. If possible, we try to split the qsubs on a " - " and then try to match the split labels against the variable labels again. The goal is to try to get to meaningful qsubs for grouping in tables and graphs.
#'
#' @param tbl Summarized survey question
#' @param qsub Question sub-labels passed through by the `summarize_question()` function
#'
#' @return Dataframe, but sub-labels are either NA or hopefully informative
#' @export
#'
#' @examples
#' \dontrun{
#' na
#' }
subquestion_clean <- function(tbl, qsub) {
  if (any(!is.na(tbl$question_sub))) {
    tmp <- tbl |>
      dplyr::mutate(question_sub = dplyr::case_when(var_label == qsub ~ NA,
                                                 TRUE ~ qsub),
                    question_sub = dplyr::case_when(question_sub == "" ~ NA,
                                                 TRUE ~ question_sub)) |>
      tidyr::separate(question_sub, into = c("s1", "s2"), sep = " - ", remove = FALSE) |>
      dplyr::mutate(dplyr::across(s1:s2, ~dplyr::case_when(var_label == . ~ NA,
                                                           TRUE ~ .)))
    if (all(is.na(tmp$s2))) {
      tmp |>
        dplyr::select(-c(question_sub, s2)) |>
        dplyr::rename(question_sub = s1)
    }
  } else {
    tbl
  }
}

#' Clean the sub-label of the group
#'
#' This is essentially the same function as `subquestion_clean()` but for the group sub-label. Normally there shouldn't be much need for this, but if someone chooses to use a funky question as a grouping variable, we can end up with poorly formatted or missing group sub-labels that provide context.
#'
#' @param tbl Summarized survey question
#' @param gsub Group sub-labels passed through by the `summarize_question()` function
#'
#' @return Dataframe, but sub-labels are either NA or hopefully informative
#' @export
#'
#' @examples
#' \dontrun{
#' na
#' }
subgroup_clean <- function(tbl, gsub) {
  if (any(!is.na(tbl$group_sub))) {
    tmp <- tbl |>
      dplyr::mutate(group_sub = dplyr::case_when(group_label == gsub ~ NA,
                                                    TRUE ~ gsub),
                    group_sub = dplyr::case_when(group_sub == "" ~ NA,
                                                    TRUE ~ group_sub)) |>
      tidyr::separate(group_sub, into = c("s1", "s2"), sep = " - ", remove = FALSE) |>
      dplyr::mutate(dplyr::across(s1:s2, ~dplyr::case_when(group_label == . ~ NA,
                                                           TRUE ~ .)))
    if (all(is.na(tmp$s2))) {
      tmp |>
        dplyr::select(-c(group_sub, s2)) |>
        dplyr::rename(group_sub = s1)
    }
  } else {
    tbl
  }
}
