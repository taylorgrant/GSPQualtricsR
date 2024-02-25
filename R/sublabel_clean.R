#' Clean the sub labels
#'
#' This is a helper function used in the `summarize_survey()` function. It is used to map any sub labels pulled from the question wordings against the variable labels pulled from the SPSS data. In the event that there is a match, variable labels win. If possible, we try to split the sublabels on a " - " and then try to match the split labels against the variable labels again. The goal is to try to get to meaningful sublabels for grouping in tables and graphs.
#'
#' @param tbl Summarized survey data
#' @param sublabel Sub labels passed through
#'
#' @return Data frame but sub labels are either NA or hopefully informative
#' @export
#'
#' @examples
sublabel_clean <- function(tbl, sublabel) {
  if (any(!is.na(tbl$sub_label))) {
    tmp <- tbl |>
      dplyr::mutate(sub_label = dplyr::case_when(var_label == sublabel ~ NA,
                                                 TRUE ~ sublabel),
                    sub_label = dplyr::case_when(sub_label == "" ~ NA,
                                                 TRUE ~ sub_label),
                    sub_label = stringr::str_remove_all(sub_label, "Selected Choice - ")) |>
      tidyr::separate(sub_label, into = c("s1", "s2"), sep = " - ", remove = FALSE) |>
      dplyr::mutate(dplyr::across(s1:s2, ~dplyr::case_when(var_label == . ~ NA,
                                                           TRUE ~ .)))
    if (all(is.na(tmp$s2))) {
      tmp |>
        dplyr::select(-c(sub_label, s2)) |>
        dplyr::rename(sub_label = s1)
    }
  } else {
    tbl
  }
}
