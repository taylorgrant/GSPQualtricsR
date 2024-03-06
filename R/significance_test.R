#' Test statistical significance
#'
#' If it's a single variable (not crosstabbed) each selection option is compared against all others via the `prop.test()` function. If it is crosstabbed, selection options within each group are compared.
#'
#' @param tbl Dataframe with summarized results
#' @param conf_level Confidence interval e.g., .9, .95
#'
#' @return Dataframe with appended significance letters. Letters are appended to the variable labels as well to match the test results
#' @export
#'
#' @examples
#' \dontrun{
#' significance_test(tbl, conf_level = .95)
#' }
significance_test <- function(tbl, conf_level) {

  if (any(grepl("group_variable", names(tbl)))) {

# CROSSTABBED VARIABLE - WITHIN GROUP -------------------------------------

  within <- tidyr::crossing(group_sub = tbl$group_sub, group_label = tbl$group_label, question_sub = tbl$question_sub,
                  var1 = tbl$var_label, var2 = tbl$var_label) |>
    dplyr::left_join(dplyr::select(tbl, c(group_sub, group_label, question_sub, var_label, n)),
                       by = c("group_sub", "group_label", "question_sub", "var1" = "var_label")) |>
    dplyr::left_join(dplyr::select(tbl, c(group_sub, group_label, question_sub, var_label, n, group_n)),
                       by = c("group_sub", "group_label", "question_sub", "var2" = "var_label")) |>
    dplyr::filter(!is.na(group_n)) |>
    tidyr::replace_na(list(n.x = 0, n.y = 0))

  within <- within |>
    dplyr::mutate(
      signif = list(n.x, n.y, group_n) |>  purrr::pmap(~ {
        prop.test(
          x = c(..1, ..2),
          n = c(..3, ..3), # all groups have same n
          conf.level = as.numeric(conf_level)
        ) |>  broom::tidy()
      })) |>
    tidyr::unnest(cols = signif) |>
    dplyr::mutate(within = as.numeric(var1) - (min(as.numeric(var1) - 1)),
                  within = LETTERS[within]) |>
    dplyr::mutate(within = ifelse(((p.value < (1 - as.numeric(conf_level))) & (estimate2 > estimate1)), within, NA)) |>
    dplyr::filter(!is.na(within)) |>
    dplyr::group_by(group_sub, group_label, question_sub, var2) |>
    dplyr::summarise(sig.tmp = paste(within, collapse = ", "))

  out_win <- tbl |>
    dplyr::mutate(var_num = dplyr::case_when(length(unique(var_num)) == 1 ~ dplyr::row_number(),
                                             TRUE ~ as.integer(var_label))) |>
    dplyr::left_join(within, by = c("group_sub", "group_label", "question_sub","var_label" = "var2")) |>
    # var_label is a factor, so need to append letters and re-level
    dplyr::mutate(var_helper = paste0("<br> (", LETTERS[var_label], ")"))

  attr(out_win, "type") <- "within"

  # CROSSTABBED VARIABLE - BETWEEN GROUPS -----------------------------------

  # pull the grouping variable
  grp1 <- tbl |>
    dplyr::distinct(group_label) |>
    dplyr::pull()

  # cross together so we get every pairing
  between <- tidyr::crossing(group_sub = tbl$group_sub, grp1 = grp1, grp2 = grp1,
                             question_sub = tbl$question_sub, var_label = tbl$var_label) |>
    dplyr::distinct(group_sub, grp1, grp2, question_sub, var_label, .keep_all = TRUE) |>
    dplyr::left_join(dplyr::select(tbl, c(group_sub, group_label, question_sub, var_label, n, group_n)),
                     by = c("group_sub", "grp1" = "group_label", "question_sub", "var_label")) |>
    dplyr::left_join(dplyr::select(tbl, c(group_sub, group_label, question_sub, var_label, n, group_n)),
                     by = c("group_sub", "grp2" = "group_label", "question_sub", "var_label")) |>
    dplyr::filter(grp1 != grp2) |>
    tidyr::replace_na(list(n.x = 0, n.y = 0)) |>
    dplyr::filter(!is.na(group_n.x)) |>
    dplyr::filter(!is.na(group_n.y))

  between <- between |>
    dplyr::mutate(
      signif = list(n.x, n.y, group_n.x, group_n.y) |>  purrr::pmap(~ {
        prop.test(
          x = c(..1, ..2),
          n = c(..3, ..4),
          conf.level = as.numeric(conf_level)
        ) |>  broom::tidy()
      })) |>
    tidyr::unnest(cols = signif) |>
    dplyr::mutate(between = as.numeric(grp1) - (min(as.numeric(grp1) - 1)),
                  between = LETTERS[between]) |>
    dplyr::mutate(between = ifelse(((p.value < (1 - as.numeric(conf_level))) & (estimate2 > estimate1)), between, NA)) |>
    dplyr::filter(!is.na(between)) |>
    dplyr::group_by(group_sub, grp2, question_sub, var_label) |>
    dplyr::summarise(sig.tmp = paste(between, collapse = ", "))

  out_bwn <- tbl |>
    dplyr::mutate(var_num = dplyr::case_when(length(unique(var_num)) == 1 ~ dplyr::row_number(),
                                             TRUE ~ as.integer(var_label))) |>
    dplyr::left_join(between, by = c("group_label" = "grp2", "group_sub", "question_sub", "var_label")) |>
    # var_label is a factor, so need to append letters and re-level
    dplyr::mutate(var_helper = paste0("<br> (", LETTERS[as.factor(group_label)], ")"))

  attr(out_bwn, "type") <- "between"

  out <- list(out_win = out_win, out_bwn = out_bwn)
  } else {

    # SINGLE VARIABLE ---------------------------------------------------------

    tmp <- tidyr::crossing(question_sub = tbl$question_sub, var1 = tbl$var_label, var2 = tbl$var_label) |>
      dplyr::left_join(dplyr::select(tbl, c(question_sub, var_label, n)),
                       by = c("question_sub", "var1" = "var_label")) |>
      dplyr::left_join(dplyr::select(tbl, c(question_sub, var_label, n, total_n)),
                       by = c("question_sub", "var2" = "var_label"))

    tmp <- tmp |>
      dplyr::mutate(
        signif = list(n.x, n.y, total_n) |>  purrr::pmap(~ {
          prop.test(
            x = c(..1, ..2),
            n = c(..3, ..3), # all groups have same n
            conf.level = as.numeric(conf_level)
          ) |>  broom::tidy()
        })) |>
      tidyr::unnest(cols = signif) |>
      dplyr::mutate(tmp = as.numeric(var1) - (min(as.numeric(var1) - 1)),
                    tmp = LETTERS[tmp]) |>
      dplyr::mutate(tmp = ifelse(((p.value < (1 - as.numeric(conf_level))) & (estimate2 > estimate1)), tmp, NA)) |>
      dplyr::filter(!is.na(tmp)) |>
      dplyr::group_by(question_sub, var2) |>
      dplyr::summarise(sig.tmp = paste(tmp, collapse = ", "))

    out_win <- tbl |>
      dplyr::mutate(var_num = dplyr::case_when(length(unique(var_num)) == 1 ~ dplyr::row_number(),
                                               TRUE ~ as.numeric(var_num) - (min(as.numeric(var_num) - 1)))) |>
      dplyr::left_join(tmp, by = c("question_sub","var_label" = "var2")) |>
      # var_label is a factor, so need to append letters and re-level
      dplyr::mutate(var_label = forcats::fct_reorder(paste0(var_label, "<br> (", LETTERS[var_num], ")"), var_num),
                    question_sub = factor(question_sub),
                    var_helper = paste0("<br> (", LETTERS[var_num], ")"))
      # dplyr::mutate(var_label = forcats::fct_reorder(paste0(var_label, " (", LETTERS[var_num], ")"), var_num)) |> data.frame()
    out <- list(out_win = out_win, out_bwn = NULL)
  }
  out
}

