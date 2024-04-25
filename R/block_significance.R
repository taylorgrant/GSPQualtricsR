#' Significance testing for all questions in a block
#'
#' Runs a prop.test for each question; both within/between groups
#'
#' @param tbl Dataframe of summarized survey data
#' @param conf_level The ci to test signficance at
#'
#' @return List of dataframes for each question; one for within group testing and one for between group testing
#' @export
#'
#' @examples
#' \dontrun{
#' out <- block_significance(tbl, conf_level, filter_text)
#' }
block_significance <- function(tbl, conf_level) {


  # GET PARAMETERS FOR CAPTION LABEL ----------------------------------------
  q <- sub("_.*", "", unique(stats::na.omit(tbl$variable))[1])
  qtext <- unique(stats::na.omit(tbl$question_text))
  resp_count <- unique(stats::na.omit(tbl$total_n))
  filter_text <- attributes(tbl)$filter_text
  if ("group_variable" %in% names(tbl)) {
    tmp <- tbl |> dplyr::distinct(group_label, group_n)
    group_mapping <- paste(tmp$group_label, "=", tmp$group_n, collapse = "; ")
  } else {
    group_mapping <- NULL
  }
  sig_let <- paste0("(",paste(LETTERS[1:length(unique(tbl$var_helper))], collapse = ", "), "): ")
  strfun <- function(str, n) {gsub(paste0("([^ ]+( +[^ ]+){",n-1,"}) +"), "\\1\n", str)}   # add line breaks after N spaces

  # significance test starts here
  if (any(grepl("group_variable", names(tbl)))) {

    # CROSSTABBED VARIABLE - WITHIN GROUP -------------------------------------

    within <- tidyr::crossing(group_sub = tbl$group_sub, group_label = tbl$group_label, question_sub = tbl$question_sub,
                              var1 = tbl$var_label, var2 = tbl$var_label) |>
      dplyr::left_join(dplyr::select(tbl, c(group_sub, group_label, question_sub, var_label, within = var_num, n)),
                       by = c("group_sub", "group_label", "question_sub", "var1" = "var_label")) |>
      dplyr::left_join(dplyr::select(tbl, c(group_sub, group_label, question_sub, var_label, n, group_n)),
                       by = c("group_sub", "group_label", "question_sub", "var2" = "var_label")) |>
      dplyr::filter(!is.na(group_n)) |>
      tidyr::replace_na(list(n.x = 0, n.y = 0))

    # run the prop.test
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
      dplyr::mutate(var_num = dplyr::case_when(length(unique(var_num)) == 1 ~ as.integer(var_label),
                                                        TRUE ~ var_num)) |>
      dplyr::left_join(within, by = c("group_sub", "group_label", "question_sub","var_label" = "var2")) |>
      # var_label is a factor, so need to append letters
      dplyr::mutate(var_helper = paste0("\n(", LETTERS[var_num], ")"))

    # need to get the letters available for the caption
    sig_let <- paste0("(",paste(LETTERS[1:length(unique(out_win$var_helper))], collapse = ", "), "): ")

    out_win <- out_win |>
      dplyr::mutate(var_label = paste0(var_label, var_helper),
                    question_sub = strfun(question_sub, 3),
                    # group_label = strfun(group_label, 3),
                    proportion = dplyr::case_when(!is.na(sig.tmp) ~ paste0(round(proportion*100), "% ", sig.tmp, "\n(", n,")"),
                                                  TRUE ~ paste0(round(proportion*100), "%",  "\n(", n,")"))) |>
      dplyr::select(`Question Group` = question_sub, `Crosstab Group` = group_label, `Crosstab Subgroup` = group_sub, Answer = var_label, Percentage = proportion) |>
      tidyr::pivot_wider(names_from = Answer, values_from = Percentage) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~dplyr::if_else(stringr::str_detect(., "NA%"), NA, .))) |>
      dplyr::mutate(caption = paste0(q,": ", qtext,
                                     "; Total respondents: ", resp_count, "; Filters: ",attr(tbl, "filter_text"), "; Group totals: ", group_mapping,
                                     "; ", sig_let, "Significance at ", paste0(conf_level*100,"%"), " confidence intervals"))
    # drop any column that is all NA
    out_win <- out_win[, !sapply(out_win, function(x) all(is.na(x)))]
    out_win$caption[-1] <- NA

    # CROSSTABBED VARIABLE - BETWEEN GROUPS -----------------------------------

    # pull the grouping variable
    grp1 <- tbl |>
      dplyr::distinct(group_label) |>
      dplyr::pull()

    # if a filter has been applied that only keeps one crosstab group, return NULL
    if (length(grp1) == 1) {

      out <- list(out_win = out_win, out_bwn = NULL) |>
        purrr::set_names(nm = c(paste0(q, "_win"),
                                paste0(q, "_bwn")))

    } else {

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
        dplyr::mutate(grp1 = haven::as_factor(grp1),
                      between = as.numeric(grp1) - (min(as.numeric(grp1) - 1)),
                      between = LETTERS[between]) |>
        dplyr::mutate(between = ifelse(((p.value < (1 - as.numeric(conf_level))) & (estimate2 > estimate1)), between, NA)) |>
        dplyr::filter(!is.na(between)) |>
        dplyr::group_by(group_sub, grp2, question_sub, var_label) |>
        dplyr::summarise(sig.tmp = paste(between, collapse = ", "))

      out_bwn <- tbl |>
        dplyr::mutate(var_num = dplyr::case_when(length(unique(var_num)) == 1 ~ dplyr::row_number(),
                                                 TRUE ~ var_num)) |>
        dplyr::left_join(between, by = c("group_label" = "grp2", "group_sub", "question_sub", "var_label")) |>
        dplyr::mutate(var_helper = paste0("\n (", LETTERS[haven::as_factor(group_label)], ")"))

      # need to get the letters available for the caption
      sig_let <- paste0("(",paste(LETTERS[1:length(unique(out_bwn$var_helper))], collapse = ", "), "): ")

      out_bwn <- out_bwn |>
        dplyr::mutate(group_label = paste0(group_label, var_helper),
                      question_sub = strfun(question_sub, 3),
                      var_label = strfun(var_label, 3),
                      proportion = dplyr::case_when(!is.na(sig.tmp) ~ paste0(round(proportion*100), "% ", sig.tmp, "\n(", n,")"),
                                                    TRUE ~ paste0(round(proportion*100), "%",  "\n(", n,")"))) |>
        dplyr::select(`Question Group` = question_sub, `Crosstab Group` = group_label, `Crosstab Subgroup` = group_sub, Answer = var_label, Percentage = proportion) |>
        tidyr::pivot_wider(names_from = `Crosstab Group`, values_from = Percentage) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), ~dplyr::if_else(stringr::str_detect(., "NA%"), NA, .))) |>
        dplyr::mutate(caption = paste0(q,": ", qtext,
                                       "; Total respondents: ", resp_count, "; Filters: ",attr(tbl, "filter_text"), "; Group totals: ", group_mapping,
                                       "; ", sig_let, "Significance at ", paste0(conf_level*100,"%"), " confidence intervals"))

      # drop any column that is all NA
      out_bwn <- out_bwn[, !sapply(out_bwn, function(x) all(is.na(x)))]
      out_bwn$caption[-1] <- NA

      out <- list(out_win, out_bwn) |>
        purrr::set_names(nm = c(paste0(q, "_win"),
                                paste0(q, "_bwn")))
    }

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
      dplyr::mutate(var_num = dplyr::row_number()) |>
      dplyr::left_join(tmp, by = c("question_sub","var_label" = "var2")) |>
      # var_label is a factor, so need to append letters and re-level
      dplyr::mutate(var_label = forcats::fct_reorder(paste0(var_label, "\n(", LETTERS[var_num], ")"), var_num),
                    question_sub = factor(question_sub),
                    var_helper = paste0("\n(", LETTERS[var_num], ")"))

    # need to get the letters available for the caption
    sig_let <- paste0("(",paste(LETTERS[1:length(unique(out_win$var_helper))], collapse = ", "), "): ")

    out_win <- out_win |>
      dplyr::mutate(proportion = dplyr::case_when(!is.na(sig.tmp) ~
                                                    paste0(round(proportion*100), "% ", sig.tmp, "\n(", n,")"),
                                                  TRUE ~ paste0(round(proportion*100), "%",  "\n(", n,")"))) |>
      dplyr::select(`Question Group` = question_sub, Answer = var_label, Percentage = proportion) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~dplyr::if_else(stringr::str_detect(., "NA%"), NA, .))) |>
      dplyr::mutate(caption = paste0(q,": ", qtext,
                                     "; Total respondents: ", resp_count, "; Filters: ",attr(tbl, "filter_text"),
                                     "; ", sig_let, "Significance at ", paste0(conf_level*100,"%"), " confidence intervals"))
    # drop any column that is all NA
    out_win <- out_win[, !sapply(out_win, function(x) all(is.na(x)))]
    out_win$caption[-1] <- NA

    out <- list(out_win = out_win) |>
      purrr::set_names(nm = q)
  }
  out
}
