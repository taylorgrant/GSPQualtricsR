#' Build results tables
#'
#' Table builds depend on analysis type. If single variable, it's a straightforward table. If crosstab, two tables are created with significance testing - results and sigtest within each group, and results and sigtest between groups.
#'
#' @param tbl Dataframe outputted from the `summarize_question()` function. If crosstabbed results, the `summarize_question()` function outputs two dataframes in a named list.
#' @param ci Confidence interval for significant testing
#'
#' @return DT datatable (really most useful in the app)
#' @export
#'
#' @examples
#' \dontrun{
#' build_table(tbl$out_bwn, .95)
#' }
build_table <- function(tbl, ci) {

  # set up for table caption
  q <- sub("_.*", "", tbl$variable[1])
  qtext <- unique(tbl$question_text)
  resp_count <- unique(tbl$total_n)
  if ("group_label" %in% names(tbl)) {
    tmp <- tbl |> dplyr::distinct(group_label, group_n)
    group_mapping <- paste(tmp$group_label, "=", tmp$group_n, collapse = "; ")
  } else {
    group_mapping <- NULL
  }
  sig_let <- paste0("(",paste(LETTERS[1:length(unique(tbl$var_helper))], collapse = ", "), "): ")

  # add line breaks after N spaces
  strfun <- function(str, n) {gsub(paste0("([^ ]+( +[^ ]+){",n-1,"}) +"), "\\1<br>", str)}


  # SINGLE VARIABLE; NO CROSSTAB --------------------------------------------

  if (!("group_label" %in% colnames(tbl))) {

    if (any(tbl$selector == "Likert")) { # no sublabel but likert scaling
      tmpout <- tbl |>
        dplyr::mutate(proportion = dplyr::case_when(!is.na(sig.tmp) ~
                                                      paste0(round(proportion*100), "%","<sup>", sig.tmp, "</sup>", "<br> (", n,")"),
                                                    TRUE ~ paste0(round(proportion*100), "%",  "<br> (", n,")")),
                      var_label = strfun(var_label, 3)) |>
        dplyr::select(`Question Group` = question_sub, Answer = var_label, Percentage = proportion) |>
        tidyr::pivot_wider(names_from = Answer, values_from = Percentage)

    } else {

      tmpout <- tbl |>
        dplyr::mutate(proportion = dplyr::case_when(!is.na(sig.tmp) ~
                                                      paste0(round(proportion*100), "%","<sup>", sig.tmp, "</sup>", "<br> (", n,")"),
                                                    TRUE ~ paste0(round(proportion*100), "%",  "<br> (", n,")"))) |>
        dplyr::select(`Question Group` = question_sub, Answer = var_label, Percentage = proportion)

      # drop any columns that are all NA (if no sub-label)
      tmpout <- tmpout[, !sapply(tmpout, function(x) all(is.na(x)))]
    }
    } else {

      if (attr(tbl, "type") == "between") { # sig test table between groups
        tmpout <- tbl |>
          dplyr::mutate(group_label = paste0(group_label, var_helper),
                        question_sub = strfun(question_sub, 3),
                        var_label = strfun(var_label, 3),
                        proportion = dplyr::case_when(!is.na(sig.tmp) ~
                                                        paste0(round(proportion*100), "%","<sup>", sig.tmp, "</sup>", "<br> (", n,")"),
                                                      TRUE ~ paste0(round(proportion*100), "%",  "<br> (", n,")"))) |>
          dplyr::select(`Question Group` = question_sub, `Crosstab Group` = group_label, `Crosstab Subgroup` = group_sub, Answer = var_label, Percentage = proportion) |>
          tidyr::pivot_wider(names_from = `Crosstab Group`, values_from = Percentage)

        # drop any columns that are all NA (if no sub-label)
        tmpout <- tmpout[, !sapply(tmpout, function(x) all(is.na(x)))]

      } else { # sig test table within groups

        tmpout <- tbl |>
          dplyr::mutate(var_label = paste0(var_label, var_helper),
                        question_sub = strfun(question_sub, 3),
                        var_label = strfun(var_label, 3),
                        proportion = dplyr::case_when(!is.na(sig.tmp) ~
                                                        paste0(round(proportion*100), "%","<sup>", sig.tmp, "</sup>", "<br> (", n,")"),
                                                      TRUE ~ paste0(round(proportion*100), "%",  "<br> (", n,")"))) |>
          dplyr::select(`Question Group` = question_sub, `Crosstab Group` = group_label, `Crosstab Subgroup` = group_sub, Answer = var_label, Percentage = proportion) |>
          tidyr::pivot_wider(names_from = Answer, values_from = Percentage)

        # drop any columns that are all NA (if no sub-label)
        tmpout <- tmpout[, !sapply(tmpout, function(x) all(is.na(x)))]
      }
    }

      DT::datatable(tmpout,
        escape = FALSE,
        rownames = FALSE,
        class = c("nowrap", "stripe", "hover"),
        options = list(
          dom = 'Brt',
          buttons = list(
            list(
              extend = c('excel'),
              filename = paste0(q)
            )
          ),
          pageLength = 1000,
          lengthChange = TRUE,
          columnDefs = list(
            list(targets = '_all', className = 'dt-center')
          )
          ),
        extensions = 'Buttons',
        filter = 'top',
        caption = htmltools::tags$caption(
          style = 'caption-side: bottom; text-align: left;',
          htmltools::em(paste0(q,": ", qtext,
                               "; Total respondents: ", resp_count, "; Filters: ",  "; Group totals: ", group_mapping,
                               "; ", sig_let, "Significance at ", paste0(ci*100,"%"), " confidence intervals"))
        )) |>
      DT::formatStyle(columns = colnames(tmpout),
                      fontSize = '100%')
  }



