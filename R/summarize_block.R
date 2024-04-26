#' Summarize a full survey block from Qualtrics
#'
#' Function will take the survey block and all other parameters (confidence interval, crosstabs, and any filters) and run analysis and significance testing
#'
#' @param block_choice What is the survey block to be summarized
#' @param parameters What are the parameters to pass through for crosstabbing and confidence intervals
#' @param data The survey data to pass through
#'
#' @return List of data frames; each with the question number and whether the siginficance testing was within/between groups
#' @export
#'
#' @examples
#' \dontrun{
#' tbl <- summarize_block("Block2", parameters, data)
#' }
summarize_block <- function(block_choice, parameters, data) {
  # pull TOC data for a given block
  blockinfo <- data$toc |> dplyr::filter(block == block_choice) |>
    dplyr::filter(question_type != 'TE')
  block_qids <- unique(dplyr::pull(blockinfo, question_id))
  # function to pull info for each Q in block
  get_qinfo <- function(qid) {
    parameters[[qid]] <- qid
    qinfo <- blockinfo |> dplyr::filter(question_id == qid)
    qvct <- dplyr::pull(qinfo, export_name)
    conf_level <- as.numeric(parameters$ci)
    if (is.null(parameters$gid)) {
      group <- data.frame(group = NA, gsub = NA)
    } else {
      group <- data$toc |> dplyr::filter(question_id == parameters$gid) |>
        dplyr::select(group = export_name, gsub = sub)
    }
    if (is.null(parameters$fiq$filterQ)) {
      filters <- list(filterQ = NA, filter_choices = NA)
      filter_text <- "No filter"
    } else {
      filters <- parameters$fiq
      filter_text <- paste(names(filter_choices(data$svy$variables, filters$filterQ))[as.numeric(filters$filter_choices)], collapse = ", ")
    }
    crossed <- tidyr::crossing(qinfo, group)
    out <- purrr::pmap_df(list(block = crossed$block, var = crossed$export_name, qname = crossed$question_text,
                            qsub = crossed$sub, selector = crossed$selector_type,
                            group = crossed$group, gsub = crossed$gsub),
                       resps = qvct, filters = filters,
                       get_responses, data = data)
    # clean up the var_num
    if ("group_label" %in% names(out)) {
      out <- out |>
        dplyr::group_by(question_sub, group_label, group_sub) |>
        dplyr::mutate(var_num = dplyr::row_number()) |>
        dplyr::ungroup()
    } else {
      out <- out |>
        dplyr::group_by(question_sub) |>
        dplyr::mutate(var_num = dplyr::row_number()) |>
        dplyr::ungroup()
    }
    attr(out, "filter_text") <- filter_text
    out
    out <- block_significance(out, conf_level)
  }
  block_qids |> purrr::map(get_qinfo)

}

# tbl <- summarize_block("SuperBowl", parameters, data)


# need to remove a level of lists from a list of lists
# tbl <- unlist(tbl,recursive=FALSE)

# write to file
# openxlsx::write.xlsx(tbl, "~/Desktop/testexcel.xlsx")

