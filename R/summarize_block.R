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
    conf_level <- parameters$ci
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
    out <- block_significance(out, conf_level, filter_text)
    out
  }
  block_qids |> purrr::map(get_qinfo) |>
    purrr::set_names(nm = "block_out")
}

# tbl <- summarize_block("SuperBowl", parameters)
# write to file
# openxlsx::write.xlsx(tbl$block_out, "~/Desktop/testexcel.xlsx")

