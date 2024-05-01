#' Convert one menu item to another in shiny
#'
#' @param mi blah
#' @param tabName blah
#'
#' @return blah
#' @keywords internal
#' @export
convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}

#' Helper function - remove html
#'
#' @param string blah
#'
#' @return string
#' @keywords internal
#' @export
remove_html <- function(string) {
  return(gsub("<.*?>", "", string))
}


#' Check to see if user generated variables are available in the survey
#'
#' These variables must begin with an "x"
#' On survey load, the function find the user generated variables and adds them to the TOC
#'
#' @param tbl Survey
#' @param toc Table of contents
#'
#' @return Updated table of contents
#' @export
#'
#' @examples
#' \dontrun{
#' user_generated(tbl, toc)
#' }
user_generated <- function(tbl, toc) {
  vars <- colnames(tbl)[grepl("^x", colnames(tbl))]
  qo <- seq(max(toc$question_order)+1, max(toc$question_order)+length(vars), 1)
  qid <- paste0("QIDx", sample(1000:9999, length(vars)))
  v1 <- data.frame(block = "User Generated", question_order = qo, export_name = vars,
                   question_id = qid, question_type = "User Generated",
                   question_text = vars, sub = character(0), selector_type = character(0))
}


#' Convert user generated variables into labelled data (`haven` labelled spss data)
#'
#' @param tbl Survey
#' @param toc TOC
#'
#' @return Survey data frame with labelled user generated vars
#' @export
#'
#' @examples
#' \dontrun{
#' create_labelled(tbl, toc)
#' }
create_labelled <- function(tbl, toc) {
  ugc <- toc[toc$block == "User Generated", ]$export_name

  run_labels <- function(tbl, ugc) {
    x <- 1:length(unique(tbl[[ugc]]))
    names(x) <- sort(unique(tbl[[ugc]]))
    tbl <- tbl |>
      dplyr::mutate(dplyr::across(!!rlang::sym(ugc), ~x[.]),
                    dplyr::across(!!rlang::sym(ugc), ~haven::labelled(., labels = x))) |>
      dplyr::select(!!rlang::sym(ugc))
  }
  out <- purrr::map_dfc(ugc, ~run_labels(tbl, .x))
  tbl |>
    dplyr::select(-names(out)) |>
    dplyr::bind_cols(out)
}


#' Calculate mean survey duration across respondents
#'
#' @param x blah
#'
#' @return time variable
#' @keywords internal
#' @export
survey_duration <- function(x){
  paste0(gsub("\\..*", "", x %% 60),":", gsub(".*\\.", "", round(x %% 60, 2)))
}


#' Filter table of contents by block
#'
#' @param tbl Survey
#' @param blck Survey block
#'
#' @return Filtered toc
#' @keywords internal
#' @export
toc_filter <- function(tbl, blck) {
  if (any(tbl$block == blck)) {
    tmp <- tbl |>
      dplyr::filter(block %in% blck) |>
      dplyr::mutate(newtoc = paste0("Q", question_order, ": ", question_text))
    # grab newtoc and set names
    tmptoc <- tmp$question_id |>
      purrr::set_names(nm = tmp$newtoc)
  }  else {
    tmp <- tbl |>
      dplyr::mutate(newtoc = paste0("Q", question_order, ": ", question_text))
    # now grab newtoc and set names
    tmptoc <- tmp$question_id |>
      purrr::set_names(nm = tmp$newtoc)
  }
}


#' Build filter options seen in app dropdown
#'
#' @param toc TOC
#' @param blockfilter Filter
#'
#' @return updated TOC
#' @keywords internal
#' @export
pull_filter_toc <- function(toc, blockfilter) {
  if (blockfilter == "ALL BLOCKS") {
    tmpdat <- toc |>
      dplyr::filter(question_type != "TE") |>
      dplyr::filter(!stringr::str_detect(question_text, stringr::fixed("Welcome to our survey"))) |>
      dplyr::mutate(question_text = ifelse(sub == "", paste0("Q",question_order,": ", question_text),
                                           paste0("Q",question_order,": ", question_text, " - ", sub)),
                    question_text = ifelse(grepl("^x", export_name), export_name, question_text)) |>
      dplyr::select(export_name, question_text)
  } else {
    tmpdat <- toc |>
      dplyr::filter(block == blockfilter) |>
      dplyr::filter(question_type != "TE") |>
      dplyr::filter(!stringr::str_detect(question_text, stringr::fixed("Welcome to our survey"))) |>
      dplyr::mutate(question_text = ifelse(sub == "", paste0("Q",question_order,": ", question_text),
                                           paste0("Q",question_order,": ", question_text, " - ", sub)),
                    question_text = ifelse(grepl("^x", export_name), export_name, question_text)) |>
      dplyr::select(export_name, question_text)
  }
  out <- c("", tmpdat$export_name)
  names(out) <- c("", tmpdat$question_text)
  out
}

#' Filtered choices available to user in app
#'
#' @param tbl Survey data
#' @param var Labels of specific variable
#'
#' @return named vector
#' @keywords internal
#' @export
filter_choices <- function(tbl, var) {
    attr(tbl[[var]], 'labels')
}





