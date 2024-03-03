## function to convert from one shiny tab to another ##
convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}

remove_html <- function(string) {
  return(gsub("<.*?>", "", string))
}

# check to see if any user generated variables are in the survey
user_generated <- function(tbl, toc) {
  vars <- colnames(tbl)[grepl("^x", colnames(tbl))]
  qo <- seq(max(toc$question_order)+1, max(toc$question_order)+length(vars), 1)
  qid <- paste0("QIDx", sample(1000:9999, length(vars)))
  v1 <- data.frame(block = "User Generated", question_order = qo, export_name = vars,
                   question_id = qid, question_type = "User Generated",
                   question_text = vars, sub = NA, selector_type = NA)
}

# convert user generated variables into labelled data
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


# calculate mean duration across respondents
survey_duration <- function(x){
  paste0(gsub("\\..*", "", x %% 60),":", gsub(".*\\.", "", round(x %% 60, 2)))
}

# filter table of contents based on block
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

# build the filter options that are seen in the dropdown
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

filter_choices <- function(tbl, var) {
    attr(tbl[[var]], 'labels')
}





