#' Fetch survey data and TOC from Qualtrics via the API
#'
#' @param sid Qualtrics survey ID of a specific survey
#'
#' @return Named list - "`svy`" is an unweighted survey design object from `srvyr::as_design()`; "`toc`" is a table of contents containing the following columns: survey block, question order, question name, question id, question type, question text, and sub-labels used, and the question selector type the respondent used.
#'
#' Question Types
#' Multiple Choice (MC); Text (TE); Matrix; Slider; Highlighter (HL); HeatMap; Drill Down (DD); Rank Order (RO); Side by Side (SBS or SS)
#'
#' Selector Types
#' Single Answer (SA or SAVR); Multiple Answer (MA or MAVR); Text Entry (TE); Likert; TB (Text Box)
#' @export
#'
#' @examples
#' \dontrun{
#' out <- fetch_survey_data(sid = "[Survey ID]")
#' }
fetch_survey_data <- function(sid) {

  # FN - get survey out of Qualtrics
  fetch_survey <- function(sid){
    tmp_dir = tempdir()
    # Get Qualtrics API Key and Base URL (assuming it's in .renviron)
    api_key <- Sys.getenv('QUALTRICS_KEY')
    base_url <- Sys.getenv('QUALTRICS_BASE_URL')

    # POST to qualtrics with SID #
    url <- glue::glue("https://{base_url}/API/v3/surveys/{sid}/export-responses")
    payload <- "{\n  \"format\": \"spss\"\n}"
    encode <- "json"
    response <- httr::VERB("POST", url, body = payload, httr::add_headers('X-API-TOKEN' = api_key),
                           httr::content_type("application/json"),
                           httr::accept("application/json"),
                           encode = encode)
    progress_id <- httr::content(response)$result$progressId

    # Check progress of download #
    response_progress <- function(sid, progress_id) {
      url <- glue::glue("https://{base_url}/API/v3/surveys/{sid}/export-responses/{progress_id}")
      # progress
      progress <- 0
      while (progress < 100) {
        CU <- httr::content(httr::VERB("GET", url, httr::add_headers('X-API-TOKEN' = api_key),
                                       httr::content_type("application/octet-stream"),
                                       httr::accept("application/json")))
        progress <- CU$result$percentComplete
      }
      CU$result$fileId
    }
    file_id <- response_progress(sid, progress_id)

    # Get the file url
    url <- glue::glue("https://{base_url}/API/v3/surveys/{sid}/export-responses/{file_id}/file")
    response <- httr::VERB("GET", url, httr::add_headers('X-API-TOKEN' = api_key),
                           httr::content_type("application/octet-stream"),
                           httr::accept("application/octet-stream, application/json"))
    url <- glue::glue("https://{base_url}/API/v3/surveys/{sid}/export-responses/{file_id}/file")

    # set up tmp file
    zip_path <- fs::file_temp(ext = "zip", tmp_dir = tmp_dir)
    withr::defer(fs::file_delete(zip_path))

    # Download, unzip, and load the file
    raw_zip <- httr::content(httr::GET(url, httr::add_headers(`X-API-TOKEN` = api_key)), "raw")

    # write to temporary file
    writeBin(raw_zip, zip_path)

    # get filename of internal spss file
    spss_filename_df <- utils::unzip(
      zipfile = zip_path,
      list = TRUE
    )
    spss_filename <- spss_filename_df[["Name"]][1]

    # read in zip file
    utils::unzip(zip_path, exdir = tmp_dir)

    # read in raw data
    svy <- haven::read_spss(file = file.path(tmp_dir, spss_filename))
  }

  # FN - build TOC and Blocklist for app
  get_survey_data <- function(sid) {
    # survey design (yes)
    d <- qsurvey::design(id = sid)

    # have to get proper q ordering (metadata is wrong sometimes)
    meta <- qualtRics::fetch_survey(sid) |>
      qualtRics::extract_colmap() |>
      dplyr::filter(!stringr::str_detect(qname, "_DO_")) |>
      dplyr::filter(stringr::str_detect(qname, "^Q")) |>
      dplyr::select(export_name = qname, sub, question_id = ImportId) |>
      dplyr::mutate(question_id = gsub("\\_.*", "", question_id),
                    sub = trimws(gsub("[\r\n\t]", " ", sub)))

    # helper to strip html tags
    remove_html <- function(string) {
      return(gsub("<.*?>", "", string))
    }
    # survey questions
    svy_q <- qsurvey::questions(design_object = d) |>
      dplyr::mutate(question_text = remove_html(question_text), # strip html tags
                    question_text = gsub("[\r\n\t]", " ", question_text), # strip line breaks
                    question_text = trimws(gsub("\\s+", " ", question_text)),
                    question_text = stringr::str_replace_all(question_text, "&quot;", "'")) |>
      dplyr::filter(question_type != "DB") |>
      dplyr::as_tibble() |>
      dplyr::distinct(question_order, question_id, question_type, question_text) |>
      dplyr::left_join(meta, by = "question_id") |>
      dplyr::relocate(export_name, .after = question_order)

    # survey blocks (yes)
    get_blocks <- function(bl) {
      blockname <- d$blocks[[bl]]$description

      qids <- data.table::rbindlist(d$blocks[[bl]]$elements, fill = TRUE) |>
        data.frame() |>
        dplyr::filter(type == "Question") |>
        dplyr::mutate(block = blockname) |>
        dplyr::select(-type, question_id = questionId)
    }
    blockbuild <- purrr::map_dfr(names(d$blocks), get_blocks)

    # function to extract selector type (Likert, Bipolar, etc)
    get_selector <- function(q) {
      tmp <- d$questions[[q]]$questionType$selector
    }

    # keep a tidy TOC of distinct questions
    toc <- svy_q |>
      dplyr::left_join(blockbuild) |>
      dplyr::relocate(block, .before = "question_order") |>
      dplyr::mutate(selector_type = purrr::map(question_id, get_selector)) |>
      dplyr::filter(!stringr::str_detect(export_name, "_TEXT")) |>
      tidyr::unnest(cols = selector_type)

    # FN - Generation and Cohort (this is based on specific Q wording so pretty inflexible)
    cohort_generation <- function(tbl) {
      if (any(toc$question_text == "How old are you? Please enter your current age below.")) {
        v1 <- tbl |>
          dplyr::filter(question_text == "How old are you? Please enter your current age below.") |>
          dplyr::mutate(question_id = paste0(question_id, "a"),
                        question_type = "TE_AGE",
                        question_text = "Respondent breakdown by age cohort",
                        export_name = "Cohorts")

        v2 <- tbl |>
          dplyr::filter(question_text == "How old are you? Please enter your current age below.") |>
          dplyr::mutate(question_id = paste0(question_id, "b"),
                        question_type = "TE_AGE",
                        question_text = "Respondent breakdown by generation",
                        export_name = "Generations")

        age_add <- rbind(v1, v2)
      }
    }
    toc <- rbind(toc, cohort_generation(toc)) |>
      dplyr::arrange(question_order)
  }

  # table of contents
  toc <- get_survey_data(sid)

  # get survey (spss version)
  svy <- fetch_survey(sid)

  if (any(toc$question_type == "TE_AGE")) {
    age_q <- toc[which(toc$question_text == "How old are you? Please enter your current age below."),]$export_name
    svy <- svy |>
      add_generation(age_q) |>
      add_cohort(age_q)
  }

  # convert survey into a survey design
  svy <- svy |>
    srvyr::as_survey()

  # return a named list
  list(svy = svy, toc = toc)
}
