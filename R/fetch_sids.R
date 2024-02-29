#' Fetch all survey IDs from Qualtrics
#'
#' This function assumes that the Qualtrics API key and subdomain are in the .renviron
#'
#' @return Dataframe with survey dates, survey IDs and survey names
#' @export
#'
#' @examples
#' \dontrun{
#' sids <- fetch_sids()
#' }
fetch_sids <- function(){
  qualtRics::all_surveys() |>
    dplyr::mutate(creationDate = as.Date(creationDate)) |>
    dplyr::arrange(dplyr::desc(creationDate))
}
