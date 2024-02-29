#' Add generations to survey data
#'
#' This is a helper function meant to estimate respondent generations. It is very specific
#' and requires a survey question that asks for each respondent's age.
#'
#' @param data Survey data imported/read into R in dataframe format
#' @param ageq Question number in survey that asks for respondent age
#'
#' @return A new "`Generation`" column in the dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' svy |> add_generation("Q3")
#' }
add_generation <- function(data, ageq) {
  quo_var <- rlang::sym(ageq)
  if (haven::is.labelled(data[[ageq]])) {
    tmpdat <- data |>
      dplyr::mutate(age = as.numeric(as.character(haven::as_factor(!!quo_var))))
  } else {
    tmpdat <- data |>
      dplyr::mutate(age = !!quo_var)
  }
  tmpdat |>
    dplyr::mutate(yob = lubridate::year(Sys.Date()) - age) |>
    dplyr::mutate(Generations = dplyr::case_when(yob < 2013 & yob > 1996 ~ 'Gen Z',
                                                 yob < 1997 & yob > 1980 ~ 'Millennials',
                                                 yob < 1981 & yob > 1964 ~ 'Gen X',
                                                 yob < 1965 & yob > 1945 ~ 'Boomers',
                                                 yob < 1946 & yob > 1927 ~ 'Silent',
                                                 yob < 1928 ~ 'Greatest',
                                                 yob > 2012 ~ 'Post-Z'),
                  Generations = factor(Generations, levels = c("Gen Z", "Millennials",
                                                             "Gen X", "Boomers"))) |>
    dplyr::select(-age)
}

#' Add age cohorts to survey data
#'
#' @param data Survey data imported/read into R in dataframe format
#' @param ageq Question number in survey that asks for respondent age
#'
#' @return A new "`Cohort`" column in the dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' svy |> add_cohort("Q3")
#' }
add_cohort <- function(data, ageq) {
  quo_var <- rlang::sym(ageq)
  if (haven::is.labelled(data[[ageq]])) {
    tmpdat <- data |>
      dplyr::mutate(age = as.numeric(as.character(haven::as_factor(!!quo_var))))
  } else {
    tmpdat <- data |>
      dplyr::mutate(age = !!quo_var)
  }
  tmpdat |>
    dplyr::mutate(Cohorts = dplyr::case_when(age <= 24 ~ "18-24",
                                             age > 24 & age <= 34 ~ "25-34",
                                             age > 34 & age <= 44 ~ "35-44",
                                             age > 44 & age <= 54 ~ "45-54",
                                             age > 54 & age <= 64 ~ "55-64",
                                             age > 64 ~ "65+"),
                  Cohorts = factor(Cohorts, levels = c("18-24", "25-34", "35-44",
                                                     "45-54", "55-64", "65+"))) |>
    dplyr::select(-age)
}
