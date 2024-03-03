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
  gen_labels = c("Gen Alpha" = 1, "Gen Z" = 2, "Millennials" = 3,"Gen X" = 4,
                 "Boomers" = 5, "Silent" = 6, "Greatest" = 7)

  if (haven::is.labelled(data[[ageq]])) {
    tmpdat <- data |>
      dplyr::mutate(age = as.numeric(as.character(haven::as_factor(!!quo_var))))
  } else {
    tmpdat <- data |>
      dplyr::mutate(age = !!quo_var)
  }
  tmpdat |>
    dplyr::mutate(yob = lubridate::year(Sys.Date()) - age) |>
    dplyr::mutate(Generations = dplyr::case_when(yob < 2013 & yob > 1996 ~ 2,
                                                 yob < 1997 & yob > 1980 ~ 3,
                                                 yob < 1981 & yob > 1964 ~ 4,
                                                 yob < 1965 & yob > 1945 ~ 5,
                                                 yob < 1946 & yob > 1927 ~ 6,
                                                 yob < 1928 ~ 7,
                                                 yob > 2012 ~ 1),
                  Generations = haven::labelled(Generations, labels = gen_labels)) |>
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
  cohort_labels <- c("18-24" = 1, "25-34" = 2, "35-44" = 3, "45-54" = 4, "55-64" = 5, "65+" = 6)

  if (haven::is.labelled(data[[ageq]])) {
    tmpdat <- data |>
      dplyr::mutate(age = as.numeric(as.character(haven::as_factor(!!quo_var))))
  } else {
    tmpdat <- data |>
      dplyr::mutate(age = !!quo_var)
  }
  tmpdat |>
    dplyr::mutate(Cohorts = dplyr::case_when(age <= 24 ~ 1,
                                             age > 24 & age <= 34 ~ 2,
                                             age > 34 & age <= 44 ~ 3,
                                             age > 44 & age <= 54 ~ 4,
                                             age > 54 & age <= 64 ~ 5,
                                             age > 64 ~ 6),
                  Cohorts = haven::labelled(Cohorts, labels = cohort_labels)) |>
    dplyr::select(-age)
}

