% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_generation.R
\name{add_generation}
\alias{add_generation}
\title{Add generations to survey data}
\usage{
add_generation(data, ageq)
}
\arguments{
\item{data}{Survey data imported/read into R in dataframe format}

\item{ageq}{Question number in survey that asks for respondent age}
}
\value{
A new "\code{Generation}" column in the dataframe
}
\description{
This is a helper function meant to estimate respondent generations. It is very specific
and requires a survey question that asks for each respondent's age.
}
\examples{
\dontrun{
svy |> add_generation("Q3")
}
}
