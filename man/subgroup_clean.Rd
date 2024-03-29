% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sublabel_clean.R
\name{subgroup_clean}
\alias{subgroup_clean}
\title{Clean the sub-label of the group}
\usage{
subgroup_clean(tbl, gsub)
}
\arguments{
\item{tbl}{Summarized survey question}

\item{gsub}{Group sub-labels passed through by the \code{summarize_question()} function}
}
\value{
Dataframe, but sub-labels are either NA or hopefully informative
}
\description{
This is essentially the same function as \code{subquestion_clean()} but for the group sub-label. Normally there shouldn't be much need for this, but if someone chooses to use a funky question as a grouping variable, we can end up with poorly formatted or missing group sub-labels that provide context.
}
\examples{
\dontrun{
na
}
}
