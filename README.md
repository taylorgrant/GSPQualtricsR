
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GSPQualtricsR

<!-- badges: start -->
<!-- badges: end -->

GSPQualtricsR provides an interface to download and work through
Qualtrics surveys. It was written for primary usage through this [Shiny
app](https://github.com/taylorgrant/qualtricsApp), however it can be
used in R directly. Code to process the survey in R (wihthout using the
app) is below.

## Installation

You can install the development version of GSPQualtricsR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("taylorgrant/GSPQualtricsR")
```

## Functionality

This package assumes that you have grabbed your API key and base URL
from Qualtrics and saved these to the .Renviron as `QUALTRICS_KEY` and
`QUALTRICS_BASE_URL`. See [here](https://api.qualtrics.com/) for
information about Qualtrics API.

### Fetch SIDs

Qualtrics surveys are defined by an ID. These can be retrieved by using
the following function `fetch_sids()`, which returns the ID, survey
name, the owner of the survey, date the survey was last modified, the
date it was created, and whether the survey is active.

``` r
sids <- fetch_sids()
head(sids)
```

    #>                   id                               name            ownerId
    #> 1 SV_daOxRRSi6ceCPZk              Omnibus Template 2024 UR_0GpuwkcMPSkdaCx
    #> 2 SV_1Ugok6J73fOP5Xg              February Omnibus 2024 UR_0GpuwkcMPSkdaCx
    #> 3 SV_1LFA0dvRLjQnSFE             2024 GKL Qual Screener UR_0GpuwkcMPSkdaCx
    #> 4 SV_01Gjb4LMVV8xfz8               January Omnibus 2024 UR_0GpuwkcMPSkdaCx
    #> 5 SV_e2KF8r7BGyYwXNc                          test 2024 UR_0GpuwkcMPSkdaCx
    #> 6 SV_0JO0dpAuAZiJGU6 Xfinity Project Engineers Dec 2023 UR_0GpuwkcMPSkdaCx
    #>           lastModified creationDate isActive
    #> 1 2024-02-21T18:07:44Z   2024-02-21    FALSE
    #> 2 2024-02-29T17:49:12Z   2024-02-08     TRUE
    #> 3 2024-02-27T21:57:28Z   2024-02-06     TRUE
    #> 4 2024-02-09T20:59:40Z   2024-01-18    FALSE
    #> 5 2024-01-04T21:30:49Z   2024-01-04    FALSE
    #> 6 2023-12-14T19:31:54Z   2023-12-13     TRUE

### Fetch the survey of interest

With this data, we can then use the function `fetch_survey_data()` to
fetch the survey of interest. This function relies on some of the
[qualtRics](https://cran.r-project.org/web/packages/qualtRics/vignettes/qualtRics.html)
package for some ingest code, but it’s been modified to download the
survey in an SPSS rather than csv format (this is useful because the
data comes in a [haven](https://haven.tidyverse.org/) format that is
both labelled and numeric). The function also creates a Table of
Contents for all questions in the survey as well any user generated
variables that were created within Qualtrics and published with the data
(any user generated variables should start with a lower case “x” to be
found and recognized by the function).

From the sids, we’ll download the February Omnibus

``` r
data <- fetch_survey_data(sid = sids$id[2])
```

The function returns a list that includes (1) the survey data (in an
`as_survey_design()` format via the
[srvyr](https://cran.r-project.org/web/packages/srvyr/vignettes/srvyr-vs-survey.html)
package) and (2) the Table of Contents that includes information for
each question.

#### Table of Contents

The Table of Contents contains the Survey Block, the question order, the
export name (a user friendly question number), the question type
(multiple choice, text field, etc.), the text of the question, whether
there is a sub-label associated with the question (normally found in a
grid or matrix question), and the selector type (we really only care
about Likerts in this case).

``` r
head(data$toc)
```

    #>       block question_order second_order export_name question_id question_type
    #> 1 Front End              2            1          Q2     QID1710            MC
    #> 2 Front End              3            1          Q3     QID2422            TE
    #> 3 Front End              3            2     Cohorts    QID2422a        TE_AGE
    #> 4 Front End              3            3 Generations    QID2422b        TE_AGE
    #> 5 Front End              4            1          Q4     QID1345            MC
    #> 6 Front End              5            1          Q5      QID164            TE
    #>                                           question_text sub selector_type
    #> 1    Which of the following best describes your gender?              SAVR
    #> 2 How old are you? Please enter your current age below.                SL
    #> 3                    Respondent breakdown by age cohort                SL
    #> 4                    Respondent breakdown by generation                SL
    #> 5                           Which state do you live in?                DL
    #> 6      Please enter your five (5) digit zip code below:                SL

#### Survey

The survey data is in a survey design and all analysis under the hood is
conducted using the `srvyr` package. To see the raw survey data, you can
use the following (truncated):

``` r
head(data$svy$variables, 4)
```

    #>             StartDate             EndDate Status Progress Duration__in_seconds_
    #> 1 2024-02-12 23:19:58 2024-02-12 23:25:48      0      100                   350
    #> 2 2024-02-12 23:22:47 2024-02-12 23:31:03      0      100                   495
    #> 3 2024-02-12 23:22:11 2024-02-12 23:31:06      0      100                   534
    #> 4 2024-02-12 23:20:51 2024-02-12 23:31:30      0      100                   638
    #>   Finished        RecordedDate        ResponseId DistributionChannel
    #> 1        1 2024-02-12 23:25:49 R_3qGULCDmE1KYWvT           anonymous
    #> 2        1 2024-02-12 23:31:04 R_3yvsZFrLGyHFys9           anonymous
    #> 3        1 2024-02-12 23:31:06 R_7fs0NIfM8QFQb0v           anonymous
    #> 4        1 2024-02-12 23:31:30 R_7KPa8YkXGRR2e3W           anonymous
    #>   UserLanguage
    #> 1           EN
    #> 2           EN
    #> 3           EN
    #> 4           EN

### Analysis

This package makes analysis very simple via the wrapper function
`summarize_question()`. All that we have to do is define some
parameters.

The parameters that we can use are:

- **qid** - the `question_id` found in the Table of Contents
- **gid** - the `question_id` of any variable that we want to use as the
  crosstab selection
- **fiq** - the filters we want to apply; e.g., do we only want to
  include women in the analysis of the question. `fiq` is a list that
  includes the filter question (called by the `export_name` from the
  TOC) and the filter choices that should be included
- **ci** - the confidence interval to use for hypothesis testing
- **top_box / bottom_box** - if the qid is a Likert scaled question, do
  we want to summarize by Top N Box / Bottom N Box

The returned results include significance tests based on the confidence
interval set by the user.

If this seems a little complicated, it probably is because this package
was intended to be used with a Shiny app.

**The rest of this won’t contain results, just code:**

#### Single Question Analysis (no crosstab)

We’ll start with a simple analysis without a crosstab. Any call to
`summarize_question()` is going to return a list with two dataframes -
`out_win` and `out_bwn`. If there is no crosstab, then `out_bwn` is
NULL.

We’re going to look to a specific variable - Q15 - that asks respondents
what it was that they enjoyed about this year’s Super Bowl ads.

``` r
data$toc |> dplyr::filter(stringr::str_detect(export_name, "^Q15"))
```

    #> # A tibble: 8 × 9
    #>   block     question_order second_order export_name question_id question_type
    #>   <chr>              <dbl>        <int> <chr>       <chr>       <chr>        
    #> 1 SuperBowl             15            1 Q15_1       QID2701     MC           
    #> 2 SuperBowl             15            2 Q15_4       QID2701     MC           
    #> 3 SuperBowl             15            3 Q15_5       QID2701     MC           
    #> 4 SuperBowl             15            4 Q15_6       QID2701     MC           
    #> 5 SuperBowl             15            5 Q15_7       QID2701     MC           
    #> 6 SuperBowl             15            6 Q15_8       QID2701     MC           
    #> 7 SuperBowl             15            7 Q15_9       QID2701     MC           
    #> 8 SuperBowl             15            8 Q15_10      QID2701     MC           
    #> # ℹ 3 more variables: question_text <chr>, sub <chr>, selector_type <chr>

``` r
parameters = list(qid = "QID2701", gid = NULL, fiq = NULL, ci = .9)
tbl <- summarize_question(parameters, data = data)
```

The object `tbl` is a list and it will have some data that we don’t
really need. But we can use a helper function to create a DT table that
takes all necessary data.

``` r
build_table(tbl$out_win, ci = .9)
```

#### Crosstabbed Question Analysis

Say we wanted to crosstab the question by respondent’s generation.

``` r
parameters = list(qid = "QID2722", gid = "QID2422b", fiq = NULL, ci = .9)
tbl <- summarize_question(parameters, data = data)
```

Again `tbl` is a list, and we’re most interested in the `tbl$out_bwn`
which is capturing any statistical signifiance between the crosstabbed
groups, in this case our generations.

``` r
build_table(tbl$out_bwn, ci = .9)
```

#### Filter Usage

Assume we wanted to filter our responses to only include Females. To
find the variable for Females, we would look to the Table of Contents,
find our Gender variable and then access the raw data to look to the
coding. Filter work for single variable and crosstabbed summaries.

``` r
parameters = list(qid = "QID2701", gid = NULL, fiq = list(filterQ = "Q2", filter_choices = c("1")), ci = .9)
tbl <- summarize_question(parameters, data = data)
```

#### Top Box / Bottom Box

We’re switching our `qid` variable to a Likert scaled question. And
then, just as we did to identify the filter variable, we find the
numeric values that are the top and bottom box. We then simply add them
to the parameters.

``` r
parameters = list(qid = "QID2722", gid = "QID2422b", fiq = NULL, ci = .9, top_box = c("5", "6"), bottom_box = c(2,3))
tbl <- summarize_question(parameters, data = data)
```

#### Summarizing a Whole Block

Finally, we can summarize an entire survey block using the following.
Entire survey blocks can be crosstabbed and filtered, but because there
are sometimes multiple Likert scales within a single block, it wasn’t
feasible to include the ability to Top/Bottom box.

When we summarize a block, the data is returned in a large list, each
question in the block includes the same `out_win` and `out_bwn` tables.
But because the list is one level deeper than we want there is a bit of
extra code required from the user. Next, we want to export the data in a
nicely formatted excel doc.

``` r
parameters = list(qid = "QID2722", gid = "QID2422b", fiq = NULL, ci = .9)
tbl <- summarize_block("SuperBowl", parameters, data)
tbl <- unlist(tbl,recursive=FALSE)
openxlsx::write.xlsx(tbl, "~/Desktop/superbowl_block.xlsx")
```
