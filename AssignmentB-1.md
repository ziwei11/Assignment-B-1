Assignment B-1
================
Ziwei Zhao
2022-11-02

# Setup

1.  Go to canvas to get your invitation to create a GitHub repository
    for this project. You can find this in the description of Assignment
    B-1. Name your repo as you wish – keeping in mind that you’ll be
    using this repo for Assignment B-2, where you’ll be bundling your
    work from Assignment B-1 into an R package.
2.  Make a new Rmd file containing all of your code. Be sure to use some
    dialogue between code chunks.

``` r
#library(datateachr) # <- might contain the data you picked!
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(datateachr)
library(palmerpenguins)
library(testthat)
```

    ## 
    ## Attaching package: 'testthat'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null
    ## 
    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

``` r
library(dplyr)
library(gapminder)
library(digest)
#library(digest)
```

# Tidy Submission (15 points)

Follow these steps to submit your work. Be sure to familiarize yourself
with the rubric for a tidy submission below, before doing these steps.

1.  Make a README file for your repository. It should be a brief
    document letting a visitor know what’s in this repository (at a high
    level) and some key things they should know about how to use the
    files in the repository.
2.  Tag a release in your GitHub repository corresponding to your
    submission before the deadline.
3.  Grab the URL corresponding to your tagged release, and submit that
    to canvas.

## Rubric:

-   The above steps were followed.
-   Your work must be reproducible from beginning to end, error-free.
-   Code should adopt a consistent and easy-to-read style – ideally, the
    tidyverse style, although we’re certainly not looking for strict
    adherence.
-   You use proper English, spelling, and grammar, and write concisely.
    If there’s any uncertainty in determining a grade here, the UBC MDS
    writing rubric will be referred to.
-   If there’s any further uncertainty in determining a grade for this
    tidy submission portion, the UBC MDS mechanics rubric will be
    referred to.

# Exercise 1: Make a Function (25 points)

In this exercise, you’ll be making a function and fortifying it. The
function need not be complicated. The function need not be “serious”,
but shouldn’t be nonsense.

## Function Ideas

-   Did you repeat any code for a data analysis in STAT 545A? If so,
    consider making a function for this action.
-   Consider bundling a specific `group_by() %>% summarise()` workflow.
-   Write a wrapper around an existing function.
    -   For example, perhaps accepting a narrower range of inputs (like
        not allowing logical vectors), or providing a different output.
    -   A specific example: my `rqdist()` function is a wrapper around
        `quantreg::rq()`, narrowing its functionality.
    -   It’s usually better to narrow a function’s focus than to
        broaden, so that a function doesn’t end up doing too much.
-   Make a function extracting parts of an `"lm"` object that you can’t
    easily access (`broom` aside), or measuring the “difference” between
    a linear and quadratic fit.
-   Make a special plot that you’d want to repeat when exploring your
    data.
-   …

## Guidelines

-   Your function should not rely on anything from your working
    environment.
-   Your function should not rely on “magic numbers” – pre-selected
    numbers (or options) that appear inside the function that can’t be
    accessed by a user of the function.
    -   For example, maybe `quantile(x, type = 1, ...)` appears in your
        function. The choice of 1 is arbitrary – unless you’re making a
        function like `quantile_type_1()`.
-   Input should not take a rigid form. An example that’s too rigid is a
    data frame that’s expected to have special types of columns.
-   The output is consistent – for example, always gives a list. An
    example of inconsistent output: `sapply(1:3, seq_len)` gives a list,
    and `sapply(1:3, sqrt)` gives an (atomic) vector.
-   Your function includes appropriate arguments. (Do you handle NA’s
    appropriately? Are you using the ellipsis properly? etc.)

# Exercise 2: Document your Function (20 points)

In the same code chunk where you made your function, document the
function using `roxygen2 tags`. Be sure to include:

1.  Title.
2.  Function description: In 1-2 brief sentences, describe what the
    function does.
3.  Document each argument with the `@param` tag, making sure to justify
    why you named the parameter as you did.

-   (Justification for naming is not often needed, but we want to hear
    your reasoning.)

4.  What the function returns, using the `@return` tag.

``` r
# Exercise 1 & Exercise 2

#' Count the data number of each group
#' @description `count_group_by` returns the number of the value choose_value in the column group_by_column of the data set dataset
#' @param dataset the data set that we choose to use
#' @param group_by_column the column that we choose to apply group_by function on
#' @param choose_value the value that we want to count
#' @return A 1 x 2 tibble with value "choose_value" in column "group_by_column" and the count number n of this value

count_group_by <- function(dataset, group_by_column, choose_value) {
  if (!is.character(group_by_column)) {
    stop("group_by_column must be a string.")
  }
  if (is.na(choose_value)) {
    stop("choose_value should not be NA.")
  }
  return(dataset %>% 
    drop_na(group_by_column) %>%
    group_by(.data[[group_by_column]]) %>%
    count() %>%
    filter(.data[[group_by_column]] == choose_value))
}
```

# Exercise 3: Include examples (15 points)

Demonstrate the usage of your function with a few examples. Use one or
more new code chunks, describing what you’re doing.

Note: If you want to deliberately show an error, you can use
`error = TRUE` in your code chunk option.

Here, we choose the `building_permits` dataset and test whether there
are suitable variable in this dataset

``` r
head(building_permits)
```

    ## # A tibble: 6 × 14
    ##   permit_nu…¹ issue_date proje…² type_…³ address proje…⁴ build…⁵ build…⁶ appli…⁷
    ##   <chr>       <date>       <dbl> <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
    ## 1 BP-2016-02… 2017-02-01       0 Salvag… 4378 W… <NA>    <NA>     <NA>   Raffae…
    ## 2 BU468090    2017-02-01       0 New Bu… 1111 R… <NA>    <NA>     <NA>   MAX KE…
    ## 3 DB-2016-04… 2017-02-01   35000 Additi… 3732 W… <NA>    <NA>     <NA>   Peter …
    ## 4 DB-2017-00… 2017-02-01   15000 Additi… 88 W P… <NA>    Mercur… "88 W … Aaron …
    ## 5 DB452250    2017-02-01  181178 New Bu… 492 E … <NA>    082016… "3559 … John H…
    ## 6 BP-2016-01… 2017-02-02       0 Salvag… 3332 W… <NA>    <NA>     <NA>   Shalin…
    ## # … with 5 more variables: applicant_address <chr>, property_use <chr>,
    ## #   specific_use_category <chr>, year <dbl>, bi_id <dbl>, and abbreviated
    ## #   variable names ¹​permit_number, ²​project_value, ³​type_of_work,
    ## #   ⁴​project_description, ⁵​building_contractor, ⁶​building_contractor_address,
    ## #   ⁷​applicant

We can find that the variables `type_of_work` and `issue_date` might be
a suitable column to use group_by function. First, we want to know how
many **New Building** in these buildings.

``` r
count_group_by(building_permits, "type_of_work", "New Building")
```

    ## Note: Using an external vector in selections is ambiguous.
    ## ℹ Use `all_of(group_by_column)` instead of `group_by_column` to silence this message.
    ## ℹ See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    ## This message is displayed once per session.

    ## # A tibble: 1 × 2
    ## # Groups:   type_of_work [1]
    ##   type_of_work     n
    ##   <chr>        <int>
    ## 1 New Building  4728

Second, we want to know how many building’s issue date is
**2017-02-02**.

``` r
count_group_by(building_permits, "issue_date", as.Date("2017-02-02", format = "%Y-%m-%d"))
```

    ## # A tibble: 1 × 2
    ## # Groups:   issue_date [1]
    ##   issue_date     n
    ##   <date>     <int>
    ## 1 2017-02-02    29

Third, we want to know how many buildings project_value is **0**.

``` r
count_group_by(building_permits, "project_value", 0)
```

    ## # A tibble: 1 × 2
    ## # Groups:   project_value [1]
    ##   project_value     n
    ##           <dbl> <int>
    ## 1             0  3218

Throw an error if choose_value is NA.

``` r
count_group_by(building_permits, "type_of_work", NA)
```

    ## Error in count_group_by(building_permits, "type_of_work", NA): choose_value should not be NA.

Throw an error if the column format is wrong.

``` r
count_group_by(building_permits, 33, "New Building")
```

    ## Error in count_group_by(building_permits, 33, "New Building"): group_by_column must be a string.

# Exercise 4: Test the Function (25 points)

Running examples is a good way of checking by-eye whether your function
is working as expected. But, having a formal “yes or no” check is useful
when you move on to other parts of your analysis.

Write formal tests for your function. You should use at least three
non-redundant uses of an `expect_()` function from the `testthat`
package, and they should be contained in a `test_that()` function (or
more than one). They should all pass.

Example of non-redundant inputs:

-   Vector with no NA’s
-   Vector that has NA’s
-   Vector of a different type (if relevant)
-   Vector of length 0, like `numeric(0)`.

Example of redundant inputs:

-   Providing a different number (unless one of these numbers have some
    significance, like a limit point – just tell us if that’s the case)

First, we use the expect_equal() to test if the value is the same with
the true value, then we use the expect_error() to test if inputting a
wrong value throws an error

``` r
test_that("Testing count_group_by function with dataset building_permits", {
  expect_equal(count_group_by(building_permits, "type_of_work", "New Building"), building_permits %>% drop_na(type_of_work) %>% group_by(type_of_work) %>% count() %>% filter(type_of_work == "New Building"))
  expect_equal(count_group_by(building_permits, "issue_date", as.Date("2017-02-04", format = "%Y-%m-%d")), building_permits %>% drop_na(issue_date) %>% group_by(issue_date) %>% count() %>% filter(issue_date == as.Date("2017-02-04", format = "%Y-%m-%d")))
  expect_equal(count_group_by(building_permits, "project_value", 0), building_permits %>% drop_na(project_value) %>% group_by(project_value) %>% count() %>% filter(project_value == 0))
  
  expect_error(count_group_by(building_permits, "address", NA), "choose_value should not be NA.")
  expect_error(count_group_by(building_permits, "permit_number", NA), "choose_value should not be NA.")
  expect_error(count_group_by(building_permits, 21, "Addition / Alteration"), "group_by_column must be a string.")
})
```

    ## Test passed 🌈
