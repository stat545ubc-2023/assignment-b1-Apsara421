AssignmentB1
================
Apsara
2023-10-26

``` r
library(palmerpenguins)
```

    ## Warning: package 'palmerpenguins' was built under R version 4.2.3

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.2.3

    ## Warning: package 'ggplot2' was built under R version 4.2.3

    ## Warning: package 'readr' was built under R version 4.2.3

    ## Warning: package 'lubridate' was built under R version 4.2.3

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.0     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.1     ✔ tibble    3.1.8
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the ]8;;http://conflicted.r-lib.org/conflicted package]8;; to force all conflicts to become errors

``` r
library(gapminder)
```

    ## Warning: package 'gapminder' was built under R version 4.2.3

``` r
library(dplyr)
library(datateachr)
library(testthat)
```

    ## Warning: package 'testthat' was built under R version 4.2.3

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
#Example of code I repeated a lot in my analysis

trees_average_diameter <- vancouver_trees %>%
  group_by(genus_name)%>%
  summarize(average_diameter= mean(diameter))
# Now we can look at this summarized data

view(trees_average_diameter)
```

Now, I will make a function that takes in a dataset and groups by a
certain categorical variable. I will also make it such that function
finds the mean of another numeric variable of the grouped variable.

``` r
#' @title Documentation for average_by_group function
#' @description This function is meant to help summarize the dataset, by essentially taking in a categorical variable that the user wants to group the data by, and then calculating the mean of a specific numeric variable that the user inputs. 
#' @param data character (1) string, This is the input dataset that the user is working with. I named this argument because it is where the user inputs the dataset they are working with.
#' @param group character (1) string, This is the categorical variable that the user wants to group the dataset by. I names this as group because it specifies the group or categorical variable the user wants to categorize the data by.
#' @param numeric_category character (1) string, This is the numeric variable that the user wants to find the mean of, grouped by the 'group' variable. I names this numeric_category because it lets the user know that this is the numerical category that the mean will be calculated from. 
#' @param na.rm logical value, This is how NA values will be handled in the data. By default, it is set to FALSE, but the user can change the input 
#' @param ... This allows the user to specify any additional inputs to be used within the mean function
#'
#' @return tibble, returns a tibble with the data summarized by group and by the average of a numeric variable within the groups.

average_by_group <- function(data, group, numeric_category, na.rm= FALSE, ...) 
  # Check if group is numeric
  {check_numerical <- data %>%
    pull({{numeric_category}}) %>%
    is.numeric
  
if (check_numerical) {data %>%
    group_by({{group}})%>%
    summarize(average_variable := mean({{numeric_category}}, na.rm= na.rm, ...))
  
  
  }
else stop("The group variable must not be numeric.")
  }
```

Examples Example 1: Check if the function works

``` r
#I will demonstrate the use of this function from the vancouver trees dataset
vancouver_trees
```

    ## # A tibble: 146,611 × 20
    ##    tree_id civic_number std_street    genus_name species_name cultivar_name  
    ##      <dbl>        <dbl> <chr>         <chr>      <chr>        <chr>          
    ##  1  149556          494 W 58TH AV     ULMUS      AMERICANA    BRANDON        
    ##  2  149563          450 W 58TH AV     ZELKOVA    SERRATA      <NA>           
    ##  3  149579         4994 WINDSOR ST    STYRAX     JAPONICA     <NA>           
    ##  4  149590          858 E 39TH AV     FRAXINUS   AMERICANA    AUTUMN APPLAUSE
    ##  5  149604         5032 WINDSOR ST    ACER       CAMPESTRE    <NA>           
    ##  6  149616          585 W 61ST AV     PYRUS      CALLERYANA   CHANTICLEER    
    ##  7  149617         4909 SHERBROOKE ST ACER       PLATANOIDES  COLUMNARE      
    ##  8  149618         4925 SHERBROOKE ST ACER       PLATANOIDES  COLUMNARE      
    ##  9  149619         4969 SHERBROOKE ST ACER       PLATANOIDES  COLUMNARE      
    ## 10  149625          720 E 39TH AV     FRAXINUS   AMERICANA    AUTUMN APPLAUSE
    ## # ℹ 146,601 more rows
    ## # ℹ 14 more variables: common_name <chr>, assigned <chr>, root_barrier <chr>,
    ## #   plant_area <chr>, on_street_block <dbl>, on_street <chr>,
    ## #   neighbourhood_name <chr>, street_side_name <chr>, height_range_id <dbl>,
    ## #   diameter <dbl>, curb <chr>, date_planted <date>, longitude <dbl>,
    ## #   latitude <dbl>

``` r
average_by_group(vancouver_trees, genus_name, diameter)
```

    ## # A tibble: 97 × 2
    ##    genus_name  average_variable
    ##    <chr>                  <dbl>
    ##  1 ABIES                  12.9 
    ##  2 ACER                   10.6 
    ##  3 AESCULUS               23.7 
    ##  4 AILANTHUS              15.9 
    ##  5 ALBIZIA                 6   
    ##  6 ALNUS                  17.5 
    ##  7 AMELANCHIER             3.21
    ##  8 ARALIA                  6.81
    ##  9 ARAUCARIA              11.4 
    ## 10 ARBUTUS                18.4 
    ## # ℹ 87 more rows

Example 2: A different dataset

``` r
#For my second example, I will use the penguins dataset, so show the generality of my function
average_by_group(penguins, island, bill_length_mm)
```

    ## # A tibble: 3 × 2
    ##   island    average_variable
    ##   <fct>                <dbl>
    ## 1 Biscoe                NA  
    ## 2 Dream                 44.2
    ## 3 Torgersen             NA

``` r
# Note that here the NAs show up because i set the default value to FALSE< so the NAs are not excluded.
#But I could also change the setting so that NAs are set to true, and I can pass additional values. Here, I will try to find the average bill_length for species that have no NA values, and I set the trim value to 0.1.

average_by_group(penguins, species, bill_length_mm, na.rm = TRUE, trim=0.1)
```

    ## # A tibble: 3 × 2
    ##   species   average_variable
    ##   <fct>                <dbl>
    ## 1 Adelie                38.8
    ## 2 Chinstrap             48.9
    ## 3 Gentoo                47.4

Example 3: To show an error incase the user inputs a numerical
categorical variable that is not actually numeric

``` r
average_by_group(penguins, species, island)
```

    ## Error in average_by_group(penguins, species, island): The group variable must not be numeric.

Testing the function

``` r
#Lets test that inputing a non numeric variable gives me an error
expect_error(average_by_group(vancouver_trees, genus_name, std_street))

# Additional Tests
test_that("Tests for average_by_group function", {
  
  # Test 1: Vector with no NAs
  df1 <- tibble(group = c("A", "A", "B", "B"), value = c(10, 20, 30, 40))
  expect_equal(average_by_group(df1, group, value), tibble(group = c("A", "B"), average_variable = c(15, 35)))
  
  # Test 2: Vector that has NAs
  df2 <- tibble(group = c("A", "A", "B", "B"), value = c(10, NA, 30, 40))
  expect_no_error(average_by_group(df2, group, value))

  # Test 3: Vector of a different type (using a character vector for numeric_category which should cause an error)
  df3 <- tibble(group = c(1, 2, 3, 4), value = c("A", "B", "C", "D"))
  expect_error(average_by_group(df3, group, value))
  
  # Test 4: Vector of length 0
  df4 <- tibble(group = character(0), value = numeric(0))
  expect_equal(average_by_group(df4, group, value), tibble(group = character(0), average_variable = numeric(0)))

})
```

    ## Test passed 🎉
