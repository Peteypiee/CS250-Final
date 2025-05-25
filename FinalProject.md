Evaulation of DOGE Data
================
Peter Ashley
5/8/25

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(dplyr)
library(httr)
library(jsonlite)
```

    ## 
    ## Attaching package: 'jsonlite'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     flatten

# Question: How did DOGE create their “value” property in their API?

# Setting up our data

## Read FPDS Data

Takes over from where I left off at the end of ProjExp.Rmd

``` r
fpds <- read_csv("fpds.csv")
```

    ## New names:
    ## Rows: 4793 Columns: 30
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (23): piid, agency, vendor, description, fpds_status, fpds_link, deleted... dbl
    ## (4): ...1, value, savings, idvAgencyID lgl (3):
    ## previous_base_and_exercised_options_value, previous_ultimate_contr...
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...1`

``` r
fpds <- fpds[-c(1)]
```

## Get AWARD-specific data, ignoring IDV values

``` r
awards <- fpds %>%
  filter(contract_type == "AWARD")
```

## Convert numeric data to numeric type

``` r
awards <- awards %>%
  mutate( #Removes dollar signs and commas from money rows, with as.numeric wrap
    obligated_amount = as.numeric(gsub('\\$|,', '', obligated_amount)),
    total_obligated_amount = as.numeric(gsub('\\$|,', '', total_obligated_amount)),
    base_and_exercised_options_value = as.numeric(gsub('\\$|,', '', base_and_exercised_options_value)),
    total_base_and_exercised_options_value = as.numeric(gsub('\\$|,', '', total_base_and_exercised_options_value)),
    ultimate_contract_value = as.numeric(gsub('\\$|,', '', ultimate_contract_value)),
    total_ultimate_contract_value = as.numeric(gsub('\\$|,', '', total_ultimate_contract_value)),
    fees_paid_for_use_of_service = as.numeric(gsub('\\$|,', '', fees_paid_for_use_of_service))
  ) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))
```

Sourced from
<https://stackoverflow.com/questions/8161836/how-do-i-replace-na-values-with-zeros-in-an-r-dataframe#>:<sub>:text=mutate_if(is.numeric%2C%20</sub>replace(.%2C%20is.na(.)%2C%200))

## Select necessary columns, rename them for shortening

``` r
awards_num <- awards %>%
  select(piid, idv_piid, value, savings, obligated_amount, total_obligated_amount, base_and_exercised_options_value, total_base_and_exercised_options_value, ultimate_contract_value, total_ultimate_contract_value, fees_paid_for_use_of_service) %>%
  rename(obligated = obligated_amount, total_obligated = total_obligated_amount, options = base_and_exercised_options_value, total_options = total_base_and_exercised_options_value, ucv = ultimate_contract_value, total_ucv = total_ultimate_contract_value, fees = fees_paid_for_use_of_service)
```

## Create Calculated Columns that are repeatedly used for future calculations

``` r
awards_num <- awards_num %>%
  mutate(
    obl_totalobl = (obligated + total_obligated),
    obl_totalobl_abs = (abs(obligated) + abs(total_obligated)),
    opt_totalopt = (options + total_options),
    opt_totalopt_abs = (abs(options) + abs(total_options)),
    ucv_totalucv = (ucv + total_ucv),
    ucv_totalucv_abs = (abs(ucv) + abs(total_ucv))
  )
awards_num
```

    ## # A tibble: 4,618 × 17
    ##    piid  idv_piid  value savings obligated total_obligated options total_options
    ##    <chr> <chr>     <dbl>   <dbl>     <dbl>           <dbl>   <dbl>         <dbl>
    ##  1 75N9… GS02Q16… 2.09e5       0        0          209395.      0        209395.
    ##  2 2033… 2032H32… 8.87e4       0        0           88734.      0         88734.
    ##  3 2033… 2032H32… 8.26e4       0        0           45424.      0         45424.
    ##  4 2032… 2032H32… 3.22e4       0   -30472.           1728. -30472.         1728.
    ##  5 2032… 2032H32… 1.77e4       0   -12120.           5616. -12120.         5616.
    ##  6 75N9… 47QRAA1… 4.78e6       0        0         4530811.      0       4530811.
    ##  7 2032… 2032H82… 9.5 e5       0        0           79557       0         79557 
    ##  8 47QD… GS00F22… 1.52e4       0     7584           15168    7584         15168 
    ##  9 68HE… <NA>     1.80e5       0        0          180000       0        180000 
    ## 10 75AC… 47QRAA1… 1.27e6       0        0         1267114.      0       1267114.
    ## # ℹ 4,608 more rows
    ## # ℹ 9 more variables: ucv <dbl>, total_ucv <dbl>, fees <dbl>,
    ## #   obl_totalobl <dbl>, obl_totalobl_abs <dbl>, opt_totalopt <dbl>,
    ## #   opt_totalopt_abs <dbl>, ucv_totalucv <dbl>, ucv_totalucv_abs <dbl>

# Initial Check-Values

### Important Variables and Concepts:

#### checks: the dataframe, named because it holds a large number of check-values

#### value: what we are solving for, from the doge api

#### minval: the lowest approximation of a row’s value based on other variables

#### obligated: one of the major contributors to value, fpds

#### options: one of the major contributors to value, fpds

#### ultimate_contract_value (ucv): one of the major contributors to value, fpds

#### fees: another number that contributes to value (included later), fpds

#### savings: another number from doge that contributes to value

#### total: prefix for obligated, options, and ucv columns

#### previous: prefix for obligated, options, and ucv columns (unused, no values)

``` r
checks <- awards_num %>%
  mutate(
    a = abs(value - total_obligated),
    a1 = abs(value - total_obligated - savings),
    a2 = abs(value - total_obligated + savings),
    b = abs(value - obl_totalobl),
    b1 = abs(value - obl_totalobl - savings),
    b2 = abs(value - obl_totalobl + savings),
    c = abs(value - obl_totalobl_abs),
    c1 = abs(value - obl_totalobl_abs - savings),
    c2 = abs(value - obl_totalobl_abs + savings),
    d = abs(value - total_options),
    d1 = abs(value - total_options - savings),
    d2 = abs(value - total_options + savings),
    e = abs(value - opt_totalopt),
    e1 = abs(value - opt_totalopt - savings),
    e2 = abs(value - opt_totalopt + savings),
    f = abs(value - opt_totalopt_abs),
    f1 = abs(value - opt_totalopt_abs - savings),
    f2 = abs(value - opt_totalopt_abs + savings),
    g = abs(value - total_ucv),
    g1 = abs(value - total_ucv - savings),
    g2 = abs(value - total_ucv + savings),
    h = abs(value - ucv_totalucv),
    h1 = abs(value - ucv_totalucv - savings),
    h2 = abs(value - ucv_totalucv + savings),
    i = abs(value - ucv_totalucv_abs),
    i1 = abs(value - ucv_totalucv_abs - savings),
    i2 = abs(value - ucv_totalucv_abs + savings)
  )
checks
```

    ## # A tibble: 4,618 × 44
    ##    piid  idv_piid  value savings obligated total_obligated options total_options
    ##    <chr> <chr>     <dbl>   <dbl>     <dbl>           <dbl>   <dbl>         <dbl>
    ##  1 75N9… GS02Q16… 2.09e5       0        0          209395.      0        209395.
    ##  2 2033… 2032H32… 8.87e4       0        0           88734.      0         88734.
    ##  3 2033… 2032H32… 8.26e4       0        0           45424.      0         45424.
    ##  4 2032… 2032H32… 3.22e4       0   -30472.           1728. -30472.         1728.
    ##  5 2032… 2032H32… 1.77e4       0   -12120.           5616. -12120.         5616.
    ##  6 75N9… 47QRAA1… 4.78e6       0        0         4530811.      0       4530811.
    ##  7 2032… 2032H82… 9.5 e5       0        0           79557       0         79557 
    ##  8 47QD… GS00F22… 1.52e4       0     7584           15168    7584         15168 
    ##  9 68HE… <NA>     1.80e5       0        0          180000       0        180000 
    ## 10 75AC… 47QRAA1… 1.27e6       0        0         1267114.      0       1267114.
    ## # ℹ 4,608 more rows
    ## # ℹ 36 more variables: ucv <dbl>, total_ucv <dbl>, fees <dbl>,
    ## #   obl_totalobl <dbl>, obl_totalobl_abs <dbl>, opt_totalopt <dbl>,
    ## #   opt_totalopt_abs <dbl>, ucv_totalucv <dbl>, ucv_totalucv_abs <dbl>,
    ## #   a <dbl>, a1 <dbl>, a2 <dbl>, b <dbl>, b1 <dbl>, b2 <dbl>, c <dbl>,
    ## #   c1 <dbl>, c2 <dbl>, d <dbl>, d1 <dbl>, d2 <dbl>, e <dbl>, e1 <dbl>,
    ## #   e2 <dbl>, f <dbl>, f1 <dbl>, f2 <dbl>, g <dbl>, g1 <dbl>, g2 <dbl>, …

## Takes all rows and gets the Minimum Value from all columns

``` r
checks <- checks %>%
  rowwise() %>% # dplyr rowwise() used here to take min() on row instead of col
  mutate(
    minval = min(c_across(a:i2))
  ) %>%
  ungroup() # ungroup the rowwise() to prevent any accidental errors
checks
```

    ## # A tibble: 4,618 × 45
    ##    piid  idv_piid  value savings obligated total_obligated options total_options
    ##    <chr> <chr>     <dbl>   <dbl>     <dbl>           <dbl>   <dbl>         <dbl>
    ##  1 75N9… GS02Q16… 2.09e5       0        0          209395.      0        209395.
    ##  2 2033… 2032H32… 8.87e4       0        0           88734.      0         88734.
    ##  3 2033… 2032H32… 8.26e4       0        0           45424.      0         45424.
    ##  4 2032… 2032H32… 3.22e4       0   -30472.           1728. -30472.         1728.
    ##  5 2032… 2032H32… 1.77e4       0   -12120.           5616. -12120.         5616.
    ##  6 75N9… 47QRAA1… 4.78e6       0        0         4530811.      0       4530811.
    ##  7 2032… 2032H82… 9.5 e5       0        0           79557       0         79557 
    ##  8 47QD… GS00F22… 1.52e4       0     7584           15168    7584         15168 
    ##  9 68HE… <NA>     1.80e5       0        0          180000       0        180000 
    ## 10 75AC… 47QRAA1… 1.27e6       0        0         1267114.      0       1267114.
    ## # ℹ 4,608 more rows
    ## # ℹ 37 more variables: ucv <dbl>, total_ucv <dbl>, fees <dbl>,
    ## #   obl_totalobl <dbl>, obl_totalobl_abs <dbl>, opt_totalopt <dbl>,
    ## #   opt_totalopt_abs <dbl>, ucv_totalucv <dbl>, ucv_totalucv_abs <dbl>,
    ## #   a <dbl>, a1 <dbl>, a2 <dbl>, b <dbl>, b1 <dbl>, b2 <dbl>, c <dbl>,
    ## #   c1 <dbl>, c2 <dbl>, d <dbl>, d1 <dbl>, d2 <dbl>, e <dbl>, e1 <dbl>,
    ## #   e2 <dbl>, f <dbl>, f1 <dbl>, f2 <dbl>, g <dbl>, g1 <dbl>, g2 <dbl>, …

## Checks which column the Minimum Value is first seen in

``` r
checks <- checks %>%
  mutate(
    mincol = # Huge if statement but it works
      if_else(a == minval, "a",
      if_else(a1 == minval, "a1",
      if_else(a2 == minval, "a2",
      if_else(b == minval, "b",
      if_else(b1 == minval, "b1",
      if_else(b2 == minval, "b2",
      if_else(c == minval, "c",
      if_else(c1 == minval, "c1",
      if_else(c2 == minval, "c2",
      if_else(d == minval, "d",
      if_else(d1 == minval, "d1",
      if_else(d2 == minval, "d2",
      if_else(e == minval, "e",
      if_else(e1 == minval, "e1",
      if_else(e2 == minval, "e2",
      if_else(f == minval, "f",
      if_else(f1 == minval, "f1",
      if_else(f2 == minval, "f2",
      if_else(g == minval, "g",
      if_else(g1 == minval, "g1",
      if_else(g2 == minval, "g2",
      if_else(h == minval, "h",
      if_else(h1 == minval, "h1",
      if_else(h2 == minval, "h2",
      if_else(i == minval, "i",
      if_else(i1 == minval, "i1",
      if_else(i2 == minval, "i2",
      "ERROR"
      ))))))))))))))))))))))))))
    )
  )
```

## Looks for rows which were affected by PlusSavings functions

``` r
checks %>%
  filter(savings > 0) %>%
  select(piid,a2,b2,c2,d2,e2,f2,g2,h2,i2,mincol) %>%
  rowwise() %>%
  mutate(
    minval = min(a2,b2,c2,d2,e2,f2,g2,h2,i2)
  ) %>%
  ungroup() %>%
  filter(minval < 1)
```

    ## # A tibble: 30 × 12
    ##    piid       a2      b2      c2      d2      e2      f2      g2      h2      i2
    ##    <chr>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 86614…  3665  3.66e+3 3.66e+3 3.66e+3 3.66e+3 3.66e+3 3.66e+3 7.33e+3 0      
    ##  2 6913G… 30000  4.50e+4 1.5 e+4 3   e+4 4.50e+4 1.5 e+4 1.5 e+4 3   e+4 0      
    ##  3 20343… 31766. 3.18e+4 3.18e+4 1.59e+4 2.00e-1 2.00e-1 1.59e+4 1.59e+4 1.59e+4
    ##  4 49100… 33520. 3.00e-1 3.00e-1 3.00e-1 3.35e+4 3.35e+4 3.00e-1 3.35e+4 3.35e+4
    ##  5 20346… 38282. 0       0       3.83e+4 0       0       3.83e+4 3.83e+4 3.83e+4
    ##  6 HQ003… 40000  0       0       4   e+4 0       0       4   e+4 4   e+4 4   e+4
    ##  7 34300… 84000  1.26e+5 4.20e+4 4.20e+4 8.40e+4 0       4.20e+4 8.40e+4 0      
    ##  8 89303… 89240  4.46e+4 4.46e+4 4.46e+4 0       0       4.46e+4 0       0      
    ##  9 89303… 46470. 8.00e-2 8.00e-2 4.65e+4 8.00e-2 8.00e-2 4.65e+4 8.00e-2 8.00e-2
    ## 10 89303… 55602. 5.52e+4 5.52e+4 5.56e+4 5.52e+4 5.52e+4 2.70e-1 4.20e+2 4.20e+2
    ## # ℹ 20 more rows
    ## # ℹ 2 more variables: mincol <chr>, minval <dbl>

## Looks exclusively for rows with a PlusSavings column for a mincol

This indicates no other columns likely impacted it, can go back and
check for each individual row

``` r
checks %>%
  filter(savings > 0) %>%
  select(piid,a2,b2,c2,d2,e2,f2,g2,h2,i2,mincol) %>%
  mutate(
    minval = min(a2,b2,c2,d2,e2,f2,g2,h2,i2)
  ) %>%
  filter(minval < 1) %>%
  filter(mincol %in% c("a2","b2","c2","d2","e2","f2","g2","h2","i2"))
```

    ## # A tibble: 57 × 12
    ##    piid        a2     b2     c2     d2     e2     f2     g2     h2     i2 mincol
    ##    <chr>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <chr> 
    ##  1 2032H5… 2.77e5 2.77e5 2.77e5 2.77e5 2.77e5 2.77e5 3.32e6 3.32e6 3.32e6 a2    
    ##  2 75D301… 6.94e2 6.94e2 6.94e2 6.94e2 6.94e2 6.94e2 6.94e2 6.94e2 6.94e2 a2    
    ##  3 343000… 3.56e3 3.56e3 3.56e3 3.56e3 3.56e3 3.56e3 3.56e3 3.56e3 3.56e3 b2    
    ##  4 75D301… 5.67e4 5.67e4 5.67e4 5.67e4 5.67e4 5.67e4 5.67e4 5.67e4 5.67e4 a2    
    ##  5 36C776… 7.08e4 4.55e4 9.61e4 7.08e4 4.55e4 9.61e4 7.08e4 4.55e4 9.61e4 b2    
    ##  6 75D301… 7.79e3 7.79e3 7.79e3 7.79e3 7.79e3 7.79e3 7.79e3 7.79e3 7.79e3 a2    
    ##  7 75D301… 2.42e4 2.42e4 2.42e4 2.42e4 2.42e4 2.42e4 8.82e4 8.82e4 8.82e4 a2    
    ##  8 N00189… 7.07e5 1.44e6 1.44e6 7.07e5 1.44e6 1.44e6 7.07e5 7.07e5 7.07e5 a2    
    ##  9 75F401… 3.60e5 3.60e5 3.60e5 3.60e5 3.60e5 3.60e5 8.15e5 8.15e5 8.15e5 a2    
    ## 10 203464… 3.83e4 0      0      3.83e4 0      0      3.83e4 3.83e4 3.83e4 b2    
    ## # ℹ 47 more rows
    ## # ℹ 1 more variable: minval <dbl>

## 503 values have no solved formula (minval \< 1), can we do better?

``` r
checks %>%
  filter(abs(minval) > 1) %>%
  count()
```

    ## # A tibble: 1 × 1
    ##       n
    ##   <int>
    ## 1   503

# An In-Depth expansive check

## Generate a string of all letter/number combos a-l, 1-9

``` r
s <- ""
for (letter in c("a","b","c","d","e","f","g","h","i","k","l")){
  for (i in 1:9){
    s <- paste0(s, letter, i, ",")
  }
}
s
```

    ## [1] "a1,a2,a3,a4,a5,a6,a7,a8,a9,b1,b2,b3,b4,b5,b6,b7,b8,b9,c1,c2,c3,c4,c5,c6,c7,c8,c9,d1,d2,d3,d4,d5,d6,d7,d8,d9,e1,e2,e3,e4,e5,e6,e7,e8,e9,f1,f2,f3,f4,f5,f6,f7,f8,f9,g1,g2,g3,g4,g5,g6,g7,g8,g9,h1,h2,h3,h4,h5,h6,h7,h8,h9,i1,i2,i3,i4,i5,i6,i7,i8,i9,k1,k2,k3,k4,k5,k6,k7,k8,k9,l1,l2,l3,l4,l5,l6,l7,l8,l9,"

## Expands from a-i to a-l, and from 3 to 9 formulas, incorporating fees

``` r
checks <- awards_num %>%
  mutate(
    a1 = abs(value - total_obligated),
    a2 = abs(value - total_obligated - fees),
    a3 = abs(value - total_obligated + fees),
    a4 = abs(value - total_obligated - savings),
    a5 = abs(value - total_obligated + savings),
    a6 = abs(value - total_obligated - savings + fees),
    a7 = abs(value - total_obligated - savings - fees),
    a8 = abs(value - total_obligated + savings + fees),
    a9 = abs(value - total_obligated + savings - fees),
    b1 = abs(value - obl_totalobl),
    b2 = abs(value - obl_totalobl - fees),
    b3 = abs(value - obl_totalobl + fees),
    b4 = abs(value - obl_totalobl - savings),
    b5 = abs(value - obl_totalobl + savings),
    b6 = abs(value - obl_totalobl - savings + fees),
    b7 = abs(value - obl_totalobl - savings - fees),
    b8 = abs(value - obl_totalobl + savings + fees),
    b9 = abs(value - obl_totalobl + savings - fees),
    c1 = abs(value - obl_totalobl_abs),
    c2 = abs(value - obl_totalobl_abs - fees),
    c3 = abs(value - obl_totalobl_abs + fees),
    c4 = abs(value - obl_totalobl_abs - savings),
    c5 = abs(value - obl_totalobl_abs + savings),
    c6 = abs(value - obl_totalobl_abs - savings + fees),
    c7 = abs(value - obl_totalobl_abs - savings - fees),
    c8 = abs(value - obl_totalobl_abs + savings + fees),
    c9 = abs(value - obl_totalobl_abs + savings - fees),
    d1 = abs(value - total_options),
    d2 = abs(value - total_options - fees),
    d3 = abs(value - total_options + fees),
    d4 = abs(value - total_options - savings),
    d5 = abs(value - total_options + savings),
    d6 = abs(value - total_options - savings + fees),
    d7 = abs(value - total_options - savings - fees),
    d8 = abs(value - total_options + savings + fees),
    d9 = abs(value - total_options + savings - fees),
    e1 = abs(value - opt_totalopt),
    e2 = abs(value - opt_totalopt - fees),
    e3 = abs(value - opt_totalopt + fees),
    e4 = abs(value - opt_totalopt - savings),
    e5 = abs(value - opt_totalopt + savings),
    e6 = abs(value - opt_totalopt - savings + fees),
    e7 = abs(value - opt_totalopt - savings - fees),
    e8 = abs(value - opt_totalopt + savings + fees),
    e9 = abs(value - opt_totalopt + savings - fees),
    f1 = abs(value - opt_totalopt_abs),
    f2 = abs(value - opt_totalopt_abs - fees),
    f3 = abs(value - opt_totalopt_abs + fees),
    f4 = abs(value - opt_totalopt_abs - savings),
    f5 = abs(value - opt_totalopt_abs + savings),
    f6 = abs(value - opt_totalopt_abs - savings + fees),
    f7 = abs(value - opt_totalopt_abs - savings - fees),
    f8 = abs(value - opt_totalopt_abs + savings + fees),
    f9 = abs(value - opt_totalopt_abs + savings - fees),
    g1 = abs(value - total_ucv),
    g2 = abs(value - total_ucv - fees),
    g3 = abs(value - total_ucv + fees),
    g4 = abs(value - total_ucv - savings),
    g5 = abs(value - total_ucv + savings),
    g6 = abs(value - total_ucv - savings + fees),
    g7 = abs(value - total_ucv - savings - fees),
    g8 = abs(value - total_ucv + savings + fees),
    g9 = abs(value - total_ucv + savings - fees),
    h1 = abs(value - ucv_totalucv),
    h2 = abs(value - ucv_totalucv - fees),
    h3 = abs(value - ucv_totalucv + fees),
    h4 = abs(value - ucv_totalucv - savings),
    h5 = abs(value - ucv_totalucv + savings),
    h6 = abs(value - ucv_totalucv - savings + fees),
    h7 = abs(value - ucv_totalucv - savings - fees),
    h8 = abs(value - ucv_totalucv + savings + fees),
    h9 = abs(value - ucv_totalucv + savings - fees),
    i1 = abs(value - ucv_totalucv_abs),
    i2 = abs(value - ucv_totalucv_abs - fees),
    i3 = abs(value - ucv_totalucv_abs + fees),
    i4 = abs(value - ucv_totalucv_abs - savings),
    i5 = abs(value - ucv_totalucv_abs + savings),
    i6 = abs(value - ucv_totalucv_abs - savings + fees),
    i7 = abs(value - ucv_totalucv_abs - savings - fees),
    i8 = abs(value - ucv_totalucv_abs + savings + fees),
    i9 = abs(value - ucv_totalucv_abs + savings - fees),
    j1 = abs(value - abs(obligated)),
    j2 = abs(value - abs(obligated) - fees),
    j3 = abs(value - abs(obligated) + fees),
    j4 = abs(value - abs(obligated) - savings),
    j5 = abs(value - abs(obligated) + savings),
    j6 = abs(value - abs(obligated) - savings + fees),
    j7 = abs(value - abs(obligated) - savings - fees),
    j8 = abs(value - abs(obligated) + savings + fees),
    j9 = abs(value - abs(obligated) + savings - fees),
    k1 = abs(value - abs(options)),
    k2 = abs(value - abs(options) - fees),
    k3 = abs(value - abs(options) + fees),
    k4 = abs(value - abs(options) - savings),
    k5 = abs(value - abs(options) + savings),
    k6 = abs(value - abs(options) - savings + fees),
    k7 = abs(value - abs(options) - savings - fees),
    k8 = abs(value - abs(options) + savings + fees),
    k9 = abs(value - abs(options) + savings - fees),
    l1 = abs(value - abs(ucv)),
    l2 = abs(value - abs(ucv) - fees),
    l3 = abs(value - abs(ucv) + fees),
    l4 = abs(value - abs(ucv) - savings),
    l5 = abs(value - abs(ucv) + savings),
    l6 = abs(value - abs(ucv) - savings + fees),
    l7 = abs(value - abs(ucv) - savings - fees),
    l8 = abs(value - abs(ucv) + savings + fees),
    l9 = abs(value - abs(ucv) + savings - fees)
  )
```

## Find minval

``` r
checks <- checks %>%
  rowwise() %>%
  mutate(
    minval = min(c_across(a1:l9))
  ) %>%
  ungroup()
checks
```

    ## # A tibble: 4,618 × 126
    ##    piid  idv_piid  value savings obligated total_obligated options total_options
    ##    <chr> <chr>     <dbl>   <dbl>     <dbl>           <dbl>   <dbl>         <dbl>
    ##  1 75N9… GS02Q16… 2.09e5       0        0          209395.      0        209395.
    ##  2 2033… 2032H32… 8.87e4       0        0           88734.      0         88734.
    ##  3 2033… 2032H32… 8.26e4       0        0           45424.      0         45424.
    ##  4 2032… 2032H32… 3.22e4       0   -30472.           1728. -30472.         1728.
    ##  5 2032… 2032H32… 1.77e4       0   -12120.           5616. -12120.         5616.
    ##  6 75N9… 47QRAA1… 4.78e6       0        0         4530811.      0       4530811.
    ##  7 2032… 2032H82… 9.5 e5       0        0           79557       0         79557 
    ##  8 47QD… GS00F22… 1.52e4       0     7584           15168    7584         15168 
    ##  9 68HE… <NA>     1.80e5       0        0          180000       0        180000 
    ## 10 75AC… 47QRAA1… 1.27e6       0        0         1267114.      0       1267114.
    ## # ℹ 4,608 more rows
    ## # ℹ 118 more variables: ucv <dbl>, total_ucv <dbl>, fees <dbl>,
    ## #   obl_totalobl <dbl>, obl_totalobl_abs <dbl>, opt_totalopt <dbl>,
    ## #   opt_totalopt_abs <dbl>, ucv_totalucv <dbl>, ucv_totalucv_abs <dbl>,
    ## #   a1 <dbl>, a2 <dbl>, a3 <dbl>, a4 <dbl>, a5 <dbl>, a6 <dbl>, a7 <dbl>,
    ## #   a8 <dbl>, a9 <dbl>, b1 <dbl>, b2 <dbl>, b3 <dbl>, b4 <dbl>, b5 <dbl>,
    ## #   b6 <dbl>, b7 <dbl>, b8 <dbl>, b9 <dbl>, c1 <dbl>, c2 <dbl>, c3 <dbl>, …

``` r
checks %>%
  filter(abs(minval) > 1) %>%
  count()
```

    ## # A tibble: 1 × 1
    ##       n
    ##   <int>
    ## 1   479

``` r
checks %>%
  filter(abs(minval) > 1) %>%
  ggplot(mapping = aes(minval)) +
  geom_histogram(binwidth = 1000000)
```

![](FinalProject_files/figure-gfm/rows-no-formula-2-1.png)<!-- -->

## Lots of high-skewed outliers, removed everything over 2.5m

This chart shows awards that I couldn’t find a formula for, with most
values closer to 0, but some ranging incredibly high

``` r
checks %>%
  filter(abs(minval) > 1) %>%
  filter(abs(minval) < 2500000) %>%
  count()
```

    ## # A tibble: 1 × 1
    ##       n
    ##   <int>
    ## 1   450

``` r
checks %>%
  filter(abs(minval) > 1) %>%
  filter(abs(minval) < 2500000) %>%
  ggplot(mapping = aes(minval)) +
  geom_histogram(binwidth=100000)
```

![](FinalProject_files/figure-gfm/high-value-histogram-1.png)<!-- -->

## Zooming in further, sub-100k

This chart is further zoomed in, but doesn’t really show much.

``` r
checks %>%
  filter(abs(minval) > 1) %>%
  filter(abs(minval) < 100000) %>%
  count()
```

    ## # A tibble: 1 × 1
    ##       n
    ##   <int>
    ## 1   232

``` r
checks %>%
  filter(abs(minval) > 1) %>%
  filter(abs(minval) < 100000) %>%
  ggplot(mapping = aes(minval)) +
  geom_histogram(binwidth=2500)
```

![](FinalProject_files/figure-gfm/low-value-histogram-1.png)<!-- -->

# Evaluating lowest check-values

## Convert checks from wide-form to long-form data

``` r
checks_long <- checks %>%
  pivot_longer(cols="a1":"l9",
               names_to="check_column",
               values_to="check_value")
checks_long %>%
  filter(check_value < 1)
```

    ## # A tibble: 164,631 × 20
    ##    piid  idv_piid  value savings obligated total_obligated options total_options
    ##    <chr> <chr>     <dbl>   <dbl>     <dbl>           <dbl>   <dbl>         <dbl>
    ##  1 75N9… GS02Q16… 209395       0         0         209395.       0       209395.
    ##  2 75N9… GS02Q16… 209395       0         0         209395.       0       209395.
    ##  3 75N9… GS02Q16… 209395       0         0         209395.       0       209395.
    ##  4 75N9… GS02Q16… 209395       0         0         209395.       0       209395.
    ##  5 75N9… GS02Q16… 209395       0         0         209395.       0       209395.
    ##  6 75N9… GS02Q16… 209395       0         0         209395.       0       209395.
    ##  7 75N9… GS02Q16… 209395       0         0         209395.       0       209395.
    ##  8 75N9… GS02Q16… 209395       0         0         209395.       0       209395.
    ##  9 75N9… GS02Q16… 209395       0         0         209395.       0       209395.
    ## 10 75N9… GS02Q16… 209395       0         0         209395.       0       209395.
    ## # ℹ 164,621 more rows
    ## # ℹ 12 more variables: ucv <dbl>, total_ucv <dbl>, fees <dbl>,
    ## #   obl_totalobl <dbl>, obl_totalobl_abs <dbl>, opt_totalopt <dbl>,
    ## #   opt_totalopt_abs <dbl>, ucv_totalucv <dbl>, ucv_totalucv_abs <dbl>,
    ## #   minval <dbl>, check_column <chr>, check_value <dbl>

# Most effective formula-variable combinations

``` r
checks_long %>%
  filter(check_value < 1) %>%
  group_by(check_column) %>%
  count() %>%
  rename(formula = check_column) %>%
  arrange(desc(n)) %>%
  head(9)
```

    ## # A tibble: 9 × 2
    ## # Groups:   formula [9]
    ##   formula     n
    ##   <chr>   <int>
    ## 1 i1       3471
    ## 2 i2       3443
    ## 3 i3       3443
    ## 4 g1       3063
    ## 5 g2       3041
    ## 6 g3       3040
    ## 7 h1       2955
    ## 8 h2       2929
    ## 9 h3       2929

\###Most effective combos: \####i1 = abs(value - ucv_totalucv_abs), +-
fees \####g1 = abs(value - total_ucv), +- fees \####h1 = abs(value -
ucv_totalucv), +- fees

This shows that the ucv variable is one of the more critical ones

## Separate variable and formula from check-column

``` r
checks_long <- checks_long %>%
  mutate(
    var = substr(check_column, 1, 1),
    formula = as.numeric(substr(check_column, 2, 2))
  )
checks_long
```

    ## # A tibble: 498,744 × 22
    ##    piid  idv_piid  value savings obligated total_obligated options total_options
    ##    <chr> <chr>     <dbl>   <dbl>     <dbl>           <dbl>   <dbl>         <dbl>
    ##  1 75N9… GS02Q16… 209395       0         0         209395.       0       209395.
    ##  2 75N9… GS02Q16… 209395       0         0         209395.       0       209395.
    ##  3 75N9… GS02Q16… 209395       0         0         209395.       0       209395.
    ##  4 75N9… GS02Q16… 209395       0         0         209395.       0       209395.
    ##  5 75N9… GS02Q16… 209395       0         0         209395.       0       209395.
    ##  6 75N9… GS02Q16… 209395       0         0         209395.       0       209395.
    ##  7 75N9… GS02Q16… 209395       0         0         209395.       0       209395.
    ##  8 75N9… GS02Q16… 209395       0         0         209395.       0       209395.
    ##  9 75N9… GS02Q16… 209395       0         0         209395.       0       209395.
    ## 10 75N9… GS02Q16… 209395       0         0         209395.       0       209395.
    ## # ℹ 498,734 more rows
    ## # ℹ 14 more variables: ucv <dbl>, total_ucv <dbl>, fees <dbl>,
    ## #   obl_totalobl <dbl>, obl_totalobl_abs <dbl>, opt_totalopt <dbl>,
    ## #   opt_totalopt_abs <dbl>, ucv_totalucv <dbl>, ucv_totalucv_abs <dbl>,
    ## #   minval <dbl>, check_column <chr>, check_value <dbl>, var <chr>,
    ## #   formula <dbl>

## Evaluating variables

``` r
var_eval <- checks_long %>%
  filter(check_value < 1) %>%
  group_by(var) %>%
  count() %>%
  arrange(desc(n))
var_eval
```

    ## # A tibble: 12 × 2
    ## # Groups:   var [12]
    ##    var       n
    ##    <chr> <int>
    ##  1 i     19359
    ##  2 g     18285
    ##  3 c     17912
    ##  4 f     17500
    ##  5 a     16995
    ##  6 h     16989
    ##  7 d     16625
    ##  8 b     16064
    ##  9 e     15694
    ## 10 l      3188
    ## 11 k      3013
    ## 12 j      3007

``` r
var_eval %>%
  ggplot(mapping=aes(x=var, y=n)) + 
    geom_point()
```

![](FinalProject_files/figure-gfm/effective-variables-1.png)<!-- -->

In evaluating variables, we find the following:

Most effective: i: ucv_totalucv_abs g: total_ucv c: obl_totalobl_abs

Least effective: l: ucv k: options j: obligated

From this, we see that calculated columns with absolute values tend to
do well, and the columns which have few non-blank values do poorly. This
is expectable.

## Evaluating formulae

``` r
formulae_eval <- checks_long %>%
  filter(check_value < 1) %>%
  group_by(formula) %>%
  count() %>%
  arrange(desc(n))
formulae_eval
```

    ## # A tibble: 9 × 2
    ## # Groups:   formula [9]
    ##   formula     n
    ##     <dbl> <int>
    ## 1       4 21197
    ## 2       6 21075
    ## 3       7 21072
    ## 4       1 20764
    ## 5       2 20645
    ## 6       3 20635
    ## 7       5 13105
    ## 8       9 13075
    ## 9       8 13063

``` r
formulae_eval %>%
  ggplot(mapping=aes(x=formula, y=n)) + 
    geom_point()
```

![](FinalProject_files/figure-gfm/effective-formulae-1.png)<!-- -->

In evaluating effectiveness of formulae, we find the following:

Most effective: 4: abs(value - var - savings) 6: abs(value - var -
savings + fees) 7: abs(value - var - savings - fees)

Least effective: 5 = abs(value - var + savings) 8 = abs(value - var +
savings + fees) 9 = abs(value - var + savings - fees)

This was more interesting, as it showed which formulae actually worked.
The fact that 13000 still pass surprises me though

## Evaluating formulae with savings

``` r
form_savings <- checks_long %>%
  filter(check_value < 1) %>%
  filter(savings > 0) %>%
  group_by(formula) %>%
  count() %>%
  arrange(desc(n))
form_savings
```

    ## # A tibble: 9 × 2
    ## # Groups:   formula [9]
    ##   formula     n
    ##     <dbl> <int>
    ## 1       4  8176
    ## 2       6  8096
    ## 3       7  8090
    ## 4       1  7743
    ## 5       2  7663
    ## 6       3  7656
    ## 7       9    93
    ## 8       5    84
    ## 9       8    84

``` r
form_savings %>%
  ggplot(mapping=aes(x=formula, y=n)) + 
    geom_point()
```

![](FinalProject_files/figure-gfm/effective-formulae-savings-1.png)<!-- -->

When only rows with a non-zero savings value are used, formulas 5, 8,
and 9 become obsolete
