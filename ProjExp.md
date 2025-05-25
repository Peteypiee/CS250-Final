DOGE Project Exploration
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

``` r
library(purrr)
source("ReadFPDS.R")
```

# Exploration Part 1, getting DOGE data

## Getting the initial page information and number of contracts

Idea of data obtained from:
<https://www.reddit.com/r/dataisbeautiful/comments/1jl3own/doge_preferentially_cancelled_grants_and/>
<https://bsky.app/profile/airmovingdevice.bsky.social/post/3ll2ehugqik2n>

Data obtained from:
<https://api.doge.gov/docs#/Savings/get_ContractSavings>

``` r
response <- GET("https://api.doge.gov/savings/contracts?sort_by=savings&sort_order=desc&per_page=100&page=1")
data <- content(response, as = "text", encoding="UTF-8")
parsed_data <- fromJSON(data)
page_count <- parsed_data$meta$pages
```

Here, we find that we have 95 pages of data

## Collect all contracts from DOGE API

``` r
all_results <- list()
for (i in 1:page_count) {
  #break # !!!REMOVE TO RE-GET ALL PAGES FROM API!!!
  #Sys.sleep(0.25)
  link <- paste0("https://api.doge.gov/savings/contracts?sort_by=savings&sort_order=asc&per_page=100&page=", i)
  i_response <- GET(link)
  i_data <- fromJSON(content(i_response, as = "text", encoding="UTF-8"))
  all_results[i] <- i_data$result
}
```

## Combine DOGE data into one dataframe

``` r
# For some reason rbind itself concats subframes into one row of frames
# instead of putting columns and stacking rows, needs do.call() wrapping to fix
combined_data <- do.call(rbind, all_results)
```

## Remove rows with no link or basic fpds.gov link

``` r
clean_data <- combined_data %>%
  mutate(
    # If link is blank or basic, valid = N, else valid = Y
    valid = ifelse(fpds_link == "" | fpds_link == "https://fpds.gov", "N", "Y")
  ) 

# remove all non-Y values, then delete the newly added valid column
clean_data <- subset(clean_data, clean_data$valid == "Y") %>%
  select(!(valid))
```

## Write DOGE data to CSV file

``` r
#write.csv(clean_data, "contracts.csv")
```

# Exploration Part 2, obtaining URL info

``` r
contracts <- read_csv("contracts.csv")
```

    ## New names:
    ## Rows: 5983 Columns: 10
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (7): piid, agency, vendor, description, fpds_status, fpds_link, deleted_... dbl
    ## (3): ...1, value, savings
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...1`

``` r
contracts <- contracts[-c(1)] # Remove extra index column added by read_csv()
```

## How to pull parameters with parse_url()

``` r
# Encountered issues with strsplit, 
# ended up finding urltools param_get(), but couldnt get that to work either,
# simplest answer turned out to be httr parse_url(), but has weird syntax
contracts %>%
  mutate(
    contract_type = map_chr(fpds_link, ~ parse_url(.)$query$contractType),
    piid2 = map_chr(fpds_link, ~ parse_url(.)$query$PIID)
  )
```

    ## # A tibble: 5,983 × 11
    ##    piid      agency vendor  value description fpds_status fpds_link deleted_date
    ##    <chr>     <chr>  <chr>   <dbl> <chr>       <chr>       <chr>     <chr>       
    ##  1 75N96024… Depar… Power… 2.09e5 DEIA Train… TERMINATED  https://… 1/22/2025   
    ##  2 2033H624… Depar… THE W… 8.87e4 DEIA Train… TERMINATED  https://… 1/22/2025   
    ##  3 2033H624… Depar… Inroa… 8.26e4 DEIA Train… TERMINATED  https://… 1/22/2025   
    ##  4 2032H325… Depar… Hispa… 3.22e4 DEIA Train… TERMINATED  https://… 1/22/2025   
    ##  5 2032H325… Depar… Inroa… 1.77e4 DEIA Train… TERMINATED  https://… 1/22/2025   
    ##  6 75N92021… Depar… DELOI… 4.78e6 DEIA Train… TERMINATED  https://… 1/23/2025   
    ##  7 2032H824… Depar… MANAG… 9.5 e5 DEIA Train… TERMINATED  https://… 1/23/2025   
    ##  8 47QDCB23… Gener… Eleva… 1.52e4 DEIA Train… TERMINATED  https://… 1/23/2025   
    ##  9 68HERH24… Envir… Light… 1.80e5 DEIA Train… TERMINATED  https://… 1/24/2025   
    ## 10 75ACF121… Depar… MEH A… 1.27e6 PROVIDE AL… TERMINATED  https://… 1/27/2025   
    ## # ℹ 5,973 more rows
    ## # ℹ 3 more variables: savings <dbl>, contract_type <chr>, piid2 <chr>

## Use parse_url to separate all parts of the links

``` r
# Note: IDV means Indefinite Delivery Vehicle
contracts <- contracts %>%
  mutate(
    contract_type = map_chr(fpds_link, ~ parse_url(.)$query$contractType),
    piid2 = map_chr(fpds_link, ~ parse_url(.)$query$PIID),
    agencyID = map_chr(fpds_link, ~ parse_url(.)$query$agencyID),
    modNumber = map_chr(fpds_link, ~ parse_url(.)$query$modNumber),
    idvAgencyID = map_chr(fpds_link, ~ parse_url(.)$query$idvAgencyID),
    idvPIID = map_chr(fpds_link, ~ parse_url(.)$query$idvPIID)
  )
contracts
```

    ## # A tibble: 5,983 × 15
    ##    piid      agency vendor  value description fpds_status fpds_link deleted_date
    ##    <chr>     <chr>  <chr>   <dbl> <chr>       <chr>       <chr>     <chr>       
    ##  1 75N96024… Depar… Power… 2.09e5 DEIA Train… TERMINATED  https://… 1/22/2025   
    ##  2 2033H624… Depar… THE W… 8.87e4 DEIA Train… TERMINATED  https://… 1/22/2025   
    ##  3 2033H624… Depar… Inroa… 8.26e4 DEIA Train… TERMINATED  https://… 1/22/2025   
    ##  4 2032H325… Depar… Hispa… 3.22e4 DEIA Train… TERMINATED  https://… 1/22/2025   
    ##  5 2032H325… Depar… Inroa… 1.77e4 DEIA Train… TERMINATED  https://… 1/22/2025   
    ##  6 75N92021… Depar… DELOI… 4.78e6 DEIA Train… TERMINATED  https://… 1/23/2025   
    ##  7 2032H824… Depar… MANAG… 9.5 e5 DEIA Train… TERMINATED  https://… 1/23/2025   
    ##  8 47QDCB23… Gener… Eleva… 1.52e4 DEIA Train… TERMINATED  https://… 1/23/2025   
    ##  9 68HERH24… Envir… Light… 1.80e5 DEIA Train… TERMINATED  https://… 1/24/2025   
    ## 10 75ACF121… Depar… MEH A… 1.27e6 PROVIDE AL… TERMINATED  https://… 1/27/2025   
    ## # ℹ 5,973 more rows
    ## # ℹ 7 more variables: savings <dbl>, contract_type <chr>, piid2 <chr>,
    ## #   agencyID <chr>, modNumber <chr>, idvAgencyID <chr>, idvPIID <chr>

## Define ‘key values’ of links as PIID and idvPIID

``` r
contracts %>%
  group_by(fpds_link) %>%
  count() %>%
  arrange(desc(n))%>%
  ungroup() %>%
  count() %>% 
  rename(fpds_link = n)
```

    ## # A tibble: 1 × 1
    ##   fpds_link
    ##       <int>
    ## 1      5982

``` r
contracts %>%
  group_by(piid) %>%
  count() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  count() %>% 
  rename(piid = n)
```

    ## # A tibble: 1 × 1
    ##    piid
    ##   <int>
    ## 1  5919

``` r
contracts %>%
  group_by(idvPIID) %>%
  count() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  count() %>% 
  rename(idvPIID = n)
```

    ## # A tibble: 1 × 1
    ##   idvPIID
    ##     <int>
    ## 1    2065

``` r
contracts %>%
  group_by(piid, idvPIID) %>%
  count() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  count() %>% 
  rename("piid, idvPIID" = n)
```

    ## # A tibble: 1 × 1
    ##   `piid, idvPIID`
    ##             <int>
    ## 1            5982

``` r
contracts %>%
  group_by(fpds_link, piid, idvPIID) %>%
  count() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  count() %>% 
  rename("fpds_link, piid, idvPIID" = n)
```

    ## # A tibble: 1 × 1
    ##   `fpds_link, piid, idvPIID`
    ##                        <int>
    ## 1                       5982

## Contract Type Analysis

``` r
contracts %>%
  group_by(contract_type) %>%
  count()
```

    ## # A tibble: 2 × 2
    ## # Groups:   contract_type [2]
    ##   contract_type     n
    ##   <chr>         <int>
    ## 1 AWARD          5805
    ## 2 IDV             178

## Look at idvAgencyID

Most rows have no value, but 19 awards do? I’m not using this further,
but found it interesting

``` r
contracts %>% 
  filter(idvAgencyID != "") %>%
  group_by(idvAgencyID) %>%
  count()
```

    ## # A tibble: 1 × 2
    ## # Groups:   idvAgencyID [1]
    ##   idvAgencyID     n
    ##   <chr>       <int>
    ## 1 2050           19

``` r
contracts %>%
  group_by(idvAgencyID, contract_type) %>%
  count()
```

    ## # A tibble: 3 × 3
    ## # Groups:   idvAgencyID, contract_type [3]
    ##   idvAgencyID contract_type     n
    ##   <chr>       <chr>         <int>
    ## 1 ""          AWARD          5786
    ## 2 ""          IDV             178
    ## 3 "2050"      AWARD            19

## Multiple duplicate links

Found 1 rows with the same link, meaning they have the same PIID,
idvPIID, modNumber, etc.

``` r
contracts %>%
  group_by(fpds_link) %>%
  count() %>%
  arrange(desc(n)) %>%
  filter(n > 1)
```

    ## # A tibble: 1 × 2
    ## # Groups:   fpds_link [1]
    ##   fpds_link                                                                    n
    ##   <chr>                                                                    <int>
    ## 1 https://www.fpds.gov/ezsearch/jsp/viewLinkController.jsp?agencyID=1406&…     2

``` r
contracts %>%
  group_by(piid, idvPIID) %>%
  count() %>%
  arrange(desc(n)) %>%
  filter(n > 1)
```

    ## # A tibble: 1 × 3
    ## # Groups:   piid, idvPIID [1]
    ##   piid          idvPIID        n
    ##   <chr>         <chr>      <int>
    ## 1 140D0422F0609 NNG15SC97B     2

# Exploration Part 3, obtaining FPDS data

### Basic fpds HTML reading

Here, I am learning how to interact with the FPDS links, and figuring
out nodes and ids for the form

``` r
reasons <- ""
for (link in contracts$fpds_link){
  #reasons <- paste0(reasons, scrape_fpds(link))
  test <- read_page(link)
  break
}

page_data <- read_html(link)
  
reasonForModification <- page_data %>%
  html_node("#reasonForModification") %>%
  html_attr("value")

reasonForModification
```

    ## [1] "Terminate for Convenience (complete or partial)"

## This was a major process, first deciding which columns I wanted:

Most of these are numeric, with reason_for_modification as an outlier I
also conisdered date and location variables, but only selected numeric
values.

``` r
ncol(blank_fpds())
```

    ## [1] 14

``` r
names(blank_fpds())
```

    ##  [1] "piid3"                                    
    ##  [2] "idv_piid"                                 
    ##  [3] "reason_for_modification"                  
    ##  [4] "obligated_amount"                         
    ##  [5] "total_obligated_amount"                   
    ##  [6] "previous_base_and_exercised_options_value"
    ##  [7] "base_and_exercised_options_value"         
    ##  [8] "total_base_and_exercised_options_value"   
    ##  [9] "previous_ultimate_contract_value"         
    ## [10] "ultimate_contract_value"                  
    ## [11] "base_and_all_options_value"               
    ## [12] "total_ultimate_contract_value"            
    ## [13] "fees_paid_for_use_of_service"             
    ## [14] "total_estimated_order_value"

## Running the scraper

First attempt failed: Was missing error detection, scraped ~500 links
and script crashed

Second attempt failed: Added error detection, scraped ~3000 links and
posit.cloud crashed

Third attempt worked: Changed append method to bind_rows() on a
dataframe Added “Catching Up” mechanism at beginning to not overwrite
data

``` r
base_page <- blank_fpds()
#results <- base_page !!!UNCOMMENT TO RUN AGAIN!!!

#max_i = nrow(results) !!!UNCOMMENT TO RUN AGAIN!!!
i <- 1
for (link in contracts$fpds_link){
  break             # !!!COMMENT OUT TO RUN AGAIN!!!
  if (i <= max_i && nrow(results) != 1) {
    if (i %% 10 == 0){ #Check in every 10, comment out to check in each time
      print(paste0("Catching up, ", i, "/", max_i))
    }
  }
  else {
    result <- tryCatch({
      fpds <- read_page(link)
    }, error = function(e) {
      result <- tryCatch({
        id <- parse_url(link)$query$PIID
        print(paste0("ERROR OCCURED: ROW: ", i, "; PIID: ", id, "; ERROR MESSAGE: ", e, ". TRYING AGAIN IN 5 SECONDS.")) 
        
        Sys.sleep(5) #Wait 5 seconds to give the site a chance to catch up
        fpds <- read_page(link) #Try again
      }, error = function(err) {
        #Was frequently getting XMLread errors 100-400 rows in, just wanted to run
        id <- parse_url(link)$query$PIID
        idv_id <- parse_url(link)$query$idvPIID
        print(paste0("KILLER ERROR OCCURED: ROW: ", i, "; PIID: ", id, "; IDV_ID: ", idv_id, "; ERROR MESSAGE: ", err)) 
        
        fpds <- blank_fpds() #Blank data for rows that fail twice
        fpds["piid"] <- id
        fpds["idv_piid"] <- idv_id
      })
    })
    
    results <- bind_rows(results, fpds)
    Sys.sleep(0.1)
    #if (i %% 10 == 0){ #Check in every 10, comment out to check in each time
    print(paste0(i, ": ", floor((i / nrow(contracts)) * 1000) / 10, "%;  ", fpds$piid))
    #}
    if (i == 1){
      #Convert to a dataframe (removing first empty row)
      results <- as.data.frame(results)[-1,] 
    }
  }
  
  i = i + 1
}
```

## Write FPDS data to a csv file

``` r
#write.csv(fpds_contracts, "fpds.csv")
```
