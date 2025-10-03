install.packages("DT")
# INTRODUCTION

This project explores the most popular programming on Netflix. It focuses on three main aspects:
  
  1. **Data Cleaning and Visualization** — inspecting and preparing the raw Netflix Top 10 datasets.
2. **Exploring Key Questions** — answering research-style questions with R and dplyr.
3. **Press Releases** — presenting three short reports inspired by the data:
  - *Stranger Things*: Upcoming Season and Global Popularity
- *India*: A Rising Streaming Powerhouse
- *Money Heist*: Global Success Across Markets

```{r}
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(lubridate)
library(DT)
library(ggplot2)
library(scales)
library(forcats)

# Helper to clean column names
format_titles <- function(df){
  colnames(df) <- colnames(df) |>
    str_replace_all("_", " ") |>
    str_to_title()
  df
}

if(!dir.exists(file.path("data", "mp01"))){
  dir.create(file.path("data", "mp01"), showWarnings=FALSE, recursive=TRUE)
}


GLOBAL_TOP_10_FILENAME <- file.path("data", "mp01", "global_top10_alltime.csv")


if(!file.exists(GLOBAL_TOP_10_FILENAME)){
  download.file("https://www.netflix.com/tudum/top10/data/all-weeks-global.tsv",
                destfile=GLOBAL_TOP_10_FILENAME)
}


COUNTRY_TOP_10_FILENAME <- file.path("data", "mp01", "country_top10_alltime.csv")


if(!file.exists(COUNTRY_TOP_10_FILENAME)){
  download.file("https://www.netflix.com/tudum/top10/data/all-weeks-countries.tsv",
                destfile=COUNTRY_TOP_10_FILENAME)
}
```
---
  ```{r}
#READ DATA
GLOBAL_TOP_10 <- read_tsv(GLOBAL_TOP_10_FILENAME)
GLOBAL_TOP_10 <- GLOBAL_TOP_10 |>
  mutate(season_title = if_else(season_title=="N/A",NA_character_,season_title))

COUNTRY_TOP_10 <- read_tsv(COUNTRY_TOP_10_FILENAME)
COUNTRY_TOP_10 <- COUNTRY_TOP_10 |>
  mutate(season_title = if_else(season_title=="N/A",NA_character_,season_title))
```
---
  ## 
  
  ```{r}
# Clean Titles and Convert hours to minutes and right numbers Global Top 10
GLOBAL_TOP_10 |> 
  mutate('runtime_(minutes)' = round(60 * runtime)) |>
  select(-season_title, 
         -runtime) |>
  format_titles() |>
  head(n=20) |>
  datatable(options=list(searching=FALSE, info=FALSE)) |>
  formatRound(c('Weekly Hours Viewed', 'Weekly Views'))
```
## Visualization of Global Top 10 
```{r}
GLOBAL_TOP_10 |> 
  head(n=20) |>
  datatable(options=list(scrollY = "400px",paging = TRUE,searching=FALSE, info=FALSE))
library(stringr)
format_titles <- function(df){
  colnames(df) <- str_replace_all(colnames(df), "_", " ") |> str_to_title()
  df
}
```

## Visualization of Country Top 10 and Clean Titles
```{r}
format_titles_country <- function(df) {
  colnames(df) <- str_replace_all(colnames(df), "_", " ") |> str_to_title()
  df
}
COUNTRY_TOP_10 <- format_titles_country(COUNTRY_TOP_10)
COUNTRY_TOP_10 |>
  head(20) |>
  datatable(options = list(searching = FALSE, info = FALSE))
```
