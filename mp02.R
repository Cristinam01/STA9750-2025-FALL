if(!dir.exists(file.path("data", "mp02"))){
  dir.create(file.path("data", "mp02"), showWarnings=FALSE, recursive=TRUE)
}

ensure_package <- function(pkg){
  pkg <- as.character(substitute(pkg))
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  if(!require(pkg, character.only=TRUE, quietly=TRUE)) install.packages(pkg)
  stopifnot(require(pkg, character.only=TRUE, quietly=TRUE))
}

ensure_package(tidyverse)
ensure_package(glue)
ensure_package(readxl)
ensure_package(tidycensus)

get_acs_all_years <- function(variable, geography="cbsa",
                              start_year=2009, end_year=2023){
  fname <- glue("{variable}_{geography}_{start_year}_{end_year}.csv")
  fname <- file.path("data", "mp02", fname)
  
  if(!file.exists(fname)){
    YEARS <- seq(start_year, end_year)
    YEARS <- YEARS[YEARS != 2020] # Drop 2020 - No survey (covid)
    
    ALL_DATA <- map(YEARS, function(yy){
      tidycensus::get_acs(geography, variable, year=yy, survey="acs1") |>
        mutate(year=yy) |>
        select(-moe, -variable) |>
        rename(!!variable := estimate)
    }) |> bind_rows()
    
    write_csv(ALL_DATA, fname)
  }
  
  read_csv(fname, show_col_types=FALSE)
}

# Household income (12 month)
INCOME <- get_acs_all_years("B19013_001") |>
  rename(household_income = B19013_001)
INCOME
# Monthly rent
RENT <- get_acs_all_years("B25064_001") |>
  rename(monthly_rent = B25064_001)
RENT
# Total population
POPULATION <- get_acs_all_years("B01003_001") |>
  rename(population = B01003_001)
POPULATION
# Total number of households
HOUSEHOLDS <- get_acs_all_years("B11001_001") |>
  rename(households = B11001_001)
HOUSEHOLDS
#
get_building_permits <- function(start_year = 2009, end_year = 2023){
  fname <- glue("housing_units_{start_year}_{end_year}.csv")
  fname <- file.path("data", "mp02", fname)
  
  if(!file.exists(fname)){
    HISTORICAL_YEARS <- seq(start_year, 2018)
    
    HISTORICAL_DATA <- map(HISTORICAL_YEARS, function(yy){
      historical_url <- glue("https://www.census.gov/construction/bps/txt/tb3u{yy}.txt")
      
      LINES <- readLines(historical_url)[-c(1:11)]
      
      CBSA_LINES <- str_detect(LINES, "^[[:digit:]]")
      CBSA <- as.integer(str_sub(LINES[CBSA_LINES], 5, 10))
      
      PERMIT_LINES <- str_detect(str_sub(LINES, 48, 53), "[[:digit:]]")
      PERMITS <- as.integer(str_sub(LINES[PERMIT_LINES], 48, 53))
      
      data_frame(CBSA = CBSA,
                 new_housing_units_permitted = PERMITS, 
                 year = yy)
    }) |> bind_rows()
    
    CURRENT_YEARS <- seq(2019, end_year)
    
    CURRENT_DATA <- map(CURRENT_YEARS, function(yy){
      current_url <- glue("https://www.census.gov/construction/bps/xls/msaannual_{yy}99.xls")
      
      temp <- tempfile()
      
      download.file(current_url, destfile = temp, mode="wb")
      
      fallback <- function(.f1, .f2){
        function(...){
          tryCatch(.f1(...), 
                   error=function(e) .f2(...))
        }
      }
      
      reader <- fallback(read_xlsx, read_xls)
      
      reader(temp, skip=5) |>
        na.omit() |>
        select(CBSA, Total) |>
        mutate(year = yy) |>
        rename(new_housing_units_permitted = Total)
    }) |> bind_rows()
    
    ALL_DATA <- rbind(HISTORICAL_DATA, CURRENT_DATA)
    
    write_csv(ALL_DATA, fname)
    
  }
  
  read_csv(fname, show_col_types=FALSE)
}

PERMITS <- get_building_permits()
PERMITS
# NAICS data schema
ensure_package(httr2)
ensure_package(rvest)
get_bls_industry_codes <- function(){
  fname <- fname <- file.path("data", "mp02", "bls_industry_codes.csv")
  
  if(!file.exists(fname)){
    
    resp <- request("https://www.bls.gov") |> 
      req_url_path("cew", "classifications", "industry", "industry-titles.htm") |>
      req_headers(`User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:143.0) Gecko/20100101 Firefox/143.0") |> 
      req_error(is_error = \(resp) FALSE) |>
      req_perform()
    
    resp_check_status(resp)
    
    naics_table <- resp_body_html(resp) |>
      html_element("#naics_titles") |> 
      html_table() |>
      mutate(title = str_trim(str_remove(str_remove(`Industry Title`, Code), "NAICS"))) |>
      select(-`Industry Title`) |>
      mutate(depth = if_else(nchar(Code) <= 5, nchar(Code) - 1, NA)) |>
      filter(!is.na(depth))
    
    naics_table <- naics_table |> 
      filter(depth == 4) |> 
      rename(level4_title=title) |> 
      mutate(level1_code = str_sub(Code, end=2), 
             level2_code = str_sub(Code, end=3), 
             level3_code = str_sub(Code, end=4)) |>
      left_join(naics_table, join_by(level1_code == Code)) |>
      rename(level1_title=title) |>
      left_join(naics_table, join_by(level2_code == Code)) |>
      rename(level2_title=title) |>
      left_join(naics_table, join_by(level3_code == Code)) |>
      rename(level3_title=title) |>
      select(-starts_with("depth")) |>
      rename(level4_code = Code) |>
      select(level1_title, level2_title, level3_title, level4_title, 
             level1_code,  level2_code,  level3_code,  level4_code)
    
    write_csv(naics_table, fname)
  }
  
  read_csv(fname, show_col_types=FALSE)
  
}

INDUSTRY_CODES <- get_bls_industry_codes()
INDUSTRY_CODES
#BLS Quarterly Census of Employment and Wages
ensure_package(httr2)
ensure_package(rvest)
get_bls_qcew_annual_averages <- function(start_year=2009, end_year=2023){
  fname <- glue("bls_qcew_{start_year}_{end_year}.csv.gz")
  fname <- file.path("data", "mp02", fname)
  
  YEARS <- seq(start_year, end_year)
  YEARS <- YEARS[YEARS != 2020] # Drop Covid year to match ACS
  
  if(!file.exists(fname)){
    ALL_DATA <- map(YEARS, .progress=TRUE, possibly(function(yy){
      fname_inner <- file.path("data", "mp02", glue("{yy}_qcew_annual_singlefile.zip"))
      
      if(!file.exists(fname_inner)){
        request("https://www.bls.gov") |> 
          req_url_path("cew", "data", "files", yy, "csv",
                       glue("{yy}_annual_singlefile.zip")) |>
          req_headers(`User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:143.0) Gecko/20100101 Firefox/143.0") |> 
          req_retry(max_tries=5) |>
          req_perform(fname_inner)
      }
      
      if(file.info(fname_inner)$size < 755e5){
        warning(sQuote(fname_inner), "appears corrupted. Please delete and retry this step.")
      }
      
      read_csv(fname_inner, 
               show_col_types=FALSE) |> 
        mutate(YEAR = yy) |>
        select(area_fips, 
               industry_code, 
               annual_avg_emplvl, 
               total_annual_wages, 
               YEAR) |>
        filter(nchar(industry_code) <= 5, 
               str_starts(area_fips, "C")) |>
        filter(str_detect(industry_code, "-", negate=TRUE)) |>
        mutate(FIPS = area_fips, 
               INDUSTRY = as.integer(industry_code), 
               EMPLOYMENT = as.integer(annual_avg_emplvl), 
               TOTAL_WAGES = total_annual_wages) |>
        select(-area_fips, 
               -industry_code, 
               -annual_avg_emplvl, 
               -total_annual_wages) |>
        # 10 is a special value: "all industries" , so omit
        filter(INDUSTRY != 10) |> 
        mutate(AVG_WAGE = TOTAL_WAGES / EMPLOYMENT)
    })) |> bind_rows()
    
    write_csv(ALL_DATA, fname)
  }
  
  ALL_DATA <- read_csv(fname, show_col_types=FALSE)
  
  ALL_DATA_YEARS <- unique(ALL_DATA$YEAR)
  
  YEARS_DIFF <- setdiff(YEARS, ALL_DATA_YEARS)
  
  if(length(YEARS_DIFF) > 0){
    stop("Download failed for the following years: ", YEARS_DIFF, 
         ". Please delete intermediate files and try again.")
  }
  
  ALL_DATA
}

WAGES <- get_bls_qcew_annual_averages()
WAGES 
HOUSEHOLDS |>
  group_by()
# Q1. Which CBSA (by name) permitted the largest number of new housing units in the decade from 2010 to 2019 (inclusive)?

library(dplyr)
library(lubridate)

# 1) Filter years 2010–2019 by CBSA
permits_agg <- PERMITS |>
  filter(dplyr::between(year, 2010, 2019)) |>
  group_by(CBSA) |>
  summarise(total_units = sum(new_housing_units_permitted, na.rm = TRUE),
            .groups = "drop")

# 2) Create lookup CBSA -> NAME from HOUSEHOLDS

cbsa_lookup <- HOUSEHOLDS |>
  count(GEOID, NAME, sort = TRUE) |>                 
  group_by(GEOID) |>
  slice_max(n, n = 1, with_ties = FALSE) |>          
  ungroup() |>
  select(GEOID, NAME)
cbsa_lookup

largest_new_housing_CBSA <- permits_agg |>
  left_join(cbsa_lookup, by = c("CBSA" = "GEOID")) |>

  mutate(NAME = dplyr::coalesce(NAME, as.character(CBSA))) |>
  slice_max(total_units, n = 1, with_ties = FALSE) |>
  select(CBSA, NAME, total_units)

largest_new_housing_CBSA

#Q2 In what year did Albuquerque, NM (CBSA Number 10740) permit the most new housing units?
new_housing_CBSA_10740_2021 <- PERMITS |>
  filter(CBSA=="10740") |>
  group_by(year) |>
  summarize( total_new_units = sum(new_housing_units_permitted)) |>
  slice_max(total_new_units, n=2,with_ties = TRUE )
new_housing_CBSA_10740_2021

#Q3 Which state (not CBSA) had the highest average individual income in 2015?
income_CBSA <- INCOME |>
  filter(year=="2015") |>
  group_by(NAME) |>
  summarize(total_income_CBSA= sum(household_income))
income_CBSA

households_CBSA <- HOUSEHOLDS |>
  filter(year=="2015") |>
  group_by(NAME) |>
  summarize(total_households= sum(households))


population_CBSA <- POPULATION |>
  filter(year=="2015") |>
  group_by(NAME) |>
  summarize(total_population= sum(population))

final_table <- income_CBSA |>
  inner_join(households_CBSA, by="NAME") |>
  inner_join(population_CBSA, by="NAME") |>
  group_by(NAME) |>
  mutate(state = str_extract(NAME, ", (.{2})", group=1)) |>
  mutate(total_avg_income= (total_income_CBSA/total_population))
final_table

ave_income_ind <- final_table |>
  group_by(state) |>
  summarize(total_average_income_by_state=sum(total_avg_income)) |>
  slice_max(total_average_income_by_state, n=1, with_ties = TRUE)
ave_income_ind  
# Q4 Data scientists and business analysts are recorded under NAICS code 5182. 
#What is the last year in which the NYC CBSA had the most data scientists in the country?
t1 <- HOUSEHOLDS |> filter(GEOID=="35620") |> mutate(std_cbsa = paste0("C", GEOID))
t2 <- WAGES |> filter(FIPS=="C3562") |> mutate(std_cbsa = paste0(FIPS, "0"))
NAICS_table <- inner_join(t1, t2, join_by(std_cbsa == std_cbsa))
NAICS_most_data_sciense <- NAICS_table |>
  filter(INDUSTRY=="5182") |>
  group_by(YEAR) |>
  summarize(total_data_science= sum(EMPLOYMENT, na.rm=TRUE)) |>
  slice_max(total_data_science, n=1, with_ties = TRUE)
NAICS_most_data_sciense

#Q5 What fraction of total wages in the NYC CBSA was earned by people employed in the finance and insurance industries (NAICS code 52)? 
#In what year did this fraction peak?
#TOTAL WAGE GENERAL
total_wages <- NAICS_table |>
  summarize(total_wages_general=sum(TOTAL_WAGES))
total_wages
  
#Fraction of Total Wage code 52
NAICS_most_fin_ins <- NAICS_table |>
  filter(INDUSTRY=="52") |>
  summarize(total_wage_code_52=sum(TOTAL_WAGES)/total_wages) 
NAICS_most_fin_ins
#Fraction peak
NAICS_most_fin_ins_by_year <- NAICS_table |>
  filter(INDUSTRY=="52") |>
  group_by(YEAR) |>
  summarize(total_financial_and_insurance= sum(TOTAL_WAGES)/total_wages) |>
  slice_max(total_financial_and_insurance, n=1, with_ties = TRUE)
NAICS_most_fin_ins_by_year
#Task 3: Initial Visualizations
# 1. The relationship between monthly rent and average household income per CBSA in 2009.

mr_ahi <- inner_join( INCOME,RENT, join_by(GEOID, year)) |>
  filter(year==2009)
mr_ahi
ggplot(mr_ahi, aes(x=monthly_rent, y=household_income)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  xlab("Montly Rent (US Dollars)") +
  ylab("Ave. Annual Household Income(US Dollars)") +
  labs(title = "Relationship Between Monthly Rent and Average Household Income (2009)",
       subtitle = "Each point represents a CBSA (Core-Based Statistical Area)",
       caption = "Source: ACS 1-Year Estimates, 2009")+
  theme_bw(base_size = 12) +
  stat_smooth(se=FALSE, color="red4") +
  scale_x_continuous(labels=scales::dollar) +
  scale_y_continuous(labels=scales::dollar) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray70"),
    plot.caption = element_text(size = 9, color = "gray40", hjust = 1)
  )
#2 The relationship between total employment and total employment in the health care and social services sector (NAICS 62) 
#across different CBSAs. Design your visualization so that it is possible to see the evolution of this relationship over time
WAGES_named <- WAGES |> mutate(std_cbsa = paste0(FIPS, "0"))
general_employment_cbsa <- WAGES_named |>
  group_by(std_cbsa,YEAR) |>
  summarize(total_employment= sum(EMPLOYMENT, na.rm = TRUE)) 

healt_employment_cbsa <- WAGES_named |>
  filter(INDUSTRY==62) |>
  group_by(std_cbsa,YEAR) |>
  summarize(total_employment_health= sum(EMPLOYMENT, na.rm = TRUE)) 
healt_employment_cbsa
table_cbsa_employment <- inner_join(general_employment_cbsa ,healt_employment_cbsa, join_by(std_cbsa,YEAR))
table_cbsa_employment


ggplot(table_cbsa_employment, aes(x=total_employment, y=total_employment_health)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick") +
  facet_wrap(~ YEAR, ncol = 4) +
  scale_x_continuous(labels = label_number(scale_cut = cut_short_scale()),  # 1M → 1M, 500000 → 500K
                     expand = expansion(mult = c(0, 0.05)))
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Health Care vs Total Employment Across CBSAs (2009–2023)",
    x = "Total Employment (All Industries)",
    y = "Employment in Health Care & Social Assistance (NAICS 62)",
    caption = "Source: BLS QCEW Annual Averages"
  ) +
  theme_bw(base_size = 11)
#3 
library(gghighlight)
avg_household_size <- POPULATION |>
    select(GEOID,NAME,year,population) |>
    inner_join(HOUSEHOLDS |> select(GEOID,year, households), by=c("GEOID", "year")) |>
    mutate(avg_household_size = population / households)  
avg_household_size

nyc_la_ids <- c(35620, 31080)
 
library(ggplot2)

ggplot(avg_household_size, aes(x = year, y = avg_household_size, group = GEOID, color = NAME)) +
  geom_line(alpha = 0.7, linewidth = 0.8) +
  gghighlight(GEOID %in% nyc_la_ids, use_direct_label = TRUE) +
  labs(
    title = "Evolution of Average Household Size (2009–2023)",
    subtitle = "New York (35620) and Los Angeles (31080) highlighted",
    x = "Year",
    y = "Average Household Size",
    color = "CBSA",
    caption= "Source:US Census Bureau, American Community Survey"
  ) +
  scale_x_continuous(breaks = seq(2009, 2023, 2)) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",          # hide if too many CBSAs
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(face = "bold")
  )
top_cbsa <- avg_household_size |>
  group_by(NAME) |>
  summarise(mean_pop = mean(population, na.rm = TRUE)) |>
  slice_max(mean_pop, n = 10)

ggplot(filter(avg_household_size, NAME %in% top_cbsa$NAME),
       aes(x = year, y = avg_household_size, color = NAME)) +
  geom_line(linewidth = 1) +
  labs(title = "Evolution of Average Household Size (Top 10 CBSAs)",
       x = "Year", y = "Average Household Size", color = "CBSA") +
  theme_bw(base_size = 12)

#Task 4 

rent_burden <- INCOME |>
  select(GEOID, NAME, year, household_income) |>
  inner_join(RENT |> select(GEOID, year, monthly_rent),
             by = c("GEOID","year")) |>
  mutate(rent_to_income = 100 * monthly_rent / (household_income / 12)) |>
  select(GEOID, NAME, year, rent_to_income)

# Min–max 0–100 within each year, with a guard for zero-range years
min_max_year <- rent_burden |>
  group_by(year) |>
  mutate(
    .min = min(rent_to_income, na.rm = TRUE),
    .max = max(rent_to_income, na.rm = TRUE),
    rti_0_100_byyear = if_else(
      .max > .min,
      100 * (rent_to_income - .min) / (.max - .min),
      NA_real_         # all equal within year -> undefined scaling
    )
  ) |>
  ungroup() |>
  select(GEOID, NAME, year, rent_to_income, rti_0_100_byyear)

min_max_33100 <- min_max_year |> filter(GEOID == "33100")




  

format_titles <- function(df){
  names(df) <- names(df) |> str_replace_all("_", " ") |> str_to_title()
  df
}


tbl <- min_max_33100 |>
  # keep only columns we want, and add a [0,1] column for percentage formatting
  select(GEOID, NAME, year, rent_to_income, rti_0_100_byyear) |>
  mutate(rent_to_income_frac = rent_to_income / 100) |>
  select(-rent_to_income)

DT::datatable(
  tbl |>
    rename(
      `CBSA` = GEOID,
      `Name`= NAME,
      `Rent-to-Income` = rent_to_income_frac,
      `Rent Burden Index (0–100)` = rti_0_100_byyear,
      `Year`= year
    ),
  options = list(searching = FALSE, info = FALSE),
  rownames = FALSE
) |>
  DT::formatPercentage(columns = "Rent-to-Income", digits = 2) |>
  DT::formatRound(columns = "Rent Burden Index (0–100)", digits = 2)

#Highlight the Metro Areas highest and lowest with the highest and lowest rent burden
highest_metro_areas <-min_max_year |>
  group_by(GEOID) |>
  slice_max(rti_0_100_byyear, n=5,with_ties = TRUE) |>
  ungroup() |>
  distinct(GEOID,NAME)

# overall top/bottom by their extreme values
cbsa_extremes_max <- min_max_year |>
  group_by(GEOID, NAME) |>
  summarise(max_idx = max(rti_0_100_byyear, na.rm = TRUE),
            .groups = "drop")

top5_overall    <- cbsa_extremes_max |> slice_max(max_idx, n = 5, with_ties = TRUE)


to_highlight <- bind_rows(top5_overall, bottom5_overall) |> distinct(GEOID,NAME)

cbsa_extremes_min <- min_max_year |>
  group_by(GEOID, NAME) |>
  summarise(min_idx = min(rti_0_100_byyear, na.rm = TRUE),
            .groups = "drop")
bottom5_overall <- cbsa_extremes_min |> slice_min(min_idx, n = 5, with_ties = TRUE)

# (A) Long-term national average = 100

# NATIONAL long-term stats (across all CBSAs/years except 2020 gap)
nat_longterm_mean <- mean(rent_burden$rent_to_income, na.rm = TRUE)
nat_longterm_sd   <- sd(rent_burden$rent_to_income, na.rm = TRUE)
nat_min           <- min(rent_burden$rent_to_income, na.rm = TRUE)
nat_max           <- max(rent_burden$rent_to_income, na.rm = TRUE)

rent_burden <- rent_burden |>
  mutate(rti_index_longterm100 = 100 * rent_to_income / nat_longterm_mean)
# (C1) Min–max 0–100 (GLOBAL across all years)
rent_burden <- rent_burden |>
  mutate(
    rti_minmax_0_100 =
      100 * (rent_to_income - nat_min) / (nat_max - nat_min)
  )
# overall top by their extreme values
cbsa_max <- rent_burden |>
  group_by(GEOID, NAME) |>
  summarise(max_idx = max(rti_minmax_0_100, na.rm = TRUE),
            .groups = "drop")

top5_overall    <- cbsa_max |> slice_max(max_idx, n = 5, with_ties = TRUE)

# overall bottom by their extreme values
cbsa_min <- rent_burden |>
  group_by(GEOID, NAME) |>
  summarise(min_idx = min(rti_minmax_0_100, na.rm = TRUE),
            .groups = "drop")

bottom5_overall    <- cbsa_min |> slice_min(min_idx, n = 5, with_ties = TRUE)
#Data Frame top 5 
DT::datatable(
  top5_overall |>
    rename(
      `CBSA` = GEOID,
      `Name`= NAME,
      `Max Rent Burden Index Long Years (0–100)` = max_idx,
    ),
  options = list(searching = FALSE, info = FALSE),
  rownames = FALSE
) |>
  DT::formatRound(columns = "Max Rent Burden Index Long Years (0–100)", digits = 2)

#Data frame bottom top 5
DT::datatable(
  bottom5_overall |>
    rename(
      `CBSA` = GEOID,
      `Name`= NAME,
      `Min Rent Burden Index Long Years (0–100)` = min_idx,
    ),
  options = list(searching = FALSE, info = FALSE),
  rownames = FALSE
) |>
  DT::formatRound(columns = "Min Rent Burden Index Long Years (0–100)", digits = 2)

#Task 5: Housing Growth 
#An ‘instantaneous’ measure of housing growth that depends on the absolute population of a CBSA and the number 
#of new housing units permitted that year.

# 1) Build average household size once (lean columns; unique per GEOID-year)
avg_hhsize <- POPULATION |>
  select(GEOID, year, population, NAME) |>
  inner_join(HOUSEHOLDS |> select(GEOID, year, households),
             by = c("GEOID","year")) |>
  mutate(avg_household_size = population / households) |>
  select(GEOID, year, NAME, population, avg_household_size)

# 2) Join PERMITS (rename CBSA -> GEOID for a clean key), keep 2014 +
housing_growth <- avg_hhsize |>
  inner_join(
    PERMITS |> rename(GEOID = CBSA) |> select(GEOID, year, new_housing_units_permitted),
    by = c("GEOID","year")
  ) |>
  filter(year >= 2014)

# 3) Instantaneous growth metrics
hg_rate <- housing_growth |>
  mutate(
    hg_rate_CBSA = 100 * (new_housing_units_permitted * avg_household_size) / population, # % 
    G_per_1000   = 1000 * new_housing_units_permitted / population                        # units per 1,000
  )

# 4) Global min–max index (0–100) across all years in scope (2014+ here)
# Ensure these are distinct scalar names
hg_min_val <- min(hg_rate$hg_rate_CBSA, na.rm = TRUE)
hg_max_val <- max(hg_rate$hg_rate_CBSA, na.rm = TRUE)
denom <- hg_max_val - hg_min_val

hg_rate <- hg_rate |>
  mutate(hg_minmax_0_100 = 100 * (hg_rate_CBSA - hg_min_val) / ifelse(denom > 0, denom, NA_real_))


# 5) Overall TOP/BOTTOM CBSAs by their extreme index value 
top5_overall_hg <- hg_rate |>
  group_by(GEOID, NAME) |>
  slice_max(hg_minmax_0_100, n = 1, with_ties = FALSE) |>  # keep row of the max (gets the year too)
  ungroup() |>
  slice_max(hg_minmax_0_100, n = 5, with_ties = TRUE) |>
  select(GEOID, NAME, year, hg_rate_CBSA, hg_minmax_0_100)

bottom5_overall_hg <- hg_rate |>
  group_by(GEOID, NAME) |>
  slice_min(hg_minmax_0_100, n = 1, with_ties = FALSE) |>
  ungroup() |>
  slice_min(hg_minmax_0_100, n = 5, with_ties = TRUE) |>
  select(GEOID, NAME, year, hg_rate_CBSA, hg_minmax_0_100)
#A ‘rate-based’ measure of housing growth that compares the number of housing permits to the population 
#growth over a 5 year lookback window.
library(RcppRoll)


# Step 1: join permits + population
rate_hg_pop <- PERMITS |>
  rename(GEOID = CBSA) |>
  inner_join(POPULATION |> select(GEOID, year, population,NAME), by = c("GEOID", "year")) |>
  arrange(GEOID, year)

# Step 2: compute 5-year rolling sum of permits and 5-year population change
rate_hg_pop <- rate_hg_pop |>
  group_by(GEOID) |>
  mutate(
    permits_5yr_sum = roll_sum(new_housing_units_permitted, n = 5, fill = NA, align = "right"),
    pop_5yr_change  = population - dplyr::lag(population, 5),
    rate_housing_growth = ifelse(pop_5yr_change > 0,
                                 permits_5yr_sum / pop_5yr_change,
                                 NA_real_)
  ) |>
  ungroup()
# 3) Global min–max index (0–100) across all years 
# Ensure these are distinct scalar names
hg_pop_min_val <- min(rate_hg_pop$rate_housing_growth, na.rm = TRUE)
hg_pop_max_val <- max(rate_hg_pop$rate_housing_growth, na.rm = TRUE)
denom <- hg_pop_max_val - hg_pop_min_val

rate_hg_pop<- rate_hg_pop |>
  mutate(hg_pop_minmax_0_100 = 100 * (rate_housing_growth - hg_pop_min_val) / ifelse(denom > 0, denom, NA_real_))


# 5) Overall TOP/BOTTOM CBSAs by their extreme index value 
top5_overall_pop <- rate_hg_pop |>
  group_by(GEOID, NAME) |>
  slice_max(hg_pop_minmax_0_100, n = 1, with_ties = FALSE) |>  # keep row of the max (gets the year too)
  ungroup() |>
  slice_max(hg_pop_minmax_0_100, n = 5, with_ties = TRUE) |>
  select(GEOID, NAME, year, rate_housing_growth, hg_pop_minmax_0_100)

bottom5_overall_pop <- rate_hg_pop |>
  group_by(GEOID, NAME) |>
  slice_min(hg_pop_minmax_0_100, n = 1, with_ties = FALSE) |>
  ungroup() |>
  slice_min(hg_pop_minmax_0_100, n = 5, with_ties = TRUE) |>
  select(GEOID, NAME, year,  rate_housing_growth, hg_pop_minmax_0_100)

#COMPOSTE

hg_rate_clean <- hg_rate %>%
  select(GEOID, year, NAME, hg_minmax_0_100) %>%
  distinct()

rate_hg_pop_clean <- rate_hg_pop %>%
  select(GEOID, year, NAME, hg_pop_minmax_0_100) %>%
  distinct()


composite <- inner_join(
  hg_rate_clean,                 # has hg_minmax_0_100
  rate_hg_pop_clean,             # has hg_pop_minmax_0_100
  by = c("GEOID", "year")        # <- include year!
) %>%
  rename(NAME = NAME.x) %>%
  select(GEOID, year, NAME, hg_minmax_0_100, hg_pop_minmax_0_100)

composite <- composite %>%
  mutate(
    composite_avg = (hg_minmax_0_100 + hg_pop_minmax_0_100) / 2
  )

top5_composite <- composite %>%
  group_by(GEOID, NAME) %>%
  summarise(avg_composite = mean(composite_avg, na.rm = TRUE), .groups="drop") %>%
  slice_max(avg_composite, n = 5)

bottom5_composite <- composite %>%
  group_by(GEOID, NAME) %>%
  summarise(avg_composite = mean(composite_avg, na.rm = TRUE), .groups="drop") %>%
  slice_min(avg_composite, n = 5)
#Data Frame top 5 
DT::datatable(
  top5_composite |>
    rename(
      `CBSA` = GEOID,
      `Name`= NAME,
      `Average (balanced scale)` = avg_composite,
    ),
  options = list(searching = FALSE, info = FALSE),
  rownames = FALSE
) |>
  DT::formatRound(columns = "Average (balanced scale)", digits = 2)

#Data frame bottom top 5
DT::datatable(
  bottom5_composite |>
    rename(
      `CBSA` = GEOID,
      `Name`= NAME,
      `Average (balanced scale)` = avg_composite,
    ),
  options = list(searching = FALSE, info = FALSE),
  rownames = FALSE
) |>
  DT::formatRound(columns = "Average (balanced scale)", digits = 2)
#High composite score (Top 5 CBSAs) → regions showing both strong construction activity and consistent population-aligned growth.
#These are high-growth, expanding metros.

#Low composite score (Bottom 5 CBSAs) → weak or stagnant housing dynamics (low permits, low or negative population growth).

#Task 6: Visualization
#Create (at least) two visualizations to investigate the relationships between your Rent Burden and Housing Growth metrics. 
#Using these plots, identify the most “YIMBY” CBSAs as ones which:
rent_growth <- rent_burden %>%
  select(GEOID, NAME, year, rent_to_income) %>%
  inner_join(hg_rate %>% select(GEOID, NAME, year, hg_rate_CBSA, hg_minmax_0_100),
             by = c("GEOID", "NAME", "year")) %>%
  inner_join(POPULATION %>% select(GEOID, year, population), by = c("GEOID", "year"))

rent_growth %>% count(GEOID) %>% summary(n)
rent_growth %>% count(GEOID, year) %>% filter(n > 1)
rent_growth <- rent_growth %>%
  arrange(GEOID, year) %>%                     # ensure years in order
  group_by(GEOID, NAME) %>%
  mutate(
    rent_change = last(rent_to_income) - first(rent_to_income),
    pop_change  = last(population) - first(population),
    mean_housing_growth = mean(hg_rate_CBSA, na.rm = TRUE)
  ) %>%
  ungroup()
rent_growth %>%
  filter(NAME == "Austin-Round Rock-Georgetown, TX Metro Area") %>%
  select(year, rent_to_income, population) %>%
  arrange(year)


ggplot(rent_growth, aes(x = mean_housing_growth, y = rent_change)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(
    title = "Relationship Between Housing Growth and Rent Burden Change",
    subtitle = "Each point is a CBSA (2009–2023)",
    x = "Average Housing Growth Rate (2009–2023)",
    y = "Change in Rent Burden (%)"
  ) +
  theme_bw(base_size = 12)

library(gghighlight)

ggplot(rent_growth, aes(x = year, y = rent_to_income, group = NAME, color = NAME)) +
  geom_line(alpha = 0.6) +
  geom_line(aes(y = hg_minmax_0_100), linetype = "dashed") +
  gghighlight(NAME %in% c("Austin-Round Rock-Georgetown, TX Metro Area",
                          "Raleigh-Cary, NC Metro Area",
                          "Boise City, ID Metro Area"),
              unhighlighted_params = list(alpha = 0.1, color = "gray")) +
  labs(
    title = "Rent Burden (solid) vs Housing Growth (dashed) Over Time",
    subtitle = "Selected CBSAs showing falling rents and increasing housing supply",
    x = "Year",
    y = "Index (0–100)",
    color = "CBSA"
  ) +
  theme_bw(base_size = 12)

library(dplyr)
library(dplyr)

# join on GEOID + year; keep only needed cols
rent_growth <- rent_burden %>%
  select(GEOID, year, rent_to_income) %>%
  inner_join(hg_rate %>% select(GEOID, year, hg_rate_CBSA, hg_minmax_0_100), by = c("GEOID","year")) %>%
  inner_join(POPULATION %>% select(GEOID, year, population, NAME), by = c("GEOID","year")) %>%
  arrange(GEOID, year)

# one row per CBSA with start/end and averages
rent_summary <- rent_growth %>%
  group_by(GEOID, NAME) %>%
  summarise(
    start_year  = first(year),
    end_year    = last(year),
    start_rent  = first(rent_to_income),
    end_rent    = last(rent_to_income),
    rent_change = end_rent - start_rent,           # ↓ good
    start_pop   = first(population),
    end_pop     = last(population),
    pop_change  = end_pop - start_pop,             # ↑ good
    mean_housing_growth = mean(hg_rate_CBSA, na.rm = TRUE),
    .groups = "drop"
  )

# pick YIMBY candidates by the 4 rules
yimby_cities <- rent_summary %>%
  filter(
    start_rent > median(start_rent, na.rm = TRUE),       # high early burden
    rent_change < 0,                                     # burden fell
    pop_change > 0,                                      # population grew
    mean_housing_growth > mean(mean_housing_growth, na.rm = TRUE) # above-avg supply
  )

library(ggplot2)
library(ggrepel)

ggplot(rent_summary, aes(x = mean_housing_growth, y = rent_change)) +
  geom_hline(yintercept = 0, linetype = 2, color = "gray60") +
  geom_vline(xintercept = mean(rent_summary$mean_housing_growth, na.rm = TRUE),
             linetype = 2, color = "gray60") +
  geom_point(alpha = 0.25, color = "gray60") +
  geom_point(data = yimby_cities, color = "darkgreen", size = 2) +
  ggrepel::geom_text_repel(
    data = yimby_cities,
    aes(label = NAME),
    size = 3, max.overlaps = 20, seed = 42, box.padding = 0.3
  ) +
  labs(
    title = "More Building, Lower Rents? (CBSA-level, study period)",
    subtitle = "Points = CBSAs; highlighted = YIMBY candidates",
    x = "Average Housing Growth (instantaneous rate)",
    y = "Change in Rent Burden (end − start, percentage points)"
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "none")

## Rent Burden Metric & Standardization
In this step, we (1) construct a rent-to-income metric per CBSA/year; (2) standardize it two ways—by-year min–max (0–100) to compare metros within the same year, and global scales (long-term baseline = 100 and global min–max 0–100) to compare across years; and (3) identify CBSAs with highest/lowest rent burden over the period. We also show an example table for a specific CBSA.
