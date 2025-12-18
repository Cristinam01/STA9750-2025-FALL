# Step 1 Data Collection
## --- Libraries ---
library(devtools)   # for install_github if needed
library(RSocrata)
library(janitor)
library(lubridate)
library(dplyr)
library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
library(tidygeocoder)
library(scales)
library(purrr)

# --- Tokens / Keys ---
app_token <- "ExUt22WiPiJvXNo3ZwPuIUDuC"
census_api_key("5cd8f72bd473b6e815af190efa42dfdcd3804b99", install = FALSE)

# --- HPD Violations (2023) ---
url_hpd <- "https://data.cityofnewyork.us/resource/wvxf-dwi5.csv?$limit=100000"
hpd_raw <- read.socrata(url_hpd, app_token = app_token) %>% clean_names()

# --- 311 Complaints (2023) ---
url_311 <- paste0(
  "https://data.cityofnewyork.us/resource/erm2-nwe9.json?",
  "$select=unique_key,created_date,complaint_type,descriptor,incident_address,incident_zip,borough,latitude,longitude",
  "&$where=created_date >= '2018-01-01T00:00:00' AND created_date < '2024-01-01T00:00:00'",
  "&$limit=50000"
)
nyc311_raw <- read.socrata(url_311, app_token = app_token) %>% clean_names()

# --- DOB Complaints (2023) ---
url_dob <- "https://data.cityofnewyork.us/resource/vztk-gaf7.csv?$limit=50000"
dob_raw <- read.socrata(url_dob, app_token = app_token) %>% clean_names()

# --- ACS Data (2023) ---
vars <- c(
  income        = "B19013_001E",
  rent_burden   = "B25070_001E",
  housing_units = "B25002_001E"
)
nyc_counties <- c("New York","Kings","Queens","Bronx","Richmond")
years <- 2018:2023

acs_list <- lapply(years, function(y) {
  get_acs(
    geography = "tract",
    variables = vars,
    year = y,
    survey = "acs5",
    state = "NY",
    county = nyc_counties,
    output = "wide"
  ) %>% mutate(year = y)
})

acs_nyc_tracts <- bind_rows(acs_list)

# ------------------------------------------------------------------------------
# Step 2: Cleaning the Data:
# --- HPD Cleaning ---
hpd_clean <- hpd_raw %>%
  mutate(
    novissueddate       = ymd(novissueddate),
    inspectiondate      = ymd(inspectiondate),
    approveddate        = ymd(approveddate),
    originalcertifybydate = ymd(originalcertifybydate),
    originalcorrectbydate = ymd(originalcorrectbydate),
    newcertifybydate    = ymd(newcertifybydate),
    newcorrectbydate    = ymd(newcorrectbydate),
    certifieddate       = ymd(certifieddate),
    currentstatusdate   = ymd(currentstatusdate),
    date = novissueddate,
    year = year(date),
    month = month(date),
    dow   = wday(date, label = TRUE),
    boroid = suppressWarnings(as.integer(boroid)),
    block  = suppressWarnings(as.integer(block)),
    lot    = suppressWarnings(as.integer(lot)),
    bbl    = if_else(!is.na(boroid) & !is.na(block) & !is.na(lot),
                     sprintf("%01d%05d%04d", boroid, block, lot), NA_character_)
  ) %>%
  filter(between(date, as.Date("2018-01-01"), as.Date("2023-12-31")),
         class %in% c("A","B","C"),
         !is.na(bbl), !is.na(boro))

# --- 311 Cleaning ---
nyc311_clean <- nyc311_raw %>%
  mutate(
    created_date = ymd_hms(created_date),
    date  = as_date(created_date),
    year  = year(created_date),
    month = month(created_date),
    dow   = wday(created_date, label = TRUE)
  ) %>%
  filter(between(date, as.Date("2018-01-01"), as.Date("2023-12-31")),
         !is.na(latitude), !is.na(longitude))

# --- DOB Cleaning ---
dob_clean <- dob_raw %>%
  mutate(
    date_entered     = mdy(date_entered),
    inspection_date  = mdy(inspection_date),
    disposition_date = mdy(disposition_date),
    dobrundate       = mdy(dobrundate),
    date  = date_entered,
    year  = year(date_entered),
    month = month(date_entered),
    dow   = wday(date_entered, label = TRUE),
    full_address = paste0(house_number, " ", house_street, ", NY ", zip_code)
  ) %>%
  filter(between(date, as.Date("2018-01-01"), as.Date("2023-12-31")))

# --- ACS Cleaning ---
acs_clean <- acs_nyc_tracts %>%
  mutate(
    tract_geoid   = GEOID,
    median_income = income,
    rent_burden   = rent_burden,
    housing_units = housing_units
  ) %>%
  dplyr::select(tract_geoid, median_income, rent_burden, housing_units, year)

# ------------------------------------------------------------------------------
# Step 3: Spatial Join for Datasets
# 311 Complaints:


# Load tract polygons for NYC counties
nyc_counties <- c("New York","Kings","Queens","Bronx","Richmond")
  tracts_sf <- tracts(state = "NY", county = nyc_counties, cb = TRUE) %>%
  st_transform(2263)   # NY State Plane CRS

# Convert 311 complaints to sf
nyc311_sf <- nyc311_clean %>%
  st_as_sf(coords = c("longitude","latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(2263)

# Spatial join: assign tract GEOID to each complaint
nyc311_join <- st_join(nyc311_sf, tracts_sf["GEOID"], left = TRUE) %>%
  st_drop_geometry() %>%
  rename(tract_geoid = GEOID)
--------------------------------------------------------------------------------
# Load MapPluto in R


pluto_path <- "C:/Users/crist/Downloads/nyc_mappluto_25v3_fgdb/MapPLUTO25v3.gdb"

# Read the correct MapPLUTO layer
pluto <- st_read(dsn = pluto_path, layer = "MapPLUTO_25v3_clipped") %>%
  st_transform(2263)   # NY State Plane CRS

# Load tract polygons for NYC
nyc_counties <- c("New York","Kings","Queens","Bronx","Richmond")
tracts_sf <- tracts(state = "NY", county = nyc_counties, year = 2023, cb = TRUE) %>%
  st_transform(2263)

# Spatial join: assign tract GEOID to each PLUTO lot
pluto_with_tract <- st_join(pluto, tracts_sf["GEOID"], left = TRUE)

# Create lookup table: BBL ??? tract GEOID
pluto_lookup <- pluto_with_tract %>%
  st_drop_geometry() %>%
  dplyr::select(BBL, GEOID) %>%
  dplyr::rename(tract_geoid = GEOID)

#Join HPD and DOB complaints by BBL (Borough - Block - Lot)

# Make both BBL fields character
pluto_lookup <- pluto_lookup %>%
  mutate(BBL = as.character(BBL))

hpd_clean <- hpd_clean %>%
  mutate(bbl = as.character(bbl))

# Join HPD complaints to tract GEOIDs (many-to-many is fine here)
hpd_join <- hpd_clean %>%
  inner_join(pluto_lookup, by = c("bbl" = "BBL"))

# Aggregate: complaints per tract per year
hpd_counts <- hpd_join %>%
  group_by(tract_geoid, year) %>%
  summarise(hpd_complaints = n(), .groups = "drop")

#DOB complaints:

census_api_key(Sys.getenv("CENSUS_API_KEY"))



geocode_in_chunks <- function(df, chunk_size = 9000) {
  split_df <- split(df, ceiling(seq_len(nrow(df)) / chunk_size))
  
  map_dfr(split_df, ~ geocode(
    .x,
    address = full_address,
    method = "census",
    lat = latitude,
    long = longitude
  ))
}

dob_geo <- geocode_in_chunks(dob_clean, chunk_size = 9000)



# Drop failed geocodes
dob_geo_clean <- dob_geo %>%
  filter(!is.na(latitude), !is.na(longitude))

# Convert to sf and join to tracts
dob_sf <- dob_geo_clean %>%
  st_as_sf(coords = c("longitude","latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(2263)

dob_join <- st_join(dob_sf, tracts_sf["GEOID"], left = TRUE) %>%
  st_drop_geometry() %>%
  rename(tract_geoid = GEOID)

# Aggregate by tract/year
dob_counts <- dob_join %>%
  group_by(tract_geoid, year) %>%
  summarise(dob_complaints = n(), .groups = "drop")


# ===================================================================================================================================
#4. Do lower-income neighborhoods experience higher rates of unresolved complaints?
#===================================================================================================================================
hpd_clean_q4 <- hpd_join |>
  dplyr::select ( tract_geoid,year, violationstatus) |>
  ungroup()
dob_clean_q4 <- dob_join |> 
  dplyr::select(tract_geoid, year, status, disposition_date) |>
  ungroup()

#===================================================================================================================================
# UNRESOLVED FLAGS 
#===================================================================================================================================
## HPD: unresolved_hpd ----
hpd_q4 <- hpd_clean_q4 %>%
  mutate(
    # clean status to avoid NA problems and case issues
    violationstatus_clean = str_to_upper(coalesce(violationstatus, "")),
    
    # unresolved = 1 if NOT any kind of CLOSE
    unresolved_hpd = if_else(
      str_detect(violationstatus_clean, "CLOSE"),
      0L,  
      1L   
    )
  )

## DOB: unresolved_dob 
dob_q4 <- dob_clean_q4 %>%
  mutate(
    status_clean = str_to_upper(coalesce(status, "")),
    
    # unresolved = 1 if status does NOT contain "CLOSED"
    unresolved_dob = if_else(
      str_detect(status_clean, "CLOSED"),
      0L,  
      1L   
    )
  )

# ===================================================================================================================================
# AGGREGATE + MERGE ACS
# ====================================================================================================================================
## HPD: count total + unresolved per tract/year 
hpd_by_tract <- hpd_q4 %>%
  group_by(tract_geoid, year) %>%
  summarise(
    hpd_n        = n(),                                   # total HPD violations
    hpd_unres    = sum(unresolved_hpd, na.rm = TRUE),     # unresolved HPD
    hpd_unres_rate = hpd_unres / hpd_n,                   # unresolved rate
    .groups = "drop"
  )

## DOB: count total + unresolved per tract/year 
dob_by_tract <- dob_q4 %>%
  group_by(tract_geoid, year) %>%
  summarise(
    dob_n        = n(),                                   # total DOB complaints
    dob_unres    = sum(unresolved_dob, na.rm = TRUE),     # unresolved DOB
    dob_unres_rate = dob_unres / dob_n,                   # unresolved rate
    .groups = "drop"
  )

complaints_q4 <- acs_clean %>%
  left_join(hpd_by_tract, by = c("tract_geoid", "year")) %>%
  left_join(dob_by_tract, by = c("tract_geoid", "year")) %>%
  mutate(
    # treat missing counts as 0 (tracts with no HPD/DOB complaints)
    hpd_n     = coalesce(hpd_n, 0L),
    hpd_unres = coalesce(hpd_unres, 0L),
    dob_n     = coalesce(dob_n, 0L),
    dob_unres = coalesce(dob_unres, 0L),
    
    # combined totals
    total_n        = hpd_n + dob_n,
    total_unres    = hpd_unres + dob_unres,
    total_unres_rate = if_else(
      total_n > 0,
      total_unres / total_n,
      0
    )
  ) 

unresolved_by_quintile <- complaints_q4 %>%
  # make income quintiles (drop NAs so ntile() doesn't complain)
  filter(!is.na(median_income)) %>%
  mutate(
    income_quintile = ntile(as.numeric(median_income), 5)
  ) %>%
  group_by(income_quintile) %>%
  summarise(
    n_complaints  = sum(total_n,   na.rm = TRUE),   
    n_unresolved  = sum(total_unres, na.rm = TRUE), 
    unresolved_rate = n_unresolved / n_complaints,
    .groups = "drop"
  )

setwd(tempdir())


#Visualization

#Unresolved Complaint Rate by Income Quintile-------------------------------------------------------------------


ggplot(unresolved_by_quintile, aes(x = factor(income_quintile),
                                   y = unresolved_rate)) +
  geom_col(fill = "black", alpha = 0.7) +
  geom_text(
    aes(label = percent(unresolved_rate, accuracy = 0.1)),
    vjust = -0.5
  ) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Unresolved Complaint Rate by Income Quintile",
    x = "Income Quintile (1 = Lowest Income, 5 = Highest Income)",
    y = "Unresolved Complaint Rate"
  ) +
  theme_minimal(base_size = 13)


#Unresolved Complaint Rates Over Time-----------------------------------------------------------------------------

yearly_rates <- complaints_q4 %>%
  group_by(year) %>%
  summarise(
    hpd_rate = sum(hpd_unres) / sum(hpd_n),
    dob_rate = sum(dob_unres) / sum(dob_n),
    .groups = "drop"
  ) %>%
  pivot_longer(-year, names_to = "source", values_to = "rate") %>%
  mutate(
    source = recode(
      source,
      hpd_rate = "HPD",
      dob_rate = "DOB"
    )
  )

ggplot(yearly_rates,
       aes(x = year, y = rate, color = source)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Unresolved Complaint Rates Over Time",
    x = "Year",
    y = "Unresolved Rate",
    color = "Agency"
  ) +
  theme_minimal(base_size = 14)
#---------------------------------------------------------------------------------------------------------------------------------------
#Logistic Model
#---------------------------------------------------------------------------------------------------------------------------------------
complaints_q4_logit <- complaints_q4 %>%
  filter(total_n >= 10) %>%   # key step
  mutate(
    total_resolved = total_n - total_unres
  )

logit_model_unres <- glm(
  cbind(total_unres, total_resolved) ~ log(median_income),
  family = binomial,
  data = complaints_q4_logit
)


# Generate predicted probabilities across income range
pred_unres <- ggpredict(
  logit_model_unres,
  terms = "median_income [all]"
)

# Convert x-axis back to numeric (ggeffects keeps it clean already)
pred_unres$median_income <- as.numeric(pred_unres$x)

ggplot(pred_unres, aes(x = median_income, y = predicted)) +
  geom_line(color = "#2C7FB8", size = 1.2) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high),
    fill = "#2C7FB8",
    alpha = 0.2
  ) +
  scale_x_continuous(
    labels = dollar_format(accuracy = 1),
    breaks = seq(20000, 100000, by = 20000)
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, NA)
  ) +
  labs(
    title = "Predicted Probability of Unresolved Complaints",
    subtitle = "Based on tract-level DOB + HPD complaints (minimum 10 complaints)",
    x = "Median Household Income",
    y = "Probability of Complaint Remaining Unresolved"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 13)
  )

#----------------------------------------------------------------------------------------------------------------------------------------
# Plot cumulative unresolved complaints (geom_sf)

unres_map <- complaints_q4 %>%
  group_by(tract_geoid) %>%
  summarise(
    total_unres_all = sum(total_unres, na.rm = TRUE),
    .groups = "drop"
  ) 

map_data <- tracts_sf %>%
  right_join(
    unres_map,
    by = c("GEOID" = "tract_geoid")
  )


map_data <- map_data %>%
  mutate(
    borough = case_when(
      COUNTYFP == "005" ~ "Bronx",
      COUNTYFP == "047" ~ "Brooklyn",
      COUNTYFP == "061" ~ "Manhattan",
      COUNTYFP == "081" ~ "Queens",
      COUNTYFP == "085" ~ "Staten Island",
      TRUE ~ NA_character_
    )
  )

boro_unres <- map_data %>%
  st_drop_geometry() %>%
  filter(!is.na(borough)) %>%   
  group_by(borough) %>%
  summarise(
    total_unresolved = sum(total_unres_all, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_unresolved))


ggplot(boro_unres, aes(x = reorder(borough, total_unresolved),
                       y = total_unresolved)) +
  geom_col(fill = "black", alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Unresolved Complaints by Borough (2018–2023)",
    subtitle = "HPD + DOB complaints aggregated across all census tracts",
    x = "Borough",
    y = "Total Unresolved Complaints"
  ) +
  theme_minimal(base_size = 14)



ggplot(map_data) +
  geom_sf(aes(fill = total_unres_all), color = NA) +
  scale_fill_viridis_c(
    option = "inferno",
    trans = "log1p",   
    na.value = "grey90",
    labels = comma
  ) +
  labs(
    title = "Cumulative Unresolved Housing Complaints by Census Tract",
    subtitle = "Log-scaled to reveal variation",
    fill = "Unresolved Complaints"
  ) +
  theme_minimal(base_size = 13)




-----------------------------------------------------------------------------------------------------------------------------------------
#Unresolved Complaints Over Time by Agency
  yearly_unresolved <- complaints_q4 %>%
  group_by(year) %>%
  summarise(
    HPD_unresolved = sum(hpd_unres, na.rm = TRUE),
    DOB_unresolved = sum(dob_unres, na.rm = TRUE),
    .groups = "drop"
  )

    yearly_unresolved_long <- yearly_unresolved %>%
  pivot_longer(
    cols = c(HPD_unresolved, DOB_unresolved),
    names_to = "agency",
    values_to = "unresolved_count"
  ) %>%
  mutate(
    agency = recode(
      agency,
      "HPD_unresolved" = "HPD",
      "DOB_unresolved" = "DOB"
    )
  )


ggplot(yearly_unresolved_long,
       aes(x = year, y = unresolved_count, color = agency)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = unique(yearly_unresolved_long$year)) +
  labs(
    title = "Unresolved Complaints Over Time by Agency",
    subtitle = "HPD vs DOB (All Census Tracts)",
    x = "Year",
    y = "Number of Unresolved Complaints",
    color = "Agency"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )































