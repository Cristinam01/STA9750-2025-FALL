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
if(!require("tidyverse")) install.packages("tidyverse")
library(readr)
library(dplyr)

GLOBAL_TOP_10 <- read_tsv(GLOBAL_TOP_10_FILENAME)

str(GLOBAL_TOP_10)
glimpse(GLOBAL_TOP_10)

GLOBAL_TOP_10 <- GLOBAL_TOP_10 |>
  mutate(season_title = if_else(season_title=="N/A",NA_character_,season_title))
glimpse(GLOBAL_TOP_10)
if(!require("tidyverse")) install.packages("tidyverse")
library(readr)
library(dplyr)

COUNTRY_TOP_10 <- read_tsv(COUNTRY_TOP_10_FILENAME)
COUNTRY_TOP_10 <- COUNTRY_TOP_10 |>
  mutate(season_title = if_else(season_title=="N/A",NA_character_,season_title))

str(COUNTRY_TOP_10)
format_titles <- function(df){
  colnames(df) <- str_replace_all(colnames(df), "_", " ") |> str_to_title()
  df
}
GLOBAL_TOP_10 |> 
  mutate('runtime_(minutes)' = round(60 * runtime)) |>
  select(-season_title, 
         -runtime) |>
  format_titles() |>
  head(n=20) |>
  datatable(options=list(searching=FALSE, info=FALSE)) |>
  formatRound(c('Weekly Hours Viewed', 'Weekly Views'))
COUNTRY_TOP_10 |>
  format_titles() |>
  head(n=20) |>
  datatable(options=list(searching=FALSE, info=FALSE))
colnames(GLOBAL_TOP_10)
colnames(COUNTRY_TOP_10)
COUNTRY_TOP_10 |>
  summarize(num_countries = n_distinct(country_name))
COUNTRY_TOP_10 |>
  distinct(country_name) |>
  arrange(country_name) |>
  datatable(options = list(
    searching = TRUE,  
    paging = TRUE,     
    pageLength = 20,   
    info = FALSE
  ))
# 2. Which non-English-language film has spent the most cumulative weeks in the global top 10? How many weeks did it spend?
GLOBAL_TOP_10 |>
  filter(category == "Films (Non-English)") |>
  slice_max(cumulative_weeks_in_top_10, n = 1) |>
  select(show_title, cumulative_weeks_in_top_10)

#3.What is the longest film (English or non-English) to have ever appeared in the Netflix global Top 10? How long is it in minutes?
GLOBAL_TOP_10 |>
  slice_max(runtime, n = 1, with_ties = TRUE) |>
  transmute(show_title, runtime_minutes = round(60 * runtime))
#4.For each of the four categories, what program has the most total hours of global viewership?
GLOBAL_TOP_10 |>
  group_by(category) |>
  slice_max(weekly_hours_viewed, n=1) |>
  select(show_title,weekly_hours_viewed) |>
  arrange(desc(weekly_hours_viewed))
#5. Which TV show had the longest run in a country’s Top 10? How long was this run and in what country did it occur?
COUNTRY_TOP_10 |>
  slice_max(cumulative_weeks_in_top_10, n=1, with_ties = TRUE) |>
  select(show_title,cumulative_weeks_in_top_10,country_name)
#6.Netflix provides over 200 weeks of service history for all but one country in our data set. Which country is this and when did Netflix cease operations in that country?
COUNTRY_TOP_10 |>
  group_by(country_name) |>
  summarize(
    weeks_of_service_by_country = n_distinct(week),
    last_week = max(week),
    .groups = "drop"
  ) |>
  slice_min(weeks_of_service_by_country, n = 1, with_ties = FALSE)
# 7. What is the total viewership of the TV show Squid Game? Note that there are three seasons total and we are looking for the total number of hours watched across all seasons.
GLOBAL_TOP_10 |>
  filter(str_detect(show_title, "Squid Game")) |>
  summarize(total_hours_view_Squid_Game = sum(weekly_hours_viewed, na.rm = TRUE))
GLOBAL_TOP_10 |>
  group_by(season_title) |>
  filter(str_detect(show_title, "Squid Game")) |>
  summarize(total_hours_view_Squid_Game = sum(weekly_hours_viewed, na.rm = TRUE))
#8.The movie Red Notice has a runtime of 1 hour and 58 minutes. Approximately how many views did it receive in 2021? Note that Netflix does not provide the weekly_views values that far back in the past, but you can compute it yourself using the total view time and the runtime.

GLOBAL_TOP_10 |>
  filter( show_title == "Red Notice", year(week)==2021) |>
  summarize(total_hours_viewed_2021= sum(weekly_hours_viewed, na.rm = TRUE) ) |>
  mutate(Red_Notice_views_2021= total_hours_viewed_2021/1.9667 )
#9.How many Films reached Number 1 in the US but did not originally debut there? That is, find films that first appeared on the Top 10 chart at, e.g., Number 4 but then became more popular and eventually hit Number 1? What is the most recent film to pull this off?

COUNTRY_TOP_10 |>
  filter(country_name == "United States") |>
  count(category, sort = TRUE)
COUNTRY_TOP_10 |>
  filter(country_name == "United States",
         grepl("^Films", category)) |>
  summarize(any_hit1 = any(weekly_rank == 1, na.rm = TRUE),
            min_rank = min(weekly_rank, na.rm = TRUE),
            max_rank = max(weekly_rank, na.rm = TRUE))
str(COUNTRY_TOP_10$weekly_rank)
str(COUNTRY_TOP_10$week)
COUNTRY_TOP_10 |>
  filter(country_name == "United States",
         grepl("^Films", category)) |>
  distinct(show_title) |>
  tally(name = "num_films_usa")
library(dplyr)

us_films <- COUNTRY_TOP_10 |>
  filter(country_name == "United States",
         grepl("^Films", category),
         !is.na(week)) |>
  mutate(weekly_rank = as.integer(weekly_rank)) |>
  arrange(show_title, week)

# debut por show (semana y rank)
debut_us <- us_films |>
  group_by(show_title) |>
  slice_min(week, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(show_title,
            debut_week = week,
            debut_rank = weekly_rank)

# #1 alguna vez y última semana #1
hit1_us <- us_films |>
  group_by(show_title) |>
  summarize(
    ever_hit_1     = any(weekly_rank == 1, na.rm = TRUE),
    last_hit1_week = if (any(weekly_rank == 1, na.rm = TRUE))
      max(week[weekly_rank == 1], na.rm = TRUE)
    else as.Date(NA),
    .groups = "drop"
  )

# combinar y aplicar condición: NO debutó #1 pero luego fue #1
res_us_films <- debut_us |>
  inner_join(hit1_us, by = "show_title") |>
  filter(debut_rank > 1, ever_hit_1)

# resultados
total_films_reached1_after_debut_US <- nrow(res_us_films)
most_recent_film_reached1_after_debut_US <- res_us_films |>
  slice_max(order_by = last_hit1_week, n = 1, with_ties = FALSE)

total_films_reached1_after_debut_US
most_recent_film_reached1_after_debut_US

# 10. Which TV show/season hit the top 10 in the most countries in its debut week? In how many countries did it chart?
debut_week_by_country <- COUNTRY_TOP_10 |>
  filter(str_detect(category, "^TV")) |>
  group_by(show_title, season_title, country_name) |>
  slice_min(week, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(show_title, season_title, country_name,
            debut_week = week,
            debut_rank = weekly_rank)

total_countries_debut <- debut_week_by_country |>
  group_by(show_title, season_title, debut_week) |>
  summarize(num_countries = n_distinct(country_name), .groups = "drop")

country_most_hit_debut <- total_countries_debut |>
  slice_max(order_by = num_countries, n = 1, with_ties = FALSE)

country_most_hit_debut

#Press Release 1: Upcoming Season of Stranger Things
#total viewers by season
stranger_views_hours <- GLOBAL_TOP_10 |>
  filter(show_title == "Stranger Things") |>
  group_by(season_title) |>
  summarise(total_hours = sum(weekly_hours_viewed, na.rm = TRUE), .groups="drop")
stranger_views
durations <- c(0.93, 0.83, 0.83, 1.77)
stranger_views <- stranger_views |>
  mutate(duration_hours = durations,
         viewers_est = total_hours / duration_hours)
stranger_views
#How many viewers 
length_of_popularity <- GLOBAL_TOP_10 |>
  filter(show_title == "Stranger Things") |>
  summarize(total_weeks_top10 = n_distinct(week))
length_of_popularity
# Stranger things top 10 countries
stranger_season_countries <- COUNTRY_TOP_10 |>
  filter(show_title == "Stranger Things") |>
  group_by(season_title) |>
  summarize(total_countries_seasons = n_distinct(country_name, na.rm = TRUE))
stranger_season_countries
# total cumulative weeks 
stranger_cumulative_weeks <- COUNTRY_TOP_10 |>
  filter(show_title == "Stranger Things") |>
  group_by(season_title) |>
  summarize(total_cumulative_weeks_top_10= sum(cumulative_weeks_in_top_10, na.rm = TRUE))
stranger_cumulative_weeks
#
stranger_top_5 <- COUNTRY_TOP_10 |>
  filter(show_title == "Stranger Things") |>
  group_by(country_name) |>
  summarise(total_weeks_top10 = n_distinct(week), .groups = "drop") |>
  slice_max(total_weeks_top10, n = 5)
stranger_top_5

COUNTRY_TOP_10 |>
  filter(show_title == "Stranger Things", country_name == "Ukraine") |>
  count(country_name, season_title, sort = TRUE)
COUNTRY_TOP_10 |>
  filter(show_title == "Stranger Things") |>
  group_by(country_name, season_title) |>
  summarise(weeks_in_top10 = n_distinct(week), .groups="drop") |>
  arrange(desc(weeks_in_top10)) |>
  slice_max(weeks_in_top10, n = 5)

# comparison us vs ukraine
st_us_ukr <- COUNTRY_TOP_10 |>
  filter(show_title == "Stranger Things",
         country_name %in% c("United States", "Ukraine")) |>
  group_by(country_name, season_title) |>
  summarise(weeks_in_top10 = n_distinct(week), .groups = "drop")

st_us_ukr_wide <- st_us_ukr |>
  pivot_wider(names_from = country_name, values_from = weeks_in_top10)

st_us_ukr_wide
#top 10 series watch
top_10_series_view <- GLOBAL_TOP_10 |>
  group_by(show_title) |>
  summarise(total_hours = sum(weekly_hours_viewed, na.rm = TRUE), .groups="drop") |>
  slice_max(total_hours, n = 10)

top_10_series_view
#compare with Wednesday


st_vs_wed <- GLOBAL_TOP_10 |>
  filter(show_title %in% c("Stranger Things", "Wednesday")) |>
  group_by(show_title) |>
  summarise(
    total_hours = sum(weekly_hours_viewed, na.rm = TRUE),
    total_weeks = n_distinct(week, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(
    COUNTRY_TOP_10 |>
      filter(show_title %in% c("Stranger Things", "Wednesday")) |>
      group_by(show_title) |>
      summarise(total_countries = n_distinct(country_name, na.rm = TRUE),
                .groups = "drop"),
    by = "show_title"
  )

st_vs_wed
#Press Release 2: Commercial Success in India

india_noneng <- COUNTRY_TOP_10 |>
  filter(
    country_name == "India",
    category %in% c("Films", "TV")) |>
  select(show_title, week)

us_any <- COUNTRY_TOP_10 |>
  filter(country_name == "United States") |>
  distinct(show_title)

india_not_us_titles <- india_noneng |>
  distinct(show_title) |>
  anti_join(us_any, by = "show_title")

india_not_us_summary <- india_not_us_titles |>
  left_join(india_noneng, by = "show_title") |>
  group_by(show_title) |>
  summarise(
    total_weeks_top10_india = n_distinct(week),   
    .groups = "drop"
  ) |>
slice_max(total_weeks_top10_india, n=10)

india_not_us_summary


india_views <- india_not_us_summary |> 
  left_join(
    GLOBAL_TOP_10 |> 
      group_by(show_title) |> 
      summarise(
        total_hours_views_india = sum(weekly_hours_viewed, na.rm = TRUE),
        total_views_india = sum(weekly_views, na.rm = TRUE),
        .groups = "drop"
      ), 
    by = c("show_title")
  )
india_views

india_trend <- india_not_us_titles |>
  left_join(GLOBAL_TOP_10, by = "show_title") |>
  mutate(year = year(week), month=month(week, label=TRUE)) |>
  filter(year %in% c(2021, 2022, 2023, 2024)) |>
  group_by(year, month) |>
  summarise(
    total_hours_views_india = sum(weekly_hours_viewed, na.rm = TRUE),
    total_titles = n_distinct(show_title),
    .groups = "drop"
  ) |>
  arrange(year, month) |>
  mutate(
    increase_by_month = (total_hours_views_india - lag(total_hours_views_india)) /
      lag(total_hours_views_india)*100)
india_trend
# COMPARE MONTH TO MONTH BY YEAR
ggplot(india_trend, aes(x = month, y = total_hours_views_india, fill = factor(year))) +
  geom_col(position = "dodge") +
  labs(
    title = "Monthly Netflix Hours Viewed in India",
    x = "Month",
    y = "Total Hours Viewed",
    fill = "Year"
  ) +
  theme_minimal()

library(ggplot2)

india_trend_long <- india_trend |>
  mutate(date = as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%b-%d"))

library(scales)
# TENDECY PLOT BY TOTAL HOURS VIEWD
ggplot(india_trend_long, aes(x = date, y = total_hours_views_india)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkred", size = 2) +
  scale_y_continuous(labels = comma) +   # 👈 formato con comas: 10,000,000
  labs(
    title = "Netflix Hours Viewed in India (Monthly, 2021–2024)",
    x = "Month",
    y = "Total Hours Viewed"
  ) +
  theme_minimal()

library(scales)
# TENDECY PLOT BY % OF GROWTH/DECLINE
ggplot(india_trend_long, aes(x = date, y = increase_by_month)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") + 
  geom_line(color = "steelblue", size = 1) +
  scale_y_continuous(labels = percent_format(scale = 1)) +          
  labs(
    title = "Month-to-Month % Change in Netflix Hours Viewed in India (2021–2024)",
    x = "Month",
    y = "Growth / Decline (%)",
    color = "Trend"
  ) +
  theme_minimal()




ggplot(india_trend, aes(x = year, y = total_titles)) +
  geom_col(fill = "forestgreen") +
  geom_text(aes(label = total_titles), vjust = -0.5) +
  labs(
    title = "Unique Titles in Top 10 (India)",
    x = "Year",
    y = "Number of Titles"
  ) +
  theme_minimal(base_size = 14)

#Trend in India shows in general

india_titles <- COUNTRY_TOP_10 |>
  filter(country_name == "India") |>
  distinct(show_title)



india_views_trend <- GLOBAL_TOP_10 |>
  semi_join(india_titles, by = "show_title") |>
  mutate(year = year(week), month = month(week, label = TRUE))  |>
  filter(year %in% c(2021, 2022, 2023, 2024)) |>
  group_by(year, month) |>
  summarise(
    total_hours_views_by_year = sum(weekly_hours_viewed, na.rm = TRUE),
    total_titles              = n_distinct(show_title),
    .groups = "drop"
  ) |>
  arrange(year, month) |>
  mutate(
    increase_by_month = (total_hours_views_by_year - lag(total_hours_views_by_year)) /
      lag(total_hours_views_by_year)*100
  )

india_views_trend

india_trend_long_general <- india_views_trend |>
  mutate(date = as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%b-%d"))

# HOLE INDIA TENDECY PLOT BY TOTAL HOURS VIEWD
ggplot(india_trend_long_general, aes(x = date, y = total_hours_views_by_year)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkred", size = 2) +
  scale_y_continuous(labels = comma) +   
  labs(
    title = "Netflix Hours Viewed in India (Monthly, 2021–2024)",
    x = "Month",
    y = "Total Hours Viewed"
  ) +
  theme_minimal()
# HOLE INDIA TENDECY PLOT BY % OF GROWTH/DECLINE
ggplot(india_trend_long_general, aes(x = date, y = increase_by_month)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") + 
  geom_line(color = "steelblue", size = 1) +
  scale_y_continuous(labels = percent_format(scale = 1)) +            
  labs(
    title = "Month-to-Month % Change in Netflix Hours Viewed in India (2021–2024)",
    x = "Month",
    y = "Growth / Decline (%)",
    color = "Trend"
  ) +
  theme_minimal()
# TENDECY PLOT BY % OF GROWTH/DECLINE
ggplot(india_trend_long, aes(x = date, y = increase_by_month)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") + 
  geom_line(color = "steelblue", size = 1) +
  scale_y_continuous(labels = percent_format(scale = 1)) +          
  labs(
    title = "Month-to-Month % Change in Netflix Hours Viewed in India (2021–2024)",
    x = "Month",
    y = "Growth / Decline (%)",
    color = "Trend"
  ) +
  theme_minimal()

library(ggplot2)

ggplot(india_trend_long_general, aes(x = year, y = total_hours_views_by_year)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "darkred", size = 3) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Total Viewing Hours of Netflix in India (2021–2024)",
    x = "Year",
    y = "Total Hours Viewed"
  ) +
  theme_minimal(base_size = 14)
ggplot(india_views_trend, aes(x = year, y = increase_by_year * 100)) +
  geom_col(fill = "tomato") +
  geom_text(aes(label = round(increase_by_year*100, 1)), vjust = -0.5) +
  labs(
    title = "Year-over-Year Growth in Viewing Hours (India)",
    x = "Year",
    y = "Growth (%)"
  ) +
  theme_minimal(base_size = 14)
ggplot(india_views_trend, aes(x = year, y = total_titles)) +
  geom_col(fill = "forestgreen") +
  geom_text(aes(label = total_titles), vjust = -0.5) +
  labs(
    title = "Unique Titles in Top 10 (India)",
    x = "Year",
    y = "Number of Titles"
  ) +
  theme_minimal(base_size = 14)

COUNTRY_TOP_10 |>
  filter(country_name =="India") |>
  group_by(category) |>
  summarize( total_categories = n_distinct(category))
# Analysis Ecuador
ecuador_views <- COUNTRY_TOP_10 |>
  filter(country_name == "Ecuador") |>
  inner_join(GLOBAL_TOP_10, by = c("show_title", "week")) |>
  group_by(show_title) |>
  summarise(total_hours_views = sum(weekly_hours_viewed, na.rm = TRUE), .groups = "drop") |>
  mutate(
    increase_by_year = (total_hours_views - lag(total_hours_views)) /
      lag(total_hours_views)*100
  )

ecuador_views

ecuador_noneng <- COUNTRY_TOP_10 |>
  filter(
    country_name == "Ecuador",
    category %in% c("Films", "TV")) |>
  select(show_title, week)

us_any <- COUNTRY_TOP_10 |>
  filter(country_name == "United States") |>
  distinct(show_title)

ecuador_not_us_titles <- ecuador_noneng |>
  distinct(show_title) |>
  anti_join(us_any, by = "show_title")

ecuador_not_us_summary <- ecuador_not_us_titles |>
  left_join(ecuador_noneng, by = "show_title") |>
  group_by(show_title) |>
  summarise(
    total_weeks_top10_ecuador = n_distinct(week),   
    .groups = "drop"
  ) |>
  slice_max(total_weeks_top10_ecuador, n=10)

ecuador_not_us_summary

ecuador_trend <- ecuador_not_us_titles |>
  left_join(GLOBAL_TOP_10, by = "show_title") |>
  mutate(year = year(week), month=month(week, label=TRUE)) |>
  filter(year %in% c(2021, 2022, 2023, 2024)) |>
  group_by(year, month) |>
  summarise(
    total_hours_views_ecuador = sum(weekly_hours_viewed, na.rm = TRUE),
    total_titles = n_distinct(show_title),
    .groups = "drop"
  ) |>
  arrange(year, month) |>
  mutate(
    increase_by_year = (total_hours_views_ecuador - lag(total_hours_views_ecuador)) /
      lag(total_hours_views_ecuador)*100)
ecuador_trend
ecuador_trend_long_general <- ecuador_trend |>
  mutate(date = as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%b-%d"))

#   ECUADOR TENDECY PLOT BY TOTAL HOURS VIEWD
ggplot(ecuador_trend_long_general, aes(x = date, y = total_hours_views_ecuador)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkred", size = 2) +
  scale_y_continuous(labels = comma) +   
  labs(
    title = "Netflix Hours Viewed in Ecuador (Monthly, 2021–2024)",
    x = "Month",
    y = "Total Hours Viewed"
  ) +
  theme_minimal()

# Press Release 3: Money Heist
library(dplyr)
#How many countries top 10
money_heist_countries <- COUNTRY_TOP_10 |>
  filter(show_title == "Money Heist") |>
  summarise(total_countries = n_distinct(country_name))

money_heist_countries
#top 10 by country
money_heist <- COUNTRY_TOP_10 |>
  filter(show_title == "Money Heist") |>
  group_by(country_name) |>
  summarise( total_weeks_top10= n_distinct(week)) |>
  slice_max(total_weeks_top10, n=10)
money_heist
money_heist <- COUNTRY_TOP_10 |>
  filter(show_title == "Money Heist") |>
  select(country_name, week, weekly_rank)
money_heist_views <- money_heist |>
  inner_join(
    GLOBAL_TOP_10 |> 
      filter(show_title== "Money Heist") |>
      select(week, weekly_hours_viewed,weekly_views),
    by ="week")
money_heist_summary <- money_heist_views |>
  group_by(country_name) |>
  summarise(  total_hours_viewed = sum(weekly_views, na.rm = TRUE),
              total_weeks_top_10 = n_distinct(week),
              .groups = "drop") |>
  slice_max(total_weeks_top_10, n=10)
money_heist_summary
# TOP 10 weeks by country
ggplot(money_heist |> 
         mutate(country_name = fct_reorder(country_name, total_weeks_top10)),
       aes(x = country_name, y = total_weeks_top10)) +
  geom_col(fill="red") +
  geom_text(aes(label = total_weeks_top10),
            hjust = -0.1, size = 3.5) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, .10))) +
  labs(
    title = "Money Heist — Top 10 Countries by Week in Top 10",
    x = NULL, y = "Weeks in Top 10"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.margin = margin(10, 30, 10, 10))


ggplot(money_heist |> 
         mutate(country_name = fct_reorder(country_name, total_weeks_top10)),
       aes(x = country_name, y = total_weeks_top10)) +
  geom_col() +
  geom_text(aes(label = total_weeks_top10),
            hjust = -0.1, size = 3.5) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, .10))) +
  labs(
    title = "Money Heist — Top 10 Countries by Week in Top 10",
    x = NULL, y = "Weeks in Top 10"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.margin = margin(10, 30, 10, 10))

spain_views <- COUNTRY_TOP_10 |>
  filter(show_title== "Money Heist", country_name =="Ecuador") |>
  summarise(total_weeks_top_10 = n_distinct(week))
spain_views 

---
  #old version
  ---
  title: "Exploring the Most Popular Programming on Netflix"
author: "Maria Moreno"
format:
  html:
  toc: true
toc-depth: 3
number-sections: true
code-tools: true
title-block-banner: true # nice hero banner
title-block-banner-image: images/Netflix.jpg.webp
---
  ```{r}
library(gt)
```


## {background-image=Netflix.jpg.webp}
### INTRODUCTION
This project will show the following aspects:
  
  1. Cleaning data and visualization  
2. Exploring questions/answers  
3. Press Release 1: Upcoming Season of *Stranger Things*  
  4. Press Release 2: India Rises as a Streaming Powerhouse: Netflix Shows Break Records in Viewership  
5. Press Release 3: Money Heist Achieves Unprecedented Global Popularity Across Multiple Markets 

```{r}
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
if(!require("tidyverse")) install.packages("tidyverse")
library(readr)
library(dplyr)

GLOBAL_TOP_10 <- read_tsv(GLOBAL_TOP_10_FILENAME)
```

```{r}
str(GLOBAL_TOP_10)
```

```{r}
glimpse(GLOBAL_TOP_10)
```
```{r}
GLOBAL_TOP_10 <- GLOBAL_TOP_10 |>
  mutate(season_title = if_else(season_title=="N/A",NA_character_,season_title))
```

```{r}
str(GLOBAL_TOP_10)
```
```{r}
glimpse(GLOBAL_TOP_10)
```

```{r}
if(!require("tidyverse")) install.packages("tidyverse")
library(readr)
library(dplyr)

COUNTRY_TOP_10 <- read_tsv(COUNTRY_TOP_10_FILENAME)
COUNTRY_TOP_10 <- COUNTRY_TOP_10 |>
  mutate(season_title = if_else(season_title=="N/A",NA_character_,season_title))

str(COUNTRY_TOP_10)
glimpse(COUNTRY_TOP_10)
```
## Visualization of Global Top 10 
```{r}
library(DT)
GLOBAL_TOP_10 |> 
  head(n=20) |>
  datatable(options=list(scrollY = "400px",paging = TRUE,searching=FALSE, info=FALSE))
library(stringr)
format_titles <- function(df){
  colnames(df) <- str_replace_all(colnames(df), "_", " ") |> str_to_title()
  df
}
```
## Clean Titles and Convert hours to minutes and right numbers Global Top 10
```{r}
GLOBAL_TOP_10 |> 
  mutate('runtime_(minutes)' = round(60 * runtime)) |>
  select(-season_title, 
         -runtime) |>
  format_titles() |>
  head(n=20) |>
  datatable(options=list(searching=FALSE, info=FALSE)) |>
  formatRound(c('Weekly Hours Viewed', 'Weekly Views'))
```
## Visualization of Country Top 10 and Clean Titles
```{r}
library(DT)
library(stringr)
format_titles_country <- function(df) {
  colnames(df) <- str_replace_all(colnames(df), "_", " ") |> str_to_title()
  df
}
COUNTRY_TOP_10 <- format_titles_country(COUNTRY_TOP_10)
COUNTRY_TOP_10 |>
  head(20) |>
  datatable(options = list(searching = FALSE, info = FALSE))
```
## Exploratory Questions
#### How many different countries does Netflix operate in? 
\ Netflix has a total of countries: 
  ``` {r}
COUNTRY_TOP_10 |>
  summarize(num_countries = n_distinct(`Country Name`))
```
### A list of countries that Netflix has is bellow: 
```{r}
COUNTRY_TOP_10 |>
  summarize(num_countries = n_distinct(`Country Name`))
COUNTRY_TOP_10 |>
  distinct(`Country Name`) |>
  arrange(`Country Name`) |>
  datatable(options = list(
    searching = TRUE,  
    paging = TRUE,     
    pageLength = 20,   
    info = FALSE
  ))
```


  
  
