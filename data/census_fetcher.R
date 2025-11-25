## ------- ADD URL/SOURCE OF DATA HERE -------

# https://walker-data.com/tidycensus/articles/basic-usage.html
# https://api.census.gov/data/key_signup.html

## ------- WHAT DATA IS THIS -------

# U.S. census data from tidycensus API
# Select variables of interest
# NOTE: will have to decide which (if any) data from tidycensus would be helpful due to limitations

#################### GRAB DATA ####################
# load library

library(tidyverse)
library(here)
library(tidycensus)
library(purrr)

# add codes to grab data here

# Original data extraction
census_api_key("dc64291726fbe5f0b1ba6cb4562565fe33baa574")

# Looking at variables to choose
View(load_variables(year = 2005, "acs1", cache = TRUE))

# ACS data only goes back to 2005
years <- c(2005:2019, 2021:2024)
years_mod <- c(2009:2019, 2021:2024) # for health insurance data

# Pulling median household income
income_data <- map(years, ~ get_acs(
  geography = "us",
  variables = "B19013_001",
  year = .x,
  survey = "acs1"
))

# Adding year
income_data <- map2(income_data, years, ~ mutate(.x, year = .y))

# Combining years
combined_income <- income_data %>%
  bind_rows() %>%
  mutate(var_name = case_when(
    variable == "B19013_001" ~ "med_income"
  )) %>%
  select(year, var_name, estimate)

# Poverty data
poverty_data <- map(years, ~get_acs(
  geography = "us",
  variables = c("B17001_001", "B17001_002"),
  year = .x,
  survey = "acs1"
))

# Adding year
poverty_data <- map2(poverty_data, years, ~ mutate(.x, year = .y))

# Combining years and wrangling to get poverty rate
combined_poverty <- poverty_data %>%
  bind_rows() %>%
  select(-moe) %>%
  group_by(year) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(poverty_rate = B17001_002/B17001_001) %>%
  select(year, poverty_rate) %>%
  pivot_longer(cols = poverty_rate, names_to = "var_name", values_to = "estimate") %>%
  select(year, var_name, estimate)

# Health insurance data (census only started collecting this in 2009)
health_data <- map(years_mod, ~get_acs(
  geography = "us",
  variable = c("B27010_001", "B27010_017", "B27010_033", "B27010_050", "B27010_066"),
  year = .x,
  survey = "acs1"
))

# Adding year
health_data <- map2(health_data, years_mod, ~ mutate(.x, year = .y))

# Combining years and wrangling to get poverty rate
combined_health <- health_data %>%
  bind_rows() %>%
  select(-moe) %>%
  group_by(year) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(prop_wo_health = (B27010_017+B27010_033+B27010_050+B27010_066)/B27010_001) %>%
  select(year, prop_wo_health) %>%
  pivot_longer(cols = prop_wo_health, names_to = "var_name", values_to = "estimate") %>%
  select(year, var_name, estimate)

# Merging income and poverty data
census_clean <- combined_income %>%
  bind_rows(combined_poverty, combined_health) %>%
  arrange(year)

# add codes to view data

glimpse(census_clean)

head(census_clean)



#################### SAVE DATASET INTO RDA ####################

save(census_clean, file = here("data/census_clean.rda"))

# naming: datasetname_clean
# rda file: datasetname_clean.rda
