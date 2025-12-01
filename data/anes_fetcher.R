## ------- ADD URL/SOURCE OF DATA HERE -------

# https://electionstudies.org/data-center/

## ------- WHAT DATA IS THIS -------

# American National Election Studies:
# All cross-section cases and variables for select questions from the 
# ANES Time Series studies conducted since 1948. 

#################### GRAB DATA ####################
# load library
library(tidyverse)
library(here)

# add codes to grab data here
anes2020 <- read_csv(here("data/anes_timeseries_cdf_csv_20220916.csv"))
anes2024 <- read_csv(here("data/anes_timeseries_2024_csv_20250808.csv"))

anes_data <- bind_rows(anes2020, anes2024)

glimpse(anes_data_year)

# add codes to view data

# summary of data by year with <30% missing data
anes_data_year <- anes_data |> 
  select(-matches("[A-Za-z]$")) |> 
  group_by(VCF0004) |> 
  summarize(across(everything(), ~ mean(.x, na.rm = TRUE))) |> 
  select(where(~ mean(is.na(.x) | is.nan(.x)) <= 0.30)) |> 
  filter(!is.na(VCF0004))


anes2020_missing <- anes2020 |> 
  select(VCF0004, VCF0706, VCF0731, VCF0824, VCF0878, VCF9008, VCF9205,
         VCF9234, VCF9235, VCF9238, VCF9275)
anes2024_missing <- anes2024 |> 
  select(V241038, V242025, V241177, V241379, V241238, V241236,
         V241250, V241251, V242325, V242209) |> 
  rename(VCF0706 = V241038,
         VCF0731 = V242025,
         VCF0824 = V241177,
         VCF0878 = V241379,
         VCF9008 = V241238,
         VCF9205 = V241236,
         VCF9234 = V241250,
         VCF9235 = V241251,
         VCF9238 = V242325,
         VCF9275 = V242209) |> 
  mutate(VCF0004 = 2024) |> 
  select(VCF0004, VCF0706, VCF0731, VCF0824, VCF0878, VCF9008, 
         VCF9205, VCF9234, VCF9235, VCF9238, VCF9275)

anes_combined <- bind_rows(anes2020_missing, anes2024_missing)
skimr::skim(anes_combined)

# V241038 VCF0706 Vote and Nonvote- President
# V242025 VCF0731 Respondent Discuss Politics with Family and Friends
# - V242025 POST: How many days in past week discussed politics with family or friends
# V241177 VCF0824 If Compelled to Choose Liberal or Conservative
# -  V241177 PRE: 7pt scale liberal-conservative self-placement
# V241379 VCF0878 Should Gays/Lesbians Be Able to Adopt Children
# V241238 VCF9008 Which Party Would Best Handle Pollution and Protecting Environment
# V241236 VCF9205 Which party would do a better job handling the nation’s economy
# V241250 VCF9234 Abortion issue placement for Democratic Presidential candidate
# V241251 VCF9235 Abortion issue placement for Republican Presidential candidate
# V242325 VCF9238 Should the government make it more difficult or easier to buy a gun, or should
# the rules stay the same as they are now
# V242209 VCF9275 In American politics, do blacks have too much, about the right amount of, or too
# little influence
# - V242209 POST: How important that more blacks get elected to political office

# codebook

anes_codebook <- tribble(
  ~variable,   ~label,
  "year",  "Study Year",
  "vote_status",  "Vote and Nonvote- President",
  "politics_",  "Respondent Discuss Politics with Family and Friends",
  "choose_",  "If Compelled to Choose Liberal or Conservative",
  "lg_adopt",  "Should Gays/Lesbians Be Able to Adopt Children",
  "environment_",  "Which Party Would Best Handle Pollution and Protecting Environment",
  "economy_",  "Which party would do a better job handling the nation’s economy",
  "abortion_dem",  "Abortion issue placement for Democratic Presidential candidate",
  "abortion_rep",  "Abortion issue placement for Republican Presidential candidate",
  "gun_",  "Should the government make it more difficult or easier to buy a gun, or should the rules stay the same as they are now",
  "black_",  "In American politics, do blacks have too much, about the right amount of, or too little influence"
)

anes_yearly <- anes_combined |> 
  # select(where(~ mean(is.na(.x) | is.nan(.x)) <= 0.80)) |> 
  filter(!is.na(year)) |> 
  rename(year = VCF0004,
         vote_status = VCF0706,
         politics_ = VCF0731,
         choose_ = VCF0824,
         lg_adopt = VCF0878,
         environment_ = VCF9008,
         economy_ = VCF9205,
         abortion_dem = VCF9234,
         abortion_rep = VCF9235,
         gun_ = VCF9238,
         black_ = VCF9275) |> 
  group_by(year) |> 
  summarize(across(everything(), ~ mean(.x, na.rm = TRUE)))

anes_clean <- anes_yearly |> 
  pivot_longer(cols = -year, names_to = "variable", values_to = "value") |> 
  filter(!is.na(value))

#################### SAVE DATASET INTO RDA ####################

save(anes_clean, anes_codebook, file = here("data/anes_clean.rda"))

# naming: datasetname_clean
# rda file: datasetname_clean.rda
