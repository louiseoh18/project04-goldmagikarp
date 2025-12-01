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

# add codes to view data

glimpse(anes_data)


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

# missing data
naniar::miss_var_summary(anes2020) |> DT::datatable()
naniar::miss_var_summary(anes2024) |> DT::datatable()

naniar::miss_var_summary(anes2024) |> 
  filter(pct_miss < 20) |> 
  pull(variable)

var_2020 <- naniar::miss_var_summary(anes2020) |> 
  filter(pct_miss < 20) |> 
  pull(variable)


# codebook

anes_codebook <- tribble(
  ~variable,   ~label,
  "year",  "Study Year",
  "VCF0706",  "Vote and Nonvote- President",
  "VCF0731",  "Respondent Discuss Politics with Family and Friends",
  "VCF0824",  "If Compelled to Choose Liberal or Conservative",
  "VCF0878",  "Should Gays/Lesbians Be Able to Adopt Children",
  "VCF9008",  "Which Party Would Best Handle Pollution and Protecting Environment",
  "VCF9205",  "Which party would do a better job handling the nation’s economy",
  "VCF9234",  "Abortion issue placement for Democratic Presidential candidate",
  "VCF9235",  "Abortion issue placement for Republican Presidential candidate",
  "VCF9238",  "Should the government make it more difficult or easier to buy a gun, or should the rules stay the same as they are now",
  "VCF9275",  "In American politics, do blacks have too much, about the right amount of, or too little influence"
)

anes_combined <- anes_combined |> 
  rename(year = VCF0004)

#################### SAVE DATASET INTO RDA ####################

save(anes_combined, anes_codebook, file = here("data/anes_clean.rda"))

# naming: datasetname_clean
# rda file: datasetname_clean.rda
