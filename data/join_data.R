
library(tidyverse)
library(zoo)
library(janitor)
library(here)

load("./data/anes_clean.rda") #anes_data and anes_codebook
load("./data/bls_data_clean.rda") #bls_data_clean
load("./data/census_clean.rda") #census_clean
load("./data/monthly_approval_clean.rda") #monthly_approval_clean
load("./data/BEA_economic_data.rda") #final_economic_data

bls_wide <- bls_data_clean %>%
  filter(date >= "1941-07-01") %>% 
  select(date, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(consumer_p_index = round(consumer_p_index, 2))

data_wide <- monthly_approval_clean %>%
  full_join(bls_wide, by = "date") %>%
  arrange(desc(date)) %>%
  mutate(
    President = ifelse(
      zoo::na.locf(President, na.rm = FALSE) == zoo::na.locf(President, na.rm = FALSE, fromLast = TRUE),
      zoo::na.locf(President, na.rm = FALSE),
      NA
    ),
    President = case_when(
      date >= "1941-07-01" & date <= "1945-04-01" ~ "Franklin D. Roosevelt",
      date >= "1945-05-01" & date <= "1953-01-01" ~ "Harry Truman",
      date == "1977-01-01" ~ "Jimmy Carter",
      TRUE ~ President
    )
  )

missing_rows <- data_wide %>%
  filter(date == "1945-04-01" | date == "1953-01-01" | date == "1977-01-01") %>%
  mutate(President = case_when(
    President == "Jimmy Carter" ~ "Gerald Ford",
    President == "Harry Truman" ~ "Dwight D. Eisenhower",
    President == "Franklin D. Roosevelt" ~ "Harry Truman"
  ))

data_wide <- data_wide %>%
  add_row(missing_rows) %>%
  arrange(desc(date)) %>%
  janitor::clean_names() 

final_combined_data <- data_wide %>%
  full_join(final_economic_data, by = "date") %>%
  arrange(date)


final_combined_data <- final_combined_data %>%
  fill(
    Real_GDP, 
    Disposable_Income, 
    Corporate_Profits, 
    Healthcare_Price_Index,
    Gov_Spending,
    Personal_Saving_Rate,
    .direction = "down"
  ) %>%
  filter(!(is.na(unemployment_rate) & is.na(approval_rating) & is.na(Real_GDP))) %>%
  
  group_by(president) %>%
  mutate(
    approval_rating = na.approx(approval_rating, na.rm = FALSE),
    disapproval_rating = na.approx(disapproval_rating, na.rm = FALSE),
    unsure_rating = na.approx(unsure_rating, na.rm = FALSE)
  ) %>%
  ungroup() %>%
  
  fill(
    Real_GDP, Disposable_Income, Corporate_Profits, Healthcare_Price_Index,
    Gov_Spending, Personal_Saving_Rate,
    .direction = "up"
  )

final_combined_data <- final_combined_data %>%
  mutate(Party = case_when(
    president %in% c("Franklin D. Roosevelt", "Harry Truman", 
                     "John F. Kennedy", "Lyndon B. Johnson", 
                     "Jimmy Carter", "Bill Clinton", "Barack Obama", 
                     "Joe Biden") ~ "Democrat",
    TRUE ~ "Republican"
  ),
  Party = factor(Party)
  )

print(colnames(final_combined_data))

visdat::vis_miss(final_combined_data, warn_large_data = FALSE)

save(final_combined_data, file = here("data/complete_data.rda"))
