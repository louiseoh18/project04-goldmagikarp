library(tidyverse)
library(zoo)
library(janitor)
library(here)

load(here("./data/anes_clean.rda")) #anes_data and anes_codebook
load(here("./data/bls_data_clean.rda")) #bls_data_clean
load(here("./data/census_clean.rda")) #census_clean
load(here("./data/monthly_approval_clean.rda")) #monthly_approval_clean
load(here("./data/BEA_economic_data.rda")) #final_economic_data

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
      zoo::na.locf(President, na.rm = FALSE) ==
        zoo::na.locf(President, na.rm = FALSE, fromLast = TRUE),
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
  mutate(
    President = case_when(
      President == "Jimmy Carter" ~ "Gerald Ford",
      President == "Harry Truman" ~ "Dwight D. Eisenhower",
      President == "Franklin D. Roosevelt" ~ "Harry Truman"
    )
  )

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
  filter(
    !(is.na(unemployment_rate) & is.na(approval_rating) & is.na(Real_GDP))
  ) %>%

  group_by(president) %>%
  mutate(
    approval_rating = na.approx(approval_rating, na.rm = FALSE),
    disapproval_rating = na.approx(disapproval_rating, na.rm = FALSE),
    unsure_rating = na.approx(unsure_rating, na.rm = FALSE)
  ) %>%
  ungroup() %>%

  fill(
    Real_GDP,
    Disposable_Income,
    Corporate_Profits,
    Healthcare_Price_Index,
    Gov_Spending,
    Personal_Saving_Rate,
    .direction = "up"
  )

pres_starts <- final_combined_data %>%
  group_by(president) %>%
  summarize(Start_Date = min(date), .groups = "drop")

final_combined_data <- final_combined_data %>%
  left_join(pres_starts, by = "president") %>%
  mutate(
    Party = case_when(
      president %in%
        c(
          "Franklin D. Roosevelt",
          "Harry Truman",
          "John F. Kennedy",
          "Lyndon B. Johnson",
          "Jimmy Carter",
          "Bill Clinton",
          "Barack Obama",
          "Joe Biden"
        ) ~ "Democrat",
      TRUE ~ "Republican"
    ),
    Party = factor(Party),
    Months_in_Office = interval(Start_Date, date) %/% months(1)
  )

# adding important events
final_combined_data <-
  final_combined_data |>
  mutate(
    important_events = case_when(
      date == ym("1945-05") ~ "End of WW2 in Europe",
      date == ym("1945-08") ~ "Atomic bombs",
      date == ym("1945-09") ~ "End of WW2",
      date == ym("1947-03") ~ "Start: Cold War",
      date == ym("1950-06") ~ "Start: Korean War",
      date == ym("1950-06") ~ "End: Korean War",
      date == ym("1955-11") ~ "Start: Vietnam War",
      date == ym("1957-10") ~ "Sputnik launched",
      date == ym("1962-10") ~ "Cuban Missile Crisis",
      date == ym("1963-11") ~ "JFK Assassination",
      date == ym("1969-07") ~ "Moon landing",
      date == ym("1974-08") ~ "Nixon resigns (Watergate)",
      date == ym("1975-04") ~ "End: Vietnam War",
      date == ym("1981-03") ~ "Reagan assassination attempt",
      date == ym("1989-11") ~ "Berlin Wall falls",
      date == ym("1991-12") ~ "End: Cold War",
      date == ym("1998-12") ~ "Clinton impeachment",
      date == ym("2001-09") ~ "9/11",
      date == ym("2003-03") ~ "Invasion of Iraq",
      date == ym("2008-09") ~ "Financial crisis peaks",
      date == ym("2011-05") ~ "Osama bin Laden killed",
      date == ym("2019-12") ~ "1st Trump Impeachment",
      date == ym("2020-03") ~ "COVID-19 emergency declared",
      date == ym("2021-01") ~ "Capitol Attack (Jan 6)",
      date == ym("2021-01") ~ "2nd Trump Impeachment",
      date == ym("2021-04") ~ "Afghanistan withdrawal announced",
      date == ym("2021-01") ~ "2nd Trump impeachment",
      date == ym("2022-02") ~ "Start: Russia-Ukraine war",
      date == ym("2023-10") ~ "Start: Israel-Palestine conflict",
      date == ym("2023-12") ~ "Peak border crisis",
      date == ym("2025-06") ~ "ICE raids in LA",
      TRUE ~ NA
    ),
    important_event_dates = case_when(
      !is.na(important_events) ~ date,
      TRUE ~ as.Date(NA)
    )
  )

# adding comparison data
approval_comparison_data <-
  final_combined_data |>
  drop_na(1:5) |>
  group_by(president) |>
  mutate(
    months_in_office = case_when(
      month(min(date)) == 1 ~ interval(min(date), date) %/% months(1),
      month(min(date)) == 2 ~ interval(min(date), date) %/%
        months(1) +
        (month(min(date)) - 1), # presi's missing first month
      month(min(date)) == 8 ~ interval(min(date), date) %/%
        months(1) +
        (month(min(date)) - 8), # Ford
      month(min(date)) == 6 ~ interval(min(date), date) %/%
        months(1) +
        (month(min(date)) - 4), # Truman
      month(min(date)) == 12 ~ interval(min(date), date) %/%
        months(1) +
        (month(min(date)) - 11), # LBJ
      month(min(date)) == 7 ~ interval(min(date), date) %/%
        months(1) +
        (month(min(date)) - 1), # FDR 3rd term
    ),
    president = case_when(
      str_detect(president, "Trump") ~ "Donald Trump",
      TRUE ~ president
    ),
    months_in_office = case_when(
      date < "2025-01-01" ~ months_in_office,
      president == "Donald Trump" & date >= "2025-01-01" ~ months_in_office +
        49,
      president == "Joe Biden" & date == "2025-01-01" ~ 48
    )
  ) |>
  select(1:5, 19)

print(colnames(final_combined_data))

visdat::vis_miss(final_combined_data, warn_large_data = FALSE)

save(final_combined_data, file = here("data/complete_data.rda"))
save(approval_comparison_data, file = here("data/comparison_data.rda"))


tail(final_combined_data)
