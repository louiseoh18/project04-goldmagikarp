
# Grabs approval rating data from the American Presidency Project website.

if (!requireNamespace(c("rvest", "dplyr", "lubridate", "stringr", "tidyr"), quietly = TRUE)) {
  install.packages(c("rvest", "dplyr", "lubridate", "stringr", "tidyr"))
}

library(rvest)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(here)

president_urls <- list(
  "Trump (2nd Term)" = "https://www.presidency.ucsb.edu/statistics/data/donald-j-trump-2nd-term-public-approval",
  "Joe Biden" = "https://www.presidency.ucsb.edu/statistics/data/joseph-r-biden-public-approval",
  "Trump (1st Term)" = "https://www.presidency.ucsb.edu/statistics/data/donald-j-trump-public-approval"
)

scrape_president_data <- function(name, url) {
  
  page <- read_html(url)
  tables <- page %>% html_table(fill = TRUE)
  
  approval_table <- NULL
  
  for (tbl in tables) {
    if ("Approving" %in% names(tbl) || "Disapproving" %in% names(tbl)) {
      approval_table <- tbl
      break
    }
  }
  
  if (is.null(approval_table)) {
    return(data.frame())
  }
  
  clean_df <- approval_table %>%
    rename_with(~ c("Start_Date", "End_Date", "Approve", "Disapprove", "Unsure"), 
                .cols = 1:5) %>%
    mutate(
      Start_Date = mdy(Start_Date),
      End_Date = mdy(End_Date),
      Poll_Date = Start_Date + floor((End_Date - Start_Date)/2),
      Approve = as.numeric(str_remove(Approve, "%")),
      Disapprove = as.numeric(str_remove(Disapprove, "%")),
      Unsure = as.numeric(str_remove(Unsure, "%"))
    ) %>%
    filter(!is.na(Poll_Date)) %>%
    mutate(President = name) %>%
    select(President, Poll_Date, Approve, Disapprove, Unsure)
  
  return(clean_df)
}

all_polls <- bind_rows(lapply(names(president_urls), function(name) {
  scrape_president_data(name, president_urls[[name]])
}))

monthly_approval <- all_polls %>%
  mutate(
    date = floor_date(Poll_Date, "month")
  ) %>%
  group_by(President, date) %>%
  summarise(
    Approval_Rating = mean(Approve, na.rm = TRUE),
    Disapproval_Rating = mean(Disapprove, na.rm = TRUE),
    Unsure_Rating = mean(Unsure, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(date))

head(monthly_approval)


#################### SAVE DATASET INTO RDA ####################

save(monthly_approval_clean, file = here("data/monthly_approval_clean.rda"))

# naming: datasetname_clean
# rda file: datasetname_clean.rda
