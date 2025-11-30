library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(here) # Added for consistent file saving

BEA_KEY <- "C77A6C27-03FD-4B8A-996F-CAA388B4ABF1"
base_url <- "https://apps.bea.gov/api/data/"

bea_config <- list(
  # Table T10106: GDP & Government Spending
  list(name = "Real_GDP",                   table = "T10106", line_desc = "Gross domestic product"),
  list(name = "Gov_Spending",               table = "T10106", line_desc = "Government consumption expenditures and gross investment"),
  
  # Table T20100: Income & Savings
  list(name = "Disposable_Income",          table = "T20100", line_desc = "Disposable personal income"),
  list(name = "Personal_Saving_Rate",       table = "T20100", line_desc = "Personal saving rate"),
  
  # Table T11200: Corporate Profits
  list(name = "Corporate_Profits",          table = "T11200", line_desc = "Corporate profits with inventory valuation"),
  
  # Table T20304: Inflation 
  list(name = "Healthcare_Price_Index",     table = "T20304", line_desc = "Health care")
)

fetch_bea_variable <- function(config_item) {
  
  params <- list(
    UserID = BEA_KEY,
    Method = "GetData",
    DataSetName = "NIPA",
    TableName = config_item$table,
    Frequency = "Q",
    Year = "ALL", 
    ResultFormat = "JSON"
  )
  
  response <- GET(base_url, query = params)
  
  if (status_code(response) != 200) {
    warning(paste("Failed to fetch", config_item$name, "- Status:", status_code(response)))
    return(NULL)
  }
  
  json_raw <- content(response, "text", encoding = "UTF-8")
  json_parsed <- fromJSON(json_raw)
  
  if (is.null(json_parsed$BEAAPI$Results$Data)) {
    warning(paste("No data returned for", config_item$name))
    return(NULL)
  }
  
  raw_df <- json_parsed$BEAAPI$Results$Data
  
  df <- raw_df %>%
    filter(grepl(config_item$line_desc, LineDescription, ignore.case = TRUE))
  
  target_line <- head(unique(df$LineNumber), 1)
  
  df <- df %>% 
    filter(LineNumber == target_line) %>%
    mutate(
      value = as.numeric(gsub(",", "", DataValue)),
      Year = as.integer(substr(TimePeriod, 1, 4)),
      Quarter = as.integer(substr(TimePeriod, 6, 6)),
      Month = (Quarter - 1) * 3 + 1,
      date = make_date(Year, Month, 1)
    ) %>%
    select(date, value) %>%
    rename_with(~ config_item$name, .cols = "value")
  
  return(df)
}

data_list <- lapply(bea_config, fetch_bea_variable) %>% 
  compact()

final_economic_data <- data_list %>%
  reduce(full_join, by = "date") %>%
  arrange(date)

print(head(final_economic_data))
print(tail(final_economic_data))

save(final_economic_data, file = here("data/BEA_economic_data.rda"))
