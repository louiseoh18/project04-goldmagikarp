
# Grabs data from the BLS API

if (!requireNamespace(c("devtools", "blsAPI"), quietly = TRUE)) {
  install_github("blsAPI")
}

library(blsAPI)
library(jsonlite)
library(dplyr)
library(ggplot2)

BLS_KEY <- "e8e14ae76cdd4f929a3e817b91126806"

series_ids <- list('LNS14000000', 'CUUR0000SA0')

payload <- list(
  'seriesid'  = series_ids,
  'startyear' = 2020, 
  'endyear'   = 2024,
  'registrationkey' = BLS_KEY 
)

response <- blsAPI(payload, api_version = 2, return_data_frame = F)
json_data <- fromJSON(response, simplifyVector = F)

process_series <- function(series_obj) {
  series_id <- series_obj$seriesID
  data_list <- series_obj$data
  
  df <- bind_rows(data_list) %>%
    mutate(
      series_id = series_id,
      value = as.numeric(value),
      year = as.integer(year),
      date = as.Date(paste0(year, "-", substr(period, 2, 3), "-01"))
    ) %>%
    select(date, series_id, value)
  
  return(df)
}

clean_data <- bind_rows(lapply(json_data$Results$series, process_series))

head(clean_data)