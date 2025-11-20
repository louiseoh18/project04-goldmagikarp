# Grabs data from the BLS API

if (!requireNamespace(c("devtools", "blsAPI"), quietly = TRUE)) {
  install.packages("devtools")
  library(devtools)
  devtools::install_github("mikeasilva/blsAPI")
}

library(blsAPI)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(here)

BLS_KEY <- "e8e14ae76cdd4f929a3e817b91126806"

# https://data.bls.gov/registrationEngine/validateKey/9b6e09b7a14eade73379b9589fbad06788eaad9542f5c75ab891f3ba7110b96a

series_ids <- list('LNS14000000', 'CUUR0000SA0')
# LNS14000000: Unemployment Rate
# CUUR0000SA0: Consumer Price Index

payload <- list(
  'seriesid' = series_ids,
  'startyear' = 2020,
  'endyear' = 2024,
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

bls_data_clean <- bind_rows(lapply(json_data$Results$series, process_series))

head(bls_data_clean)

nrow(bls_data_clean)

#################### SAVE DATASET INTO RDA ####################

save(bls_data_clean, file = here("data/bls_data_clean.rda"))

# naming: datasetname_clean
# rda file: datasetname_clean.rda 
