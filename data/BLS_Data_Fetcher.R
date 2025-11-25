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

# 2004-2024
payload1 <- list(
  'seriesid' = series_ids,
  'startyear' = 2004,
  'endyear' = 2024,
  'registrationkey' = BLS_KEY
)

response1 <- blsAPI(payload1, api_version = 2, return_data_frame = F)
json_data1 <- fromJSON(response1, simplifyVector = F)

# 1983-2003
payload2 <- list(
  'seriesid' = series_ids,
  'startyear' = 1983,
  'endyear' = 2003,
  'registrationkey' = BLS_KEY
)

response2 <- blsAPI(payload2, api_version = 2, return_data_frame = F)
json_data2 <- fromJSON(response2, simplifyVector = F)

# 1962-1982
payload3 <- list(
  'seriesid' = series_ids,
  'startyear' = 1962,
  'endyear' = 1982,
  'registrationkey' = BLS_KEY
)

response3 <- blsAPI(payload3, api_version = 2, return_data_frame = F)
json_data3 <- fromJSON(response3, simplifyVector = F)

# 1941-1961
payload4 <- list(
  'seriesid' = series_ids,
  'startyear' = 1941,
  'endyear' = 1961,
  'registrationkey' = BLS_KEY
)

response4 <- blsAPI(payload4, api_version = 2, return_data_frame = F)
json_data4 <- fromJSON(response4, simplifyVector = F)

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

bls_data_clean1 <- bind_rows(lapply(json_data1$Results$series, process_series))
bls_data_clean2 <- bind_rows(lapply(json_data2$Results$series, process_series))
bls_data_clean3 <- bind_rows(lapply(json_data3$Results$series, process_series))
bls_data_clean4 <- bind_rows(lapply(json_data4$Results$series, process_series))

# appending tables adding more descriptive variable column after series_id for ease of later analysis
bls_data_clean <- bls_data_clean1 %>%
  bind_rows(bls_data_clean2, bls_data_clean3, bls_data_clean4) %>%
  mutate(variable = case_when(
    series_id == 'LNS14000000' ~ "unemployment_rate",
    series_id == 'CUUR0000SA0' ~ "consumer_p_index"
  )) %>%
  relocate(variable, .after = series_id)

head(bls_data_clean)

nrow(bls_data_clean)

#################### SAVE DATASET INTO RDA ####################

save(bls_data_clean, file = here("data/bls_data_clean.rda"))

# naming: datasetname_clean
# rda file: datasetname_clean.rda 
