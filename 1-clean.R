library(tidyverse)

## Timeseries CSV #####
data <- read_csv("raw_data/timeseries-tidy.csv")

data %>% group_by(state)

data %>%
  # filter(country == "USA") %>%
  group_by(country, date, type) %>%
  spread(type, value)

## Snapshot CSV ######
data_all <- read_csv("raw_data/data.csv")

data_all %>%
  filter(country == "USA") %>%
  group_by(state)


