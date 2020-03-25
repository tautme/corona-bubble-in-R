## EDA
library(tidyverse)
explore_data <- read_csv("data/cds_timeseries_spread.csv")

## are there negative counts?
explore_data %>% names()

dim(explore_data)

explore_data$county %>% is.na() %>% sum()
explore_data %>%
  group_by(country)

## does the timeseries csv data also have the problem of double list growthFactor
## no, they are from the spread
