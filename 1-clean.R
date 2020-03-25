library(tidyverse)

## Timeseries CSV #####
data <- read_csv("raw_data/timeseries-tidy.csv")

names(data)
part_time_data <- data %>%
  # filter(date == "2020-03-23") %>%
  group_by(type) %>%
  mutate(rid = row_number()) %>%
  select(rid, date, county, state, country, population, lat, long, type, value)


time_data_out <- part_time_data %>%
  spread(type, value)

write_csv(time_data_out, "data/cds_timeseries_spread.csv")



time_data_out %>% group_by(state)

# data %>%
#   # filter(country == "USA") %>%
#   group_by(country, date, type) %>%
#   spread(type, value)



## Snapshot CSV ######
## which does not have date
data_all <- read_csv("raw_data/data.csv")
names(data_all)

data_all %>%
  filter(country == "USA") %>%
  group_by(state)


