library(tidyverse)

## Timeseries CSV #####
data <- read_csv("raw_data/timeseries-tidy.csv")

names(data)
is.na(data$value) %>% sum()
data %>% filter(type != "growthFactor" & value == 0.5)

## growthFactor adds an extra observation for every day. remove first.
part_time_data <- data %>%
  filter(type != "growthFactor") %>%
  group_by(type) %>%
  mutate(rid = row_number()) %>%
  select(rid, date, county, state, country, population, lat, long, type, value)

## which dataset is the first one? with all the data details?
timeseries_data <- read_csv("raw_data/timeseries.csv")
dim(timeseries_data)
timeseries_data$county %>% is.na() %>% sum()
timeseries_data$city %>% is.na() %>% sum()
timeseries_data$state %>% is.na() %>% sum()
timeseries_data$growthFactor %>% is.na() %>% sum()
datet <- max(timeseries_data$date)
timeseries_data %>%
  filter(date == datet) %>%
  select(growthFactor) %>% is.na() %>% sum()
## some of the 268 latest dates still donot have a growthfactor, so remove

timeseries_data_clean <- timeseries_data %>%
  select(-county, -city, -growthFactor)
write_csv(timeseries_data_clean, "data/cds_timeseries_spread.csv")

# time_data_out <- part_time_data %>%
#   spread(type, value, fill = 0)


# write_csv(time_data_out, "data/cds_timeseries_spread.csv")



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


