library(tidyverse)

## Timeseries CSV #####
# data <- read_csv("raw_data/timeseries-tidy.csv")
# 
# names(data)
# is.na(data$value) %>% sum()
# data %>% filter(type != "growthFactor" & value == 0.5)
# 
# data %>%
# is.na(data$value) %>% sum()
# 
# ## growthFactor adds an extra observation for every day. remove first.
# part_time_data <- data %>%
#   filter(type != "growthFactor") %>%
#   group_by(type) %>%
#   mutate(rid = row_number()) %>%
#   select(rid, date, county, state, country, population, lat, long, type, value)



## which dataset is the first one? with all the data details?
timeseries_data <- read_csv("raw_data/timeseries.csv")
## I think it is this one
## But why does it have calculations that are not needed.
## Tidy data is one observation per row
## Are there sums already built in from all... states = USA?
timeseries_data %>%
  filter(date == "2020-03-25") %>%
  filter(country == "USA") %>%
  group_by(state, url) %>%
  # summarise(total = sum(cases, na.rm = TRUE)) %>%
  # filter(state != "NA") %>%
  # summarise(total = sum(total)) %>%
  View()
## March 25 has 64,916 cases. ref. wikipedia
## 83,800 ref. jhu dashboard
## how too tell if the state is already totaled
## stop the double counting!


dim(timeseries_data)
timeseries_data$county %>% is.na() %>% sum()
timeseries_data$city %>% is.na() %>% sum()
timeseries_data$state %>% is.na() %>% sum()
timeseries_data$growthFactor %>% is.na() %>% sum()
datet <- max(timeseries_data$date)
datet
timeseries_data %>%
  filter(date == datet) %>%
  select(growthFactor) %>% is.na() %>% sum()
## some of the 268 latest dates still donot have a growthfactor, so remove

timeseries_data_clean <- timeseries_data %>%
  select(-county, -city, -growthFactor)

## this filter removes too much data, there are still three negavites in active for CHN GUM PRI
# timeseries_data_clean <- timeseries_data_clean %>%
#   filter(active > 1)

write_csv(timeseries_data_clean, "data/cds_timeseries_spread.csv")

# time_data_out <- part_time_data %>%
#   spread(type, value, fill = 0)


# write_csv(time_data_out, "data/cds_timeseries_spread.csv")



# time_data_out %>% group_by(state)

# data %>%
#   # filter(country == "USA") %>%
#   group_by(country, date, type) %>%
#   spread(type, value)



## Snapshot CSV ######
# which does not have date
# data_all <- read_csv("raw_data/data.csv")
# names(data_all)
# 
# data_all %>%
#   filter(country == "USA") %>%
#   group_by(state) %>%
#   summarise(case = sum(cases, na.rm = TRUE),
#             pop = sum(population, na.rm = TRUE)) %>%
#   mutate(density = pop / case) %>%
#   arrange(density) %>%
#   View()
# 
# ## recreate count for March 25
# data_all %>%
#   filter(country == "USA") %>%
#   group_by(state) %>%
#   # summarise(case = sum(cases, na.rm = TRUE)) %>%
#   # summarise(totalusa = sum(case, na.rm = TRUE)) %>%
#   View()

