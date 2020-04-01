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

## load from this machine at /Users/adamhughes/coronadatascraper/dist/data.csv
# timeseries_data0 <- read_csv("/Users/adamhughes/coronadatascraper/dist/timeseries.csv", 
#                              col_types = cols(
#                                city = col_character(),
#                                county = col_character(),
#                                state = col_character(),
#                                country = col_character(),
#                                population = col_double(),
#                                lat = col_double(),
#                                long = col_double(),
#                                url = col_character(),
#                                aggregate = col_character(),
#                                tz = col_character(),
#                                cases = col_double(),
#                                deaths = col_double(),
#                                recovered = col_double(),
#                                active = col_double(),
#                                tested = col_double(),
#                                growthFactor = col_double(),
#                                date = col_date(format = "")
#                              ))
# 
# timeseries_data <- timeseries_data0

# which dataset is the first one? with all the data details?
timeseries_data <- read_csv("raw_data/timeseries.csv", col_types = cols(
  city = col_character(),
  county = col_character(),
  state = col_character(),
  country = col_character(),
  population = col_double(),
  lat = col_double(),
  long = col_double(),
  url = col_character(),
  cases = col_double(),
  deaths = col_double(),
  recovered = col_double(),
  active = col_double(),
  tested = col_double(),
  growthFactor = col_double(),
  date = col_date(format = "")
))

ldate <- max(timeseries_data$date)
ldate
names(timeseries_data)
## here we have a warning about county column expected logical, then character comes in.

## Russia has states in curilic, and it reads them!
timeseries_data %>%
  filter(country == "RUS") %>%
  group_by(state) %>% 
  filter(date == ldate, state != "NA") %>%
  summarise(cases = sum(cases)) %>%
  arrange(desc(cases))
  
## where are the cities?
timeseries_data[!is.na(timeseries_data$city), ]

## reading json #######
# library(jsonlite)
# timeseries_data <- read_json("raw_data/timeseries.json", simplifyVector = TRUE)
# timeseries_data <- fromJSON("raw_data/timeseries.json")
# 
# library(data.table)
# timeseries_data <- rbindlist(timeseries_data, fill=TRUE)
# 
# timeseries_data <- lapply(timeseries_data, function(x) {
#   x[sapply(x, is.null)] <- NA
#   unlist(x)
# })
# timeseries_data <-do.call("rbind", timeseries_data)


#########
## I think it is this one
## But why does it have calculations that are not needed.
## Tidy data is one observation per row
## Are there sums already built in from all... states = USA?
timeseries_data %>%
  filter(date == "2020-03-28") %>%
  filter(country == "USA", state == "NY") %>%
  # group_by(state, url) %>%
  summarise(total = sum(cases, na.rm = TRUE)) %>%
  # filter(state != "NA") %>%
  summarise(total = sum(total)) 

## March 25 has 64,916 cases. ref. wikipedia
## 83,800 ref. jhu dashboard
## how too tell if the state is already totaled
## stop the double counting!

##  If I group by county and state then remove na will it give correct count?
timeseries_data %>%
  filter(country == "USA") %>%
  group_by(state) %>%
  filter(state == "CA", date == "2020-03-29", county != "NA") %>%
  summarise(total = sum(cases, na.rm = TRUE))
## game changer, county != "NA"
## MARCH 26 AR = 349, CA = 4040
## March 29 AR = 409, CA = 5552
## As of March 26, 2020, 2 p.m. Pacific Daylight Time, there are a total of 3,801 positive cases 

## Do the same sums with aggregate
timeseries_data %>%
  filter(country == "USA", aggregate == "county")
## the aggregate doesn't make sense, go back and check snapshot data.csv

timeseries_data_clean_usa_state <- timeseries_data %>%
  filter(country == "USA", state != "NA") 
timeseries_data_clean_usa_state <- timeseries_data_clean_usa_state[is.na(timeseries_data_clean_usa_state$county), ]
timeseries_data_clean_usa_state <- timeseries_data_clean_usa_state[is.na(timeseries_data_clean_usa_state$city), ]

## Now that we have aggregate, it should be easier to remove sum observations


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
  # select(-growthFactor) %>%
  filter(county != "NA") 

## this filter removes too much data, there are still three negavites in active for CHN GUM PRI
# timeseries_data_clean <- timeseries_data_clean %>%
#   filter(active > 1)


## remove state from date set for us
write_csv(timeseries_data_clean, "data/cds_timeseries_spread.csv")
write_csv(timeseries_data_clean_usa_state, "data/cds_timeseries_spread_usa_state.csv")

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

