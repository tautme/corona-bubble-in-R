library(tidyverse)
## My People ########
## degree buffer search area long, lat.
lon_buffer <- 3
lat_buffer <- 3

my_people <- tibble(    c(1:32),
                        c("NewOrleans", "London", "NewYorkCity", "Melissa", "WarnerRobins", "Phoenix", "OverlandPark", "Denver", "Oakland", "Fayetteville", "Leesburg", "Smackover", "RoundTop", "SanDiego", "Ravenna", "Barrington", "Helwan", "PalmDesert", "Melbourne", "Victoria", "LosAngeles", "Poole", "Pittsburgh", "PIT", "ATL", "Annapolis", "Nassau", "Windsor", "Montpelier", "Russelville", "Pittsburg", "BatonRouge" ),
                        c("LA", "ENG", "NY", "TX", "GA", "AZ", "KS", "CO", "CA", "AR", "FL", "AR", "TX", "CA", "OH", "RI", "EGY", "CA", "AUS", "CAN",                                                               "CA", "ENG", "PA", "PA", "GA", "MD", "BAH", "CT", "VT", "AR", "KS", "LA" ),
                        c("NewOrleans", "London", "NYC", "Wayne", "Garrett", "Jaber", "Daniel", "Chase", "Bliss", "Adam", "Grandma", "Mom",  "Dad",  "Chance", "Ohio", "RI",  "Helwan", "Ahmed", "Ben", "Victoria", "Ryan",    "Rob", "CMU", "PIT",    "ATL", "Annapolis", "Ozy", "Justin", "Joshua", "Haley", "Don", "Benee"),
                        c(-90.10, -0.11, -73.99, -96.56, -83.60, -112.07, -94.68, -104.94, -122.25, -94.16, -81.87,     -92.73, -96.80, -117.12, -81.24,  -71.32, 31.33,  -116.39,  145.04, -123.37,    -118.44,  -2.00, -79.95,-80.25, -84.43,  -76.54,     -77.29, -72.64,   -72.57,   -93.13,        -94.70, -91.20),
                        c(29.95, 51.52, 40.74, 33.27, 32.61, 33.49, 38.98, 39.60, 37.81, 36.07,  28.73,      33.36,  29.98,  32.80,   41.16,  41.74,  29.84,  33.73,   -36.83,  48.47,      34.07,    50.71, 40.44, 40.50,  33.64,   38.79,       25.04,  41.85 ,   44.26,   35.27,         37.40,  30.45), 
                        .name_repair = ~ c("number", "city", "region",  "name", "longitude", "latitude"))

# names(my_people) <- c("name", "longitude", "latitude")
num <- dim(my_people)[1]

## choose data ############
## timeseries cds
# data_time <- read_csv("data/cds_timeseries_spread.csv", col_types = cols(
#   city = col_character(),
#   county = col_character(),
#   state = col_character(),
#   country = col_character(),
#   population = col_double(),
#   lat = col_double(),
#   long = col_double(),
#   url = col_character(),
#   cases = col_double(),
#   deaths = col_double(),
#   recovered = col_double(),
#   active = col_double(),
#   tested = col_double(),
#   date = col_date(format = "")
# ))

# data <- read_csv("/Users/adamhughes/coronadatascraper/dist/data.csv", 
#                  col_types = cols(
#                    .default = col_double(),
#                    city = col_character(),
#                    county = col_character(),
#                    state = col_character(),
#                    country = col_character(),
#                    url = col_character(),
#                    maintainers = col_character(),
#                    aggregate = col_character(),
#                    tz = col_character(),
#                    sources = col_character(),
#                    curators = col_character()
#                  ))


# today <- max(data_time$date)
# data_time <- time_data_out %>%
#   filter(date == today) 

## snapshot cds
data_snap <- read_csv("raw_data/data.csv")

## For Mypeople bubble count, I must remove the sum duplicates
data_snap %>% filter(aggregate == "county")
## looks like aggregate broke
# data_all <- data_snap %>% filter(aggregate == "county")
data_all <- data_snap %>% filter(!is.na(county), !is.na(state))
data_all %>% filter(country == "USA") %>% summarise(earth = sum(cases, na.rm = TRUE))
data_all %>% filter(country == "USA", state == "AR") %>% summarise(earth = sum(cases, na.rm = TRUE))


## this out is wrong, but I need it to create the out variable
out <- data_all %>%
  filter(  lat <= my_people$latitude[1] + lat_buffer
           & lat >= my_people$latitude[1] - lat_buffer
           & long <= my_people$longitude[1] + lon_buffer
           & long >= my_people$longitude[1] - lon_buffer) %>%
  summarise(cases = sum(cases, na.rm = TRUE),
            deaths = sum(deaths, na.rm = TRUE),
            tested = sum(tested, na.rm = TRUE),
            recovered = sum(recovered, na.rm = TRUE),
            active = sum(active, na.rm = TRUE)
  ) %>%
  mutate(name = my_people$name[1], buffer_deg = lon_buffer)

# max(today$date)
# min(today$date)
## the real question is the number of new cases in the last few days.
## first - you are healthy - then sick - then tested positive - then dead or recovered

## Check Joshua, does Canada count? 44.26, -72.57
## Still have problem of state sum already in observation
# Longit <- my_people$longitude[19]
# Latit <- my_people$latitude[19]

# date_county <- "2020-03-09"
## need to seperate the States
# names(today)
# today %>%
#   filter(  Lat <= Latit + lat_buffer
#            & Lat >= Latit - lat_buffer
#            & Long_ <= Longit + lon_buffer
#            & Long_ >= Longit - lon_buffer) %>%
#   summarize(deaths = sum(Deaths), confirmed = sum(Confirmed))

## is this sum correct? calculating rate change is the number of new cases per day
## where as the date already give a cumulative count.


for(x in c(1:num)) {
  out[x, ] <- data_all %>%
    filter(    lat <= my_people$latitude[x] + lat_buffer
               & lat >= my_people$latitude[x] - lat_buffer
               & long <= my_people$longitude[x] + lon_buffer
               & long >= my_people$longitude[x] - lon_buffer) %>%
    summarize(cases = sum(cases, na.rm = TRUE),
              deaths = sum(deaths, na.rm = TRUE),
              tested = sum(tested, na.rm = TRUE),
              recovered = sum(recovered, na.rm = TRUE),
              active = sum(active, na.rm = TRUE)) %>%
    mutate(name = my_people$name[x], 
           buffer_lon_deg = lon_buffer)
}

## Very strange, but when calculating deaths sum first it mixed up death and confirmed

# out %>%
#   arrange(desc(confirmed))

## before filtering max date, add column with count per day
# today %>%
#   group_by(Lat, Long_) %>%
#   mutate(day_count = count - lag(count),
#          day_case = case - lag(case),
#          day_recover = recover - lag(recover)) %>% 
#   arrange(desc(day_count))

## Examining the data shows that this radius may only work in US and China
## Some countries data has only one cental coordinate.

## RUN LONGITUDE.R SCRIPT BEFORE RUNNING BELOW ######

## add information about miles from degree longitude
radius <- my_people %>%
  mutate(lon_miles = lon_buffer * latlongdeg2mile(my_people$latitude, my_people$longitude),
         lat_miles = lat_buffer * round(circumference / 360, 2))

# max(today$date)

my_people_out <- merge(radius, out, by = "name", all = TRUE) %>%
  arrange(desc(cases), desc(deaths))

my_people_out %>% 
  select(name, city, region, longitude, latitude, lon_miles, lat_miles, cases, deaths, tested, recovered, active)

write_csv(my_people_out, "data/20200329_my_people_cds_snapshot.csv")


## Map ##########
# install.packages(c("leaflet", "sp"))
library(sp)
library(leaflet)
my_people_out_usa <- my_people_out %>% filter(longitude < -20)

df <- my_people_out_usa

coordinates(df) <- ~longitude+latitude

leaflet(df) %>% 
  addMarkers(popup = paste(df$name, "has", df$cases, "cases of COVID-19,", df$lon_miles, "miles around them. DATA:coronadatascraper")) %>%
  # addCircleMarkers(radius = 10) %>%
  # addRectangles(lng1 = lng - 3,
  #               lng2 = lng + 3,
  #               lat1 = lat - 3,
  #               lat1 = lat + 3) %>%
  # addCircles(radius = df$cases * df$deaths / 10) %>%
  # labelOptions() %>%
  addTiles()

