library(tidyverse)

## radius from angle then circumference
## for longitude by radius
####
## NOTICE: estimation, assuming earth is a perfect sphere, NOT ELLIPSOID!
####

latlongdeg2mile <- function(lat, long){
  
  ## Latitude 
  R <- 3959 # mean radius of Earth in miles
  circumference <- 2 * pi * R
  
  ## The 1 degree of 360 is
  D_lat_1_deg <- circumference / 360
  round(D_lat_1_deg, 2)
  
  ## Longitude
  ## This is the same for one degree at the equator (Latitude = 0)
  ## But as abs(Latitude) goes to 90 the circumference goes to 0
  
  # lat <- 34
  lat <- abs(lat)
  lat
  ## at lat = 0, then D_lat_1_deg == D_lon_1_deg
  ## https://www.nhc.noaa.gov/gccalc.shtml
  ## at lat = 34, then one degree lon is 50 nautical miles
  ## at lat = 84, then one degree lon is 06 nautical miles
  
  ## calculate radius first
  ## radius is the angle theta (90 - lat) | opposite (R_lat) over hypot (R)., 
  ##         with hypot. = R
  ## sin(theta) = opposite / hypot. = R_lat / R
  radian <- (90 - lat) * 2 * pi / 360
  R_lat <- sin(radian) * R
  R_lat
  circumference_lat <- 2 * pi * R_lat
  ## at set latitude (lat)
  D_lon_1_deg <-  circumference_lat / 360
  # D_lon_1_deg <-  (circumference / (lat * 4)) / 3.6 
  # where x is the angle of latidude
  round(D_lon_1_deg, 2)
  
  # print(paste("one degree latitude =", round(D_lat_1_deg), "miles",
  #             "one degree longitude =", round(D_lon_1_deg), "miles"))
}


latlongdeg2mile(lat = 0, long = -93)

## Latitude 
R <- 3959 # mean radius of Earth in miles
circumference <- 2 * pi * R
circumference / 360


# ## My People ########
# ## degree buffer search area long, lat.
# lon_buffer <- 1
# lat_buffer <- 1 ## change to 3 for comparison
# 
# my_people <- tibble(    c(1:23),
#                         c("Fayetteville", "Leesburg", "Smackover", "RoundTop", "SanDiego", "Ravenna", "Barrington", "Helwan", "PalmDesert", "Melbourne", "Victoria", "LosAngeles", "Poole", "Pittsburgh", "PIT", "ATL", "Annapolis", "Nassau", "Windsor", "Montpelier", "Russelville", "Pittsburg", "BatonRouge" ),
#                         c("AR", "FL", "AR", "TX", "CA", "OH", "RI", "EGY", "CA", "AUS", "CAN",                                                               "CA", "ENG", "PA", "PA", "GA", "MD", "BAH", "CT", "VT", "AR", "KS", "LA" ),
#                         c("Adam", "Grandma", "Mom",  "Dad",  "Chance", "Ohio", "RI",  "Helwan", "Ahmed", "Ben", "Victoria", "Ryan",    "Rob", "CMU", "PIT",    "ATL", "Annapolis", "Ozy", "Justin", "Joshua", "Haley", "Don", "Benee"),
#                         c(-94.16, -81.87,     -92.73, -96.80, -117.12, -81.24,  -71.32, 31.33,  -116.39,  145.04, -123.37,    -118.44,  -2.00, -79.95,-80.25, -84.43,  -76.54,     -77.29, -72.64,   -72.57,   -93.13,        -94.70, -91.20),
#                         c(36.07,  28.73,      33.36,  29.98,  32.80,   41.16,  41.74,  29.84,  33.73,   -36.83,  48.47,      34.07,    50.71, 40.44, 40.50,  33.64,   38.79,       25.04,  41.85 ,   44.26,   35.27,         37.40,  30.45), 
#                         .name_repair = ~ c("number", "city", "region",  "name", "longitude", "latitude"))
# 
# # names(my_people) <- c("name", "longitude", "latitude")
# num <- dim(my_people)[1]
# 
# 
# ## Load Data ############
# ## timeseries cds
# # time_data_out <- read_csv("data/cds_timeseries_spread.csv", col_types = cols(
# #   city = col_character(),
# #   county = col_character(),
# #   state = col_character(),
# #   country = col_character(),
# #   population = col_double(),
# #   lat = col_double(),
# #   long = col_double(),
# #   url = col_character(),
# #   cases = col_double(),
# #   deaths = col_double(),
# #   recovered = col_double(),
# #   active = col_double(),
# #   tested = col_double(),
# #   date = col_date(format = "")
# # ))
# # 
# # today <- max(time_data_out$date)
# # today
# # data_time <- time_data_out %>%
# #   filter(date == today)
# 
# ## snapshot cds
# data_snap <- read_csv("raw_data/data.csv")
# names(data_snap)
# ## For Mypeople bubble count, I must remove the sum duplicates
# data_snap %>% filter(aggregate == "county") %>% summarise(earth = sum(cases))
# ## looks like aggregate broke
# data_all <- data_snap %>% filter(aggregate == "county")
# ## if you want to sum it yourself, you can do 
# ## city = NULL, county != NULL, state = != NULL
# data_snap %>% filter(is.na("county"), is.na("state")) %>% summarise(earth = sum(cases, na.rm = TRUE))
# data_all <- data_snap %>% filter(!is.na("county"))
# data_snap %>% filter(state != "NA", county != "NA") %>% summarise(total = sum(cases))
# data_all <- data_snap %>% filter(state != "NA", county != "NA")
# 
# ##
# 
# ## Out ##########
# ## this out is wrong, but I need it to create the out variable
# out <- data_all %>%
#   filter(  lat <= my_people$latitude[1] + lat_buffer
#            & lat >= my_people$latitude[1] - lat_buffer
#            & long <= my_people$longitude[1] + lon_buffer
#            & long >= my_people$longitude[1] - lon_buffer) %>%
#   summarise(cases = sum(cases, na.rm = TRUE),
#             deaths = sum(deaths, na.rm = TRUE),
#             tested = sum(tested, na.rm = TRUE),
#             recovered = sum(recovered, na.rm = TRUE),
#             active = sum(active, na.rm = TRUE)) %>%
#   mutate(name = my_people$name[1], buffer_deg = lon_buffer)
# 
# # max(today$date)
# # min(today$date)
# ## the real question is the number of new cases in the last few days.
# ## first - you are healthy - then sick - then tested positive - then dead or recovered
# 
# ## Check Joshua, does Canada count? 44.26, -72.57
# ## Still have problem of state sum already in observation
# # Longit <- my_people$longitude[19]
# # Latit <- my_people$latitude[19]
# 
# # date_county <- "2020-03-09"
# ## need to seperate the States
# # names(today)
# # today %>%
# #   filter(  Lat <= Latit + lat_buffer
# #            & Lat >= Latit - lat_buffer
# #            & Long_ <= Longit + lon_buffer
# #            & Long_ >= Longit - lon_buffer) %>%
# #   summarize(deaths = sum(Deaths), confirmed = sum(Confirmed))
# 
# ## is this sum correct? calculating rate change is the number of new cases per day
# ## where as the date already give a cumulative count.
# 
# 
# for(x in c(1:num)) {
#   out[x, ] <- data_all %>%
#     filter(    lat <= my_people$latitude[x] + lat_buffer
#                & lat >= my_people$latitude[x] - lat_buffer
#                & long <= my_people$longitude[x] + lon_buffer
#                & long >= my_people$longitude[x] - lon_buffer) %>%
#     summarize(cases = sum(cases, na.rm = TRUE),
#               deaths = sum(deaths, na.rm = TRUE),
#               tested = sum(tested, na.rm = TRUE),
#               recovered = sum(recovered, na.rm = TRUE),
#               active = sum(active, na.rm = TRUE)) %>%
#     mutate(name = my_people$name[x], 
#            buffer_lon_deg = lon_buffer)
# }
# 
# ## Very strange, but when calculating deaths sum first, it mixed up death and confirmed
# 
# # out %>%
# #   arrange(desc(confirmed))
# 
# ## before filtering max date, add column with count per day
# # today %>%
# #   group_by(Lat, Long_) %>%
# #   mutate(day_count = count - lag(count),
# #          day_case = case - lag(case),
# #          day_recover = recover - lag(recover)) %>% 
# #   arrange(desc(day_count))
# 
# ## Examining the data shows that this radius may only work in US and China
# ## Some countries data has only one cental coordinate.
# 
# 
# 
# ## RUN LONGITUDE.R SCRIPT BEFORE RUNNING BELOW ######
# 
# ## add information about miles from degree longitude
# radius <- my_people %>%
#   mutate(lon_miles = lon_buffer * latlongdeg2mile(my_people$latitude, my_people$longitude),
#          lat_miles = lat_buffer * round(circumference / 360, 2))
# 
# # max(today$date)
# 
# my_people_out <- merge(radius, out, by = "name", all = TRUE) %>%
#   arrange(desc(cases), desc(deaths))
# 
# ## counts are too high because sums are already given for states
# ## counts may be too high because multiple sources reporting on same location
# 
# my_people_out %>%
#   select(name, city, region, lon_miles, lat_miles, cases, deaths, tested, recovered, active) %>%
#     write_csv(paste0("data/", format(Sys.time(), "%Y%m%d%H%M"), "_my_people_cds.csv"))



## USA State #######
timeseries_data_clean_usa_state %>%
  # filter(long < -100) %>%
  filter(long > -75) %>%
  group_by(state) %>%
  # filter(state == "NJ") %>%
  ggplot(aes(date, cases, color = state)) +
    geom_line()

c("Arkansas", "Louisiana", "New York", "California", "Texas")

timeseries_data_clean_usa_state %>% filter(state %in% c("AR", "LA", "NY", "CA", "TX")) %>%
  group_by(state) %>%
  ggplot(aes(date, cases, color = state)) +
    geom_line()
  

## Why is ALASKA long > -75? +0.7 degree long

