library(tidyverse)
## My People ########
## degree buffer search area long, lat.
lon_buffer <- 1
lat_buffer <- 1

my_people <- 
  tibble(    c(1:34),
  c("Crawfordville", "LittleRock", "NewOrleans", "London", "NewYorkCity", 
    "Melissa", "WarnerRobins", 
    "Phoenix", "OverlandPark", "Denver", "Oakland", "Fayetteville", 
    "Leesburg", "Smackover", "RoundTop", 
    "SanDiego", "Ravenna", "Barrington", "Helwan", "PalmDesert", 
    "Melbourne", "Victoria", "LosAngeles", "Poole", 
    "Pittsburgh", "PIT", "ATL", "Annapolis", "Nassau", "Windsor", "Montpelier", "Russelville", 
    "Pittsburg", "BatonRouge" ),
  c("FL", "AR", "LA", "ENG", "NY", "TX", "GA", "AZ", "KS", "CO", 
    "CA", "AR", "FL", "AR", "TX", "CA", "OH", "RI", "EGY", "CA", "AUS", "CAN", 
    "CA", "ENG", "PA", "PA", "GA", "MD", "BAH", "CT", "VT", "AR", "KS", "LA" ),
  c("Jan", "Anne", "NewOrleans", "London", "NYC", "Wayne", "Garrett", "Jaber", "Daniel", 
    "Chase", "Bliss", "Adam", "Grandma", "Mom",  "Dad",  "Chance", "Ohio", 
    "RI",  "Helwan", "Ahmed", "Ben", "Victoria", 
    "Ryan",    "Rob", "CMU", "PIT",    "ATL", "Annapolis", "Ozy", 
    "Justin", "Joshua", "Haley", "Don", "Benee"),
  c(-84.39, -92.41, -90.10, -0.11, -73.99, -96.56, -83.60, -112.07, -94.68, 
    -104.94, -122.25, -94.16, -81.87,     -92.73, -96.80, 
    -117.12, -81.24,  -71.32, 31.33,  -116.39,  145.04, -123.37,    
    -118.44,  -2.00, -79.95,-80.25, -84.43,  -76.54,     
    -77.29, -72.64,   -72.57,   -93.13,        -94.70, -91.20),
  c(30.19, 34.74, 29.95, 51.52, 40.74, 33.27, 32.61, 33.49, 
    38.98, 39.60, 37.81, 36.07,  28.73,      33.36,  29.98,  
    32.80,   41.16,  41.74,  29.84,  33.73,   -36.83,  48.47,      
    34.07,    50.71, 40.44, 40.50,  33.64,   38.79,       
    25.04,  41.85 ,   44.26,   35.27,         37.40,  30.45), 
  .name_repair = ~ c("number", "city", "region",  "name", "longitude", "latitude"))


# names(my_people) <- c("name", "longitude", "latitude")
num <- dim(my_people)[1]

# names(data_time)
# # ##make to run like snap_data
# # time_snap <- data_time %>%
# data_time <-data_time %>% select(-growthFactor)

# data <- read_csv("/Users/adamhughes/Documents/coronadatascraper/dist/data.csv",
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


## load data ######
data_snap <- read_csv("raw_data/data.csv", 
                        col_types = cols(.default = col_character(),
                                        city = col_character(),
                                        cases = col_double(),
                                        deaths = col_double(),
                                        recovered = col_double(),
                                        tested = col_double(),
                                        active = col_double(),
                                        population = col_double(),
                                        lat = col_double(),
                                        long = col_double(),
                                        rating = col_double(),
                                        hospitalized = col_double()
                                        ))


names(data_snap)
glimpse(data_snap)



## For Mypeople bubble count, I must remove the sum duplicates
# data_snap %>% filter(aggregate == "county")
## looks like aggregate broke
# data_all <- data_snap %>% filter(aggregate == "county")

## cut data #######
is.na(data_snap$county)
data_all <- data_snap %>% filter(level != county)
# data_all <- data_snap %>% filter(!is.na(county), !is.na(state))
data_all %>% filter(country == "United States") %>% 
  summarise(usa = sum(cases, na.rm = TRUE))
data_all %>% filter(country == "United States", state == "Arkansas") %>% 
  summarise(ar = sum(cases, na.rm = TRUE))

my_people$name
pep <- 1
## this out is wrong, but I need it to create the out variable
out <- data_all %>%
  filter(  lat <= my_people$latitude[pep] + lat_buffer
           & lat >= my_people$latitude[pep] - lat_buffer
           & long <= my_people$longitude[pep] + lon_buffer
           & long >= my_people$longitude[pep] - lon_buffer) %>%
  summarise(cases = sum(cases, na.rm = TRUE),
            deaths = sum(deaths, na.rm = TRUE),
            tested = sum(tested, na.rm = TRUE),
            recovered = sum(recovered, na.rm = TRUE),
            active = sum(active, na.rm = TRUE),
            population = sum(population, na.rm = TRUE),
            hospitalized = sum(hospitalized, na.rm = TRUE)
  ) %>%
  mutate(name = my_people$name[pep], buffer_deg = lon_buffer)

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
              active = sum(active, na.rm = TRUE),
              population = sum(population, na.rm = TRUE),
              hospitalized = sum(hospitalized, na.rm = TRUE)) %>%
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

my_people_out <- merge(radius, out, by = "name", all = TRUE) 

names(my_people_out)
my_people_out %>% 
  select(name, city, region, longitude, 
         latitude, lon_miles, lat_miles, tested, 
         cases, deaths, recovered, active) %>%
  arrange(desc(cases), desc(deaths))

my_people_out %>% 
  mutate(case_per_capita = cases / population, death_per_capita = deaths / population) %>% 
  arrange(desc(population))

write_csv(my_people_out, 
          paste0("data/", format(Sys.time(), "%Y%m%d%H%M"), "_my_people_cds_snapshot.csv"))



## Map People ##########
# install.packages(c("leaflet", "sp"))
library(sp)
library(leaflet)
my_people_out_usa <- my_people_out %>% filter(longitude < -20)
df <- my_people_out_usa
my_people_out_usa$name
coordinates(df) <- ~longitude+latitude
pep <- 3
df$name

leaflet(df) %>% 
  addMarkers(popup = paste(df$name, "has", df$cases, "cases of COVID-19,", 
                            df$deaths, "deaths,", df$tested, "tested",
                           " approximatly ", df$lon_miles, 
                           "miles radius around them. DATA:coronadatascraper.com"),
             # fillOpacity = df$deaths / 80, 
             # radius = 5,
             # weight = 1
             ) %>%
  # addCircleMarkers(map = dfa, 
  #                  fillOpacity = dfa$deaths,
  #                  popup = paste("<font size=3> ", dfa$county, " , ", dfa$state,
  #                                "<p>Population: <B>", dfa$population, "</B></p>
  #                               <p>Cases: <B>", dfa$cases, "</B></p>
  #                               <p>Deaths: <B>", dfa$deaths, "</B></p>
  #                               <p>DATA: <B>https://coronadatascraper.com</B></p></font>")
  #                  ) %>%
  # addCircleMarkers(radius = 10) %>%
  addRectangles(lat2 = my_people_out_usa$latitude[pep] + lat_buffer,
                lat1 = my_people_out_usa$latitude[pep] - lat_buffer,
                lng2 = my_people_out_usa$longitude[pep] + lon_buffer, 
                lng1 = my_people_out_usa$longitude[pep] - lon_buffer, 
                popup = paste("Estimate from county level data points --" , 
                              "(2 degree Longitude, 2 degree Latitude square) In this", 
                              my_people_out_usa$lon_miles[pep] * 2, "mile wide and ", 
                              my_people_out_usa$lat_miles[pep] * 2, 
                              "mile tall area, there are an estimated <B>", 
                              my_people_out_usa$cases[pep], 
                              "</B> confirmed cases of COVID-19.", 
                              "DATA:https://coronadatascraper.com")) %>%
  # addCircles(radius = df$cases * df$deaths / 10) %>%
  # labelOptions() %>%
  addTiles()



## plot w/ county #######
## recreate plot from JHU with square and all county counts
dfa <- data_all %>% 
  filter(country == "United States", 
         state %in% c("Arkansas", "Louisiana", "Missouri", 
                      # "California", 
                      "Mississippi", "Oklahoma", "Texas"),
         # !is.na(Admin2),
         lat != 0)
names(dfa)
my_people_out_usa$name
pep <- 6
names(dfa)[13] <- "latitude"
names(dfa)[14] <- "longitude"

coordinates(dfa) <- ~longitude+latitude

## Normalize
normalized <- (dfa$cases - min(dfa$cases)) / (max(dfa$cases) - min(dfa$cases)) * 10
median(normalized)

leaflet(dfa) %>% 
  addTiles() %>%
  addRectangles(lat2 = my_people_out_usa$latitude[pep] + lat_buffer,
                lat1 = my_people_out_usa$latitude[pep] - lat_buffer,
                lng2 = my_people_out_usa$longitude[pep] + lon_buffer, 
                lng1 = my_people_out_usa$longitude[pep] - lon_buffer, 
                popup = paste("<font size=3>ESTIMATION SQUARE:
                              <p>Population: <B>", my_people_out_usa$population[pep], "</B></p>
                              <p>Test: <B>", my_people_out_usa$test[pep], "</B></p>
                              <p>Cases: <B>", my_people_out_usa$cases[pep], "</B></p>
                              <p>Active: <B>", my_people_out_usa$active[pep], "</B></p>
                              <p>Recovered: <B>", my_people_out_usa$recovered[pep], "</B></p>
                              <p>Deaths: <B>", my_people_out_usa$deaths[pep], "</B></p>
                              DATA: <B>https://coronadatascraper.com</B></font>")) %>%
  addCircleMarkers(fillOpacity = dfa$deaths, 
                   radius = 4, 
                   weight = 1,
                   popup = paste("<font size=3> ", dfa$county, " , ", dfa$state,
                                "<p>Population: <B>", dfa$population, "</B></p>
                                <p>Cases: <B>", dfa$cases, "</B></p>
                                <p>Deaths: <B>", dfa$deaths, "</B></p>
                                <p>DATA: <B>https://coronadatascraper.com</B></p></font>"))



## Normalize
(dfa$cases - min(dfa$cases)) / (max(dfa$cases) - min(dfa$cases)) %>% max()

# paste("Estimate from county level data points --", 
#       "(2 degree Longitude, 2 degree Latitude square) In this",
#       my_people_out_usa$lon_miles[pep] * 2, "mile wide and ",
#       my_people_out_usa$lat_miles[pep] * 2, "mile tall area, there are an estimated <B>",
#       my_people_out_usa$cases[pep], 
#       "</B> confirmed cases of COVID-19. DATA: https://coronadatascraper.com")
# 
# paste("Estimate from county level data points. In this square there are an estimated, ",
#       my_people_out_usa$population[pep], " people. There are<B>",
#       my_people_out_usa$cases[pep], 
#       "</B> cases of COVID-19, and<B>", my_people_out_usa$deaths[pep],
#       "</B>deaths. DATA: <B>https://coronadatascraper.com</B>")) %>%
#   addCircleMarkers(fillOpacity = dfa$cases,
#                    radius = 5, popup = paste("ESTIMATE: ",
#                             dfa$county, " , ", dfa$state, "has<B>",
#                             dfa$cases, "</B>cases of COVID-19, and<B>", 
#                             my_people_out_usa$deaths[pep],
#                             "</B>deaths. DATA: <B>https://coronadatascraper.com</B>")
# 
#    addCircleMarkers(radius = normalized * 15,
#                     popup = paste("Estimate: ",
#                        df$Admin2, " Parish/County, ", df$Province_State, "has<B>",
#                        df$Confirmed, "</B>cases of COVID-19, on",
#                            df$Last_Update, ". DATA: JHU-CSSE"))
                   
## PUBLISH TEXT
# Experimental
# Click Square -- COVID-19 Map -- DATA: https://coronadatascraper.com
# Check state and territorial health departments.
# Estimate COVID-19 cases from county level data points.
# DATA: https://coronadatascraper.com
# covid_baton_rouge





## As you increase the radius, how does increase plot
## radius vs. cases #########

my_radius <- seq(0, 2.5, by = 0.2)
my_radius[1]
num <- length(my_radius)
names(data_all)

outi <- data_all %>%
  filter(    lat <= my_people$latitude[1] + my_radius[1]
             & lat >= my_people$latitude[1] - my_radius[1]
             & long <= my_people$longitude[1] + my_radius[1]
             & long >= my_people$longitude[1] - my_radius[1]) %>%
  summarise(cases = sum(cases),
            deaths = sum(deaths)) %>%
  mutate(radius = my_radius[1])

my_people$name
pep <- 34

for(x in c(1:num)) {
  outi[x, ] <- data_all %>%
    filter(    lat <= my_people$latitude[pep] + my_radius[x]
               & lat >= my_people$latitude[pep] - my_radius[x]
               & long <= my_people$longitude[pep] + my_radius[x]
               & long >= my_people$longitude[pep] - my_radius[x]) %>% 
    summarise(cases = sum(cases),
              deaths = sum(deaths, na.rm = TRUE)) %>%
    mutate(radius = my_radius[x])
}

# max(today$date)

outi %>%
  ggplot(aes(x = radius, y = cases)) +
    geom_line() +
    ggtitle(paste0("Estimate COVID-19 around ", my_people$city[pep], ", ", 
                  my_people$region[pep], 
                  # " -- (Longitude, Latitude) --", 
                  " (", my_people$longitude[pep], ", ", my_people$latitude[pep], 
                  ")")) +
    xlab("Degrees Longitude & Latitude Away") +
    annotate("text", x = 1.5, y = 300, 
             label = paste("data source: coronadatascraper.com", Sys.Date()))
      
date()
Sys.Date()

## add information about miles from degree longitude
radiusi <- my_radius %>% tibble() %>%
  mutate(lon_miles = my_radius * latlongdeg2mile(my_people$latitude[1], my_people$longitude[1]),
         lat_miles = my_radius * round(circumference / 360, 2))
names(radiusi)[1] <- "radius"

my_radius_outi <- merge(radiusi, outi, by = "radius", all = TRUE) %>%
  arrange(desc(deaths), desc(cases))

write_csv(my_radius_outi, paste0("data/", 
                                 format(Sys.time(), "%Y%m%d%H%M"), 
                                 "_my_radius_cds_daily.csv"))


paste0("Estimate COVID-19 around ", my_people$city[pep], ", ", 
       my_people$region[pep], 
       # " -- (Longitude, Latitude) --", 
       " (", my_people$longitude[pep], ", ", my_people$latitude[pep], 
       ")")

