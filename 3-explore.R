## EDA
library(tidyverse)

## load data ########
## timeseries_spread
time_data <- read_csv("raw_data/timeseries.csv", 
                         col_types = cols(
                           city = col_character(),
                           county = col_character(),
                           state = col_character(),
                           country = col_character(),
                           population = col_double(),
                           lat = col_double(),
                           long = col_double(),
                           url = col_character(),
                           aggregate = col_character(),
                           tz = col_character(),
                           cases = col_double(),
                           deaths = col_double(),
                           recovered = col_double(),
                           active = col_double(),
                           tested = col_double(),
                           date = col_date(format = ""),
                           hospitalized = col_double(),
                           discharged = col_character()
                         ))


## timeseries jhu style
time_jhu <- read_csv("raw_data/timeseries-jhu.csv",
                     col_types = cols(
                       .default = col_double(),
                       name = col_character(),
                       level = col_character(),
                       city = col_character(),
                       county = col_character(),
                       state = col_character(),
                       country = col_character(),
                       url = col_character(),
                       aggregate = col_character(),
                       tz = col_character()
                     ))

## snapshot cds 
data_snap <- read_csv("raw_data/data.csv", 
                      col_types = cols(
                        name = col_character(),
                        city = col_character(),
                        cases = col_double(),
                        deaths = col_double(),
                        recovered = col_double(),
                        tested = col_double(),
                        active = col_double(),
                        population = col_double(),
                        populationDensity = col_double(),
                        lat = col_double(),
                        long = col_double(),
                        rating = col_double(),
                        hospitalized = col_double(),
                        publishedDate = col_character()
                      ))


## are there negative counts?
time_data %>% names()
time_data$level %>% as.factor() %>% levels()
dim(time_data)

time_data$state %>% is.na() %>% sum()
time_data %>%
  group_by(country)

## does the timeseries csv data also have the problem of double list growthFactor
## no, they are all from the spread
explore_complete_data <- time_data %>%
  select(-recovered, -active)

explore_complete_data %>%
  filter(level == "county") %>%
  group_by(state, date) %>%
  summarise(tested = sum(tested, na.rm = TRUE), 
            cases = sum(cases, na.rm = TRUE)) %>%
  filter(state == c("Arkansas")) %>%
    ggplot(aes(x = date, y = tested, color = state)) +
      geom_point(aes(x = date, y = cases))

## check issue #673 ##########
## https://github.com/covidatlas/coronadatascraper/issues/673
## https://covid-atlas.slack.com/archives/C0101BDEEDV/p1586281979468800
max(time_data$date)

## Kansas #698
## https://github.com/covidatlas/coronadatascraper/issues/698

time_data_tidy <- time_data %>%
  group_by(state, date)

# ## is there a kansas state total?
# time_data %>%
#   filter(level == "county") %>%
#   filter(state == c("Kansas")) %>% 
#   select(date, name, level, cases, deaths, url) %>%
#   view()
# ## YES! and it is added to my plot below!

## county rollup
time_data %>%
  filter(level == "county", date > "2020-03-11") %>%
  group_by(state, date) %>%
  filter(state == c("Kansas")) %>%
  summarise(tested = sum(tested, na.rm = FALSE),
            cases = sum(cases, na.rm = FALSE)) %>%
  ggplot(aes(x = date, y = cases, color = state)) +
    geom_point() +
    # geom_point(aes(y = tested)) +
    scale_x_date(date_minor_breaks = "1 day")
    # scale_x_date(date_breaks = seq("2020-03-23", "2020-04-09"))
    
## straight state
time_data %>%
  filter(level == "state", state == "Kansas", date > "2020-03-11") %>%
    ggplot(aes(x = date, y = cases, color = state)) +
      geom_point() +
      # geom_point(aes(y = tested)) +
      scale_x_date(date_minor_breaks = "1 day")
## do not match | kansas mar. 19 - 22 and apr. 1
## match but anomolius mar. 27 & 31

## data.csv today match these?
names(data_snap)
unique(data_snap$level)

data_snap %>%
  filter(level == "state", state == "Kansas") %>%
  select(cases)

data_snap %>%
  filter(level == "county", state == "Kansas") %>%
  # group_by(county) %>%
  summarize(total = sum(cases))

data_snap %>%
  filter(level == "country") %>%
  arrange(desc(cases))


## use tidy to map each type
# names(tidy_data)

# tidy_data %>% 
#   filter(level == "state", state == "Kansas", date > "2020-03-14", 
#          # type != "growthFactor",
#          type == "cases") %>%
#     ggplot(aes(x = date, y = value, color = type)) +
#       geom_point() +
#       scale_x_date(date_minor_breaks = "1 day")

# tidy_data %>% 
#   filter(level == "county", state == "Kansas", date > "2020-03-14") %>%
#   spread(type, value) %>%
#   group_by(date, state) %>%
#   summarise(cases = sum(cases)) %>%
#     ggplot(aes(x = date, y = cases)) +
#       geom_point() +
#       scale_x_date(date_minor_breaks = "1 day")

explore_state <- time_data %>%
  filter(state == "Kansas")

explore_state %>%
  group_by(date) %>%
  filter(state == "Kansas") %>%
  summarise(tested = sum(tested, na.rm = FALSE),
            cases = sum(cases, na.rm = FALSE))

time_data %>%
  group_by(state, date) %>%
  summarise(tested = sum(tested, na.rm = FALSE),
            cases = sum(cases, na.rm = FALSE)) %>%
  filter(state %in% c("Kansas", "Alaska", "New Jersey", "South Dakota")) %>%
  ggplot(aes(x = date, y = cases, color = state)) +
    geom_point() +
    facet_grid(state ~ ., scales = "free")

time_data %>%
  group_by(state, date) %>%
  summarise(tested = sum(tested, na.rm = FALSE),
            cases = sum(cases, na.rm = FALSE)) %>%
  filter(state %in% c("Kansas", "Alaska", "New Jersey", "South Dakota")) %>%
  ggplot(aes(x = date, y = cases, color = state)) +
  geom_point() +
  facet_grid(state ~ ., scales = "free")

## Check a State ##########
## do timeseries and tidy match?
# test <- "New Jersey"
# test <- "South Dakota"
# test <- "Alaska"
# test <- "Kansas"
test <- "California"
# test <- "New York"

data_snap %>%
  filter(level == "county", state == test) %>%
  summarise(total = sum(cases))

data_snap %>%
  filter(level == "state", state == test) %>%
  summarise(total = sum(cases))

## county 

# tidy_data %>% 
#   filter(level == "county", state == test, date > "2020-03-15") %>% 
#   spread(type, value) %>% 
#   group_by(date, state) %>%
#   summarise(cases = sum(cases)) %>% 
#     ggplot(aes(x = date, y = cases)) +
#       geom_point() +
#       scale_x_date(date_minor_breaks = "1 day")

time_data %>% 
  filter(level == "county", state == test, date > "2020-03-19") %>% 
  group_by(date, state) %>%
  summarise(cases = sum(cases, na.rm = TRUE)) %>% 
    ggplot(aes(x = date, y = cases)) +
      geom_point() +
      scale_x_date(date_minor_breaks = "1 day")

time_jhu %>%
  gather("2020-01-22":"2020-04-19", key = date, value = cases) %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  filter(level == "county", date > "2020-03-19", state == test) %>%
  group_by(state, date) %>%
  summarise(cases = sum(cases, na.rm = TRUE)) %>%
    ggplot(aes(x = date, y = cases)) +
      geom_point() +
      scale_x_date(date_minor_breaks = "1 day")

## state

data_snap %>%
  filter(level == "state", state == test) %>%
  select(name, cases, deaths)

# max(tidy_data$date)

# tidy_data %>% 
#   filter(level == "state", state == test, type == "cases") %>% 
#   select(value) %>% max()

# tidy_data %>% 
#   filter(level == "state", state == test, date > "2020-03-15", 
#          type == "cases") %>%
#     ggplot(aes(x = date, y = value, color = type)) +
#       geom_point() +
#       scale_x_date(date_minor_breaks = "1 day")

max(time_data$date)

time_data %>%
  filter(level == "state", date > "2020-03-19", state == test) %>%
    ggplot(aes(x = date, y = cases, color = state)) +
      geom_point() +
      scale_x_date(date_minor_breaks = "1 day")

time_jhu %>%
  gather("2020-01-22":"2020-04-19", key = date, value = cases) %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  filter(level == "state", date > "2020-03-19", state == test) %>%
    ggplot(aes(x = date, y = cases)) +
      geom_point() +
      scale_x_date(date_minor_breaks = "1 day")

## see last date
time_jhu %>%
  gather("2020-01-22":"2020-04-12", key = date, value = cases) %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  filter(level == "state", date > "2020-03-11", state == test) %>%
  arrange(desc(date)) %>%
  select(name, date, cases)
  



## South Dakota
## the decrease could be accociated with counting active cases?
time_data %>%
  filter(state == "South Dakota", date > "2020-04-01") %>% 
  group_by(county, date) %>%
  summarise(tested = sum(tested, na.rm = FALSE),
            cases = sum(cases, na.rm = FALSE)) %>%
  ggplot(aes(x = date, y = cases, color = county)) +
  geom_line() #+
  # facet_grid(~ . county, scales = "free")

## data.csv
data_snap %>%
  filter(state == "South Dakota") %>%
  summarise(total = sum(cases, na.rm = TRUE))

## county level
data_snap %>% 
  filter(state == "South Dakota") %>%
  filter(level == "county") %>%
  select(name, cases, deaths)
 
## individual county
# 'Aurora County',    'Beadle County',    'Bennett County',    'Bon Homme County',    'Brookings County',    'Brown County',    'Brule County',    'Buffalo County',    'Butte County',    'Campbell County',    
# 'Charles Mix County',    'Clark County',    'Clay County',    'Codington County',    'Corson County',    'Custer County',    'Davison County',
# 'Day County',    'Deuel County',    'Dewey County',    'Douglas County',    'Edmunds County',    'Fall River County',    'Faulk County',    'Grant County',    'Gregory County',    'Haakon County',   'Hamlin County',
# 'Hand County',    'Hanson County',    'Harding County',    'Hughes County',    'Hutchinson County',    'Hyde County',    'Jackson County',  
# 'Jerauld County',    'Jones County',    'Kingsbury County',    'Lake County',    'Lawrence County',    'Lincoln County',    'Lyman County',    'Marshall County',    'McCook County',
# 'McPherson County',    'Meade County',    'Mellette County',    'Miner County',    'Minnehaha County',    'Moody County',    'Oglala Lakota County',   
# 'Pennington County',    'Perkins County',    'Potter County',  'Roberts County',    'Sanborn County',   'Spink County',    'Stanley County',    'Sully County',    'Todd County',    'Tripp County',    
# 'Turner County',    'Union County',    'Walworth County',    'Yankton County',    'Ziebach County'
data_snap %>% 
  mutate(line = rownames(data_snap)) %>%
  filter(state == "South Dakota", county == "Minnehaha County") %>%
  select(line, name, cases, deaths)
time_data %>% 
  filter(date == "2020-04-06", state == "South Dakota", county == "Minnehaha County") %>%
  select(line, name, cases, deaths)

## yarn start --location "Minnehaha County, SD, USA"
tidy_data %>% 
  filter(type == "cases",
         date == "2020-04-05",
         # state == "South Dakota", 
         county == "Minnehaha County") %>%
  select(name, type, value)

names(tidy_data)
head(tidy_data)
# explore_complete_data %>%
#   filter(state == "FL")
## some states have multiple reports differentiated by coordinates, no city or county.
# mdate <- max(explore_complete_data$date)
# explore_complete_data %>%
#   filter(date == mdate) %>%
#   group_by(state) %>%
#   summarise(tested = sum(tested), cases = sum(cases)) %>% View()

## cumulative crap!! this is not how humans read new news

# names(data_all)

## 2020 March 28 Saturday 0043
## covid-19 cases on Earth >= 597335 JHU CSSE
## Earth land area ~ 14894000000 m2
## Earth -> Continent -> Country -> State -> Region -> County -> City -> Nieghborhood -> Home -> Room -> Hand -> Nose
## 10^10 -> 10^9      -> 10^8    -> 10^7  -> 10^6   -> 10^5   -> 10^4 -> 10^3         -> 10^2 -> 10   -> 1    -> 0.1
## ~1.5  -> America   -> USA     -> AR    -> NWA    -> Wash.  -> Fay. -> Wilson       -> C8   -> SE   -> Adam -> Breathe
## 574430->              99447 -> 



        
## where is Brazil?
data_snap %>%
  filter(country == "Brazil")

## hash Kansas
hash_kansas <- read_csv("/Users/adamhughes/Documents/coronadatascraper/coronadatascraper-cache/2020-4-9/9cd650122f1e65a5a95a77187448db0c.csv")
names(hash_kansas)
glimpse(hash_kansas)

sum(hash_kansas$Cases)

## can tz have a format?
# data_snap$tz[[58]] %>% col_time()
#   parse_date(format = "%Z")
#   locale(tz = "UTC")
# col_time(format = "%Z")
# 
# us_central <- locale(tz = "US/Central")
# parse_datetime("1979-10-14T1010", locale = us_central)


## how does the aggregate level work?
data_snap %>%
  filter(country == "United States", 
         # aggregate == "county",
         cases > 90000) %>% view()

data_snap %>%
  group_by(level) %>%
  summarise(total = sum(cases, na.rm = TRUE))

data_snap$level %>% as_factor() %>% levels()

data_snap %>% arrange(city)

data_snap %>% filter(aggregate == "state") %>% arrange(desc(cases))

data_snap %>% arrange(desc(cases)) %>% select(-deaths, -url, -featureId)

## understatnd the aggregate column, which is primarily used for mapping. 
timeseries_data %>% filter(country == "United States") %>% View()

timeseries_data %>% arrange(desc(cases)) %>% select(-deaths, -url) %>% View()

## check issue # 478 ########
## COOR AROUND 0 & 180 LONGITUDE 
names(data_snap)
glimpse(data_snap)
check_478 <- data_snap %>%
  filter(population != "NA") %>%
  arrange(abs(long))

iso1 <- read_csv("/Users/adamhughes/Documents/country-levels/coor_check.csv")

check_iso <- iso1$countrylevel_id

data_snap %>% filter(countryId %in% as_vector(check_iso), level == "country") %>%
  arrange(desc(population)) %>% 
  select(name, countryId, lat, long) %>% View()

data_snap %>%
  mutate(line_number = rownames(data_snap)) %>%
  select(line_number, name, cases, population, lat, long, url, aggregate, tz, deaths) %>%
  filter(population != "NA") %>%
  arrange(abs(long)) %>% View()

  ## USA tz in africa/algers
data_snap %>% 
  mutate(UID = rownames(data_snap)) %>%
  filter(country == "United States", aggregate == "state", population == 325145963) %>% View()

## plot long, lat

## less than 5 ###############
## sudoku
time_data %>%
  filter(level == "county", state == "Arkansas", date == "2020-03-25") %>%
  view()
## Arknasas started giving <5 number on April 14th
time_data %>%
  filter(level == "county", state == "Rhode Island", date == "2020-03-21") %>%
  view()

## check random 2 3
test <- seq(5, 100, by = 1)
test <- (sample.int(101,size=100,replace=TRUE)-1)
a <- sum(test)

sample(c(1:4), size = 4, replace = F)[1]




## Map People ##########
# install.packages(c("leaflet", "sp"))
library(sp)
library(leaflet)
dfc <- check_478 %>% head(20)
names(dfc)
names(dfc)[14] <- "latitude"
names(dfc)[15] <- "longitude"
coordinates(dfc) <- ~longitude+latitude

leaflet(dfc) %>% 
  addMarkers(popup = paste(dfc$name, "has", dfc$cases, "cases of COVID-19,", 
                           dfc$deaths, "deaths,", dfc$tested, "tested")
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
  # addRectangles(lat2 = my_people_out_usa$latitude[pep] + lat_buffer,
  #               lat1 = my_people_out_usa$latitude[pep] - lat_buffer,
  #               lng2 = my_people_out_usa$longitude[pep] + lon_buffer, 
  #               lng1 = my_people_out_usa$longitude[pep] - lon_buffer, 
  #               popup = paste("Estimate from county level data points --" , 
  #                             "(2 degree Longitude, 2 degree Latitude square) In this", 
  #                             my_people_out_usa$lon_miles[pep] * 2, "mile wide and ", 
  #                             my_people_out_usa$lat_miles[pep] * 2, 
  #                             "mile tall area, there are an estimated <B>", 
  #                             my_people_out_usa$cases[pep], 
  #                             "</B> confirmed cases of COVID-19.", 
  #                             "DATA:https://coronadatascraper.com")) %>%
  addTiles()







## Explore
data_snap %>% arrange(desc(cases)) %>% 
  # select(-deaths, -url) %>% 
  filter(country == "ITA") %>% View()

timeseries_data %>% arrange(desc(cases)) %>% 
  # select(-deaths, -url) %>% 
  filter(country == "ITA") %>% View()

data_snap %>% filter(country == "United States", state == "California") %>% arrange(desc(cases))
## CA sum row = 5550 cases
data_snap %>% filter(country == "United States", state == "California", !is.na(county), is.na(city)) %>% summarise(cases = sum(cases))
## CA sum of county observations cases = 5551

## top 10 countries
data_snap %>% filter(is.na(state), is.na(county), is.na(city)) %>% summarise(cases = sum(cases)) %>% arrange(desc(cases))
## Earth sum of countries = 654766 cases in 178 countries
earth <- data_snap %>% filter(is.na(state), is.na(county)) 
earth$country %>% as_factor() %>% levels()

data_snap %>% filter(level == "state")
data_snap$level %>% as_factor() %>% levels()
data_snap %>% filter(level == "state") %>% summarise(cases = sum(cases, na.rm = TRUE))
## some NA in state
data_snap %>% filter(level == "county") %>% summarise(cases = sum(cases))
data_snap %>% filter(level == "country") %>% summarise(cases = sum(cases))

## USA only
data_snap %>% filter(country == "United States", aggregate == "state") %>% summarise(cases = sum(cases))


## County ####
## we only have data down to county level, so which counties are over 1000 cases?
## check that it is only county
## why are some lat lon 0 or missing?
df <- data_snap %>% filter(level == "county", cases >= 1000, !is.na(lat)) %>%
  arrange(desc(cases))
names(df)
names(df)[13] <- "latitude"
names(df)[14] <- "longitude"

## what about fast rate change
# df <- time_data %>% filter(date == "2020-03-29", !is.na(lat), cases > 100)
# names(df)
# names(df)[6] <- "latitude"
# names(df)[7] <- "longitude"

## Map ##########
# install.packages(c("leaflet", "sp"))
library(sp)
library(leaflet)

coordinates(df) <- ~longitude+latitude

leaflet(df) %>% 
  addCircleMarkers(popup = paste(df$county, "--", df$state, "has", df$cases, "cases of COVID-19. DATA:coronadatascraper.com", df$url)) %>%
  # addCircleMarkers(radius = 10) %>%
  # addRectangles(lng1 = lng - 3,
  #               lng2 = lng + 3,
  #               lat1 = lat - 3,
  #               lat1 = lat + 3) %>%
  # addCircles(radius = df$cases * df$deaths / 10) %>%
  # labelOptions() %>%
  addTiles()


## area #######
## at state level
library(datasets)

state.area # land in square miles
state.name

area <- tibble(state.name, state.area)

## time
names(time_data)
time_us <- time_data %>%
  filter(country == "United States") %>%
  filter(date == max(time_data$date)) %>%
  filter(level == "state")

names(time_us)
names(time_us)[5] <- "state.name"

time_pop_us <- merge(time_us, area, by = "state.name", all = TRUE)
time_pop_us <- as_tibble(time_pop_us)
## NA in area?
time_pop_us <- time_pop_us[!is.na(time_pop_us$state.area), ]
## Alaska NA ?
time_pop_us <- time_pop_us[!is.na(time_pop_us$cases), ]
tail(time_pop_us)
max(time_pop_us$date)
min(time_pop_us$date)
names(time_pop_us)
state_count_area <- time_pop_us %>%
  mutate(d_rate_area = deaths / state.area, 
         c_rate_area = cases / state.area,
         r_rate_area = recovered / state.area,
         t_rate_area = tested / state.area) %>%
  select(-county, -date, -long, -lat, -url, tz) %>%
  arrange(desc(c_rate_area)) 


state_count_area <-state_count_area %>% select(name, population, cases, deaths, recovered, 
                                               tested, state.area, d_rate_area, c_rate_area, r_rate_area, t_rate_area)
state_count_area
## woops! I have been summing them all along and they are already cumulative count by date

write_csv(state_count_area, "data/state_case_area.csv")

# read_csv("output/rate_change_day.csv")

## state is really too big for area ratio. maybe a county is too?

