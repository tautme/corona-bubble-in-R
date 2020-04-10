## EDA
library(tidyverse)
## timeseries_spread ########
explore_data <- read_csv("raw_data/timeseries.csv", 
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
                           date = col_date(format = "")
                         ))

## timeseries tidy
tidy_data <- read_csv("raw_data/timeseries-tidy.csv", 
                      col_types = 
                        cols(
                          name = col_character(),
                          level = col_character(),
                          city = col_character(),
                          county = col_character(),
                          state = col_character(),
                          country = col_character(),
                          population = col_double(),
                          lat = col_double(),
                          long = col_double(),
                          aggregate = col_character(),
                          tz = col_character(),
                          date = col_date(format = ""),
                          type = col_character(),
                          value = col_double()
                        ))


## are there negative counts?
explore_data %>% names()
explore_data$level %>% as.factor() %>% levels()
dim(explore_data)

explore_data$state %>% is.na() %>% sum()
explore_data %>%
  group_by(country)

## does the timeseries csv data also have the problem of double list growthFactor
## no, they are all from the spread
explore_complete_data <- explore_data %>%
  select(-recovered, -active)

explore_complete_data %>%
  group_by(state, date) %>%
  summarise(tested = sum(tested, na.rm = TRUE), 
            cases = sum(cases, na.rm = TRUE)) %>%
  filter(state == c("Arkansas")) %>%
  ggplot(aes(x = date, y = tested, color = state)) +
    geom_line(aes(x = date, y = cases))

## check issue #673 ##########
## https://github.com/covidatlas/coronadatascraper/issues/673
## https://covid-atlas.slack.com/archives/C0101BDEEDV/p1586281979468800
max(explore_data$date)

## Kansas #698
## https://github.com/covidatlas/coronadatascraper/issues/698

explore_data %>%
  group_by(state, date) %>%
  summarise(tested = sum(tested, na.rm = FALSE),
            cases = sum(cases, na.rm = FALSE)) %>%
  filter(state == c("Kansas")) %>%
  ggplot(aes(x = date, y = cases, color = state)) +
    geom_point() +
    scale_x_date(date_minor_breaks = "1 day")
    scale_x_date(date_breaks = seq("2020-03-23", "2020-04-07"))

explore_state <- explore_data %>%
  filter(state == "Kansas")

explore_state %>%
  group_by(date) %>%
  filter(state == "Kansas") %>%
  summarise(tested = sum(tested, na.rm = FALSE),
            cases = sum(cases, na.rm = FALSE))

explore_data %>%
  group_by(state, date) %>%
  summarise(tested = sum(tested, na.rm = FALSE),
            cases = sum(cases, na.rm = FALSE)) %>%
  filter(state %in% c("Kansas", "Alaska", "New Jersey", "South Dakota")) %>%
  ggplot(aes(x = date, y = cases, color = state)) +
    geom_line() +
    facet_grid(state ~ ., scales = "free")

## South Dakota
## the decrease could be accociated with counting active cases?
explore_data %>%
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
explore_data %>% 
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


## snapshot cds ########
data_snap <- read_csv("raw_data/data.csv", col_types = cols(
        name = col_character(),
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
        publishedDate = col_date(format = "")
      ))
        
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

## Map People ##########
# install.packages(c("leaflet", "sp"))
library(sp)
library(leaflet)
dfc <- check_478 %>% head(20)
names(dfc)
names(dfc)[13] <- "latitude"
names(dfc)[14] <- "longitude"
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
# df <- explore_data %>% filter(date == "2020-03-29", !is.na(lat), cases > 100)
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
names(explore_data)
time_us <- explore_data %>%
  filter(country == "United States") %>%
  filter(date == max(explore_data$date)) %>%
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

