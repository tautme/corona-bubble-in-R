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
  # head(50) %>%
  group_by(state, date) %>%
  summarise(tested = sum(tested, na.rm = TRUE), 
            cases = sum(cases, na.rm = TRUE)) %>%
  filter(state == c("Arkansas")) %>%
  ggplot(aes(x = date, y = tested, color = state)) +
    geom_line(aes(x = date, y = cases))

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
  .default = col_character(),
  cases = col_double(),
  deaths = col_double(),
  recovered = col_double(),
  tested = col_double(),
  active = col_double(),
  population = col_double(),
  lat = col_double(),
  long = col_double(),
  rating = col_double(),
  featureId = col_character()
))

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

names(data_snap)
data_snap %>% arrange(desc(cases)) %>% 
  # select(-deaths, -url) %>% 
  View()


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

