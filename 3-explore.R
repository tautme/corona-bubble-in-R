## EDA
library(tidyverse)
explore_data <- read_csv("data/cds_timeseries_spread.csv")

## are there negative counts?
explore_data %>% names()

dim(explore_data)

explore_data$state %>% is.na() %>% sum()
explore_data %>%
  group_by(country)

## does the timeseries csv data also have the problem of double list growthFactor
## no, they are all from the spre
explore_complete_data <- explore_data %>%
  select(-recovered, -active)

explore_complete_data %>%
  # head(50) %>%
  group_by(state, date) %>%
  summarise(tested = sum(tested, na.rm = TRUE), 
            cases = sum(cases, na.rm = TRUE)) %>%
  filter(state == c("AR")) %>% View()
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

names(data_all)

## 2020 March 28 Saturday 0043
## covid-19 cases on Earth >= 597335 JHU CSSE
## Earth land area ~ 14894000000 m2
## Earth -> Continent -> Country -> State -> Region -> County -> City -> Nieghborhood -> Home -> Room -> Hand -> Nose
## 10^10 -> 10^9      -> 10^8    -> 10^7  -> 10^6   -> 10^5   -> 10^4 -> 10^3         -> 10^2 -> 10   -> 1    -> 0.1
## ~1.5  -> America   -> USA     -> AR    -> NWA    -> Wash.  -> Fay. -> Wilson       -> C8   -> SE   -> Adam -> Breathe
## 574430->              99447 -> 

## Explore my own machines data scrap
cds <- read_csv("/Users/adamhughes/coronadatascraper/dist/data.csv",
                col_types = cols(
                  .default = col_double(),
                  city = col_character(),
                  county = col_character(),
                  state = col_character(),
                  country = col_character(),
                  url = col_character(),
                  maintainers = col_character(),
                  aggregate = col_character(),
                  tz = col_character(),
                  sources = col_character(),
                  curators = col_character()
                ))

dim(cds)
names(cds)
head(cds$tz)

## show me ccsc NA for Earth
cds[is.na(cds$state), ] %>%
  summarise(cases = sum(cases))

cds %>% filter(aggregate == "state") %>% summarize(cases = sum(cases, na.rm = TRUE))
cds %>% filter(aggregate == "county") %>% summarize(cases = sum(cases, na.rm = TRUE))
cds %>% filter(aggregate == "country") %>% summarize(cases = sum(cases, na.rm = TRUE))

## America #####
cds %>% filter(country == "MEX") %>% select(tz)
cds[grepl("America", cds$tz), ] %>% view()
  filter(aggregate == "country") %>% view()
  summarise(cases = sum(cases))

## USA ######
# cds %>% filter(country == "USA", state != "NA") %>%
#   summarise(cases = sum(cases, na.rm = TRUE))

cds_usa_today <- cds %>%
  filter(country == "USA") %>%
  select(city, county, state, cases, deaths, tested, population, lat, long)

cds_usa_today %>%
  arrange(desc(cases))
## 99447 cases in USA

## AR ########
cds %>% filter(aggregate == "county" & state == "AR")




  
  
  
