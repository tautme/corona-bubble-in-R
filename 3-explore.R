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



