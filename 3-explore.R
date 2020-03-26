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
  select(-recovered, -active) %>%
  na.omit()

explore_complete_data %>%
  head(50) %>%
  group_by(state, date) %>%
  summarise(tested = sum(tested), cases = sum(cases)) %>%
  ggplot(aes(x = date, y = tested, color = state)) +
    geom_line()

explore_complete_data %>%
  filter(state == "FL")
## some states have multiple reports differentiated by coordinates, no city or county.
mdate <- max(explore_complete_data$date)
explore_complete_data %>%
  filter(date == mdate) %>%
  group_by(state) %>%
  summarise(tested = sum(tested), cases = sum(cases)) %>% View()

## cumulative crap!! this is not how humans read new news

