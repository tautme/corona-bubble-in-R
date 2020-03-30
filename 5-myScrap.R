library(tidyverse)
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
## How to get major groups/ major city areas/ groups size of RI
as_factor(cds$tz) %>% levels()
large_cities <- cds %>%
  filter(population > 1E7) %>%
  select(country, cases, population, lat, long, url, aggregate, tz, featureId, curators)

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

## Canada ########    
cds %>% filter(country == "CAN") %>% View()

## USA ######
cds %>% filter(country == "CAN", state != "NA") %>% View()
summarise(cases = sum(cases, na.rm = TRUE))

cds_usa_today <- cds %>%
  filter(country == "USA") %>%
  select(city, county, state, cases, deaths, tested, population, lat, long)
## remove state sums, county == NA
cds_usa_today <- cds_usa_today[!is.na(cds_usa_today$county), ]
## and for long == NA
cds_usa_today <- cds_usa_today[!is.na(cds_usa_today$long), ]

cds_usa_today %>%
  arrange(desc(cases))
## 99447 cases in USA

## AR ########
cds %>% filter(aggregate == "county" & state == "NM") %>% View()


## Map ##########
# install.packages(c("leaflet", "sp"))
library(sp)
library(leaflet)

# dfa <- cds_usa_today
# names(dfa)[8] <- "latitude"
# names(dfa)[9] <- "longitude"

dfa <- data_all
head(dfa)
dfa <- dfa %>% filter(latitude != "NA")
names(dfa)[11] <- "latitude"
names(dfa)[12] <- "longitude"

coordinates(dfa) <- ~longitude+latitude


leaflet(dfa) %>% 
  # addMarkers(popup = paste(df$name, "has", df$cases, "cases of COVID-19,", 
  #                          df$lon_miles, "miles around them. DATA:coronadatascraper")) %>%
  addCircleMarkers(opacity = dfa$population / 1000000, radius = df$cases/50000) %>%
  # addRectangles(lng1 = lng - 3,
  #               lng2 = lng + 3,
  #               lat1 = lat - 3,
  #               lat1 = lat + 3) %>%
  # addCircles(radius = df$cases * df$deaths / 10) %>%
  # addCircleMarkers(radius = 0.01, popup = paste0(dfa$county, ", ", dfa$state, " tested ", dfa$tested, " with ",
  #                                               dfa$cases," cases and ", dfa$deaths, " deaths among ",
  #                                               dfa$population, " people. DATA: coronadatascraper.com")) %>%
  # labelOptions() %>%
  addTiles()




