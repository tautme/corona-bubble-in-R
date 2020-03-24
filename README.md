# corona-bubble-in-R
How many cases of coronavirus are around you? This is an Rsudio project.
Data is downloaded from the Corona Data Scraper project. https://coronadatascraper.com/#home

## Game Plan
 - get counts of cases by Longitude and Latitude and date
 - create a buffer around your location of ~200 miles
 - sum all cases within that area
