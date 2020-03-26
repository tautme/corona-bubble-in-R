# corona-bubble-in-R
How many cases of coronavirus are around you? This is an Rsudio project.
Data is downloaded from the Corona Data Scraper project. https://coronadatascraper.com/#home

## Plan
 - get counts of cases by Longitude and Latitude and date
 - create a polygon around your location of ~200 miles
 - sum all cases within that area
 - then display a timeseries plot for that location

i.e. counts defined by bounderies can be misleading for a place like northern New York getting a count of New York cases, when it is more realistic for the people of New Jersey to be concerned.

So far we have a buffer set up with a 3 degrees in longitute and latitude, then the distance in miles is estimated assuming Earth is a sphere. 

The next step will be to set buffer in terms of miles and create a polygon instead of a square. 
Also estimate distance from an elipsoid instead of a sphere.
