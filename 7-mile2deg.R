library(tidyverse)

## radius from angle then circumference
## for longitude by radius
####
## NOTICE: estimation, assuming earth is a perfect sphere, NOT ELLIPSOID!
####

latlongdeg2mile <- function(lat, long){
  
  ## Latitude 
  R <- 3959 # mean radius of Earth in miles
  circumference <- 2 * pi * R
  
  ## The 1 degree of 360 is
  D_lat_1_deg <- circumference / 360
  round(D_lat_1_deg, 2)
  
  ## Longitude
  ## This is the same for one degree at the equator (Latitude = 0)
  ## But as abs(Latitude) goes to 90 the circumference goes to 0
  
  # lat <- 34
  lat <- abs(lat)
  lat
  ## at lat = 0, then D_lat_1_deg == D_lon_1_deg
  ## https://www.nhc.noaa.gov/gccalc.shtml
  ## at lat = 34, then one degree lon is 50 nautical miles
  ## at lat = 84, then one degree lon is 06 nautical miles
  
  ## calculate radius first
  ## radius is the angle theta (90 - lat) | opposite (R_lat) over hypot (R)., 
  ##         with hypot. = R
  ## sin(theta) = opposite / hypot. = R_lat / R
  radian <- (90 - lat) * 2 * pi / 360
  R_lat <- sin(radian) * R
  R_lat
  circumference_lat <- 2 * pi * R_lat
  ## at set latitude (lat)
  D_lon_1_deg <-  circumference_lat / 360
  # D_lon_1_deg <-  (circumference / (lat * 4)) / 3.6 
  # where x is the angle of latidude
  round(D_lon_1_deg, 2)
  
  # print(paste("one degree latitude =", round(D_lat_1_deg), "miles",
  #             "one degree longitude =", round(D_lon_1_deg), "miles"))
}


latlongdeg2mile(lat = 0, long = -93)

## Latitude 
R <- 3959 # mean radius of Earth in miles
circumference <- 2 * pi * R
circumference / 360
