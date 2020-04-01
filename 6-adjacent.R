## import geojson
## find adjacent counties
## make output json file with adjacent county list for each county
## Adam Hughes
## 2020-03-31-18-29

library(tidyverse)

# download.file("https://www2.census.gov/geo/docs/reference/county_adjacency.txt", "raw_data/county_adjacency.txt")

adj_co <- read_tsv("raw_data/county_adjacency.txt", col_names = FALSE,
                  col_types = cols(
                    X1 = col_character(),
                    X2 = col_character(),
                    X3 = col_character(),
                    X4 = col_character()
                  ))
adj_co
adj_co <- adj_co %>% mutate(X5 = X2)
adj_co
adj_co <- adj_co %>% fill(X1, .direction = "down")
adj_co
adj_co <- adj_co %>% fill(X2, .direction = "down")
adj_co
## remove the duplicate for county to itself?
adj_co %>% filter(X5 != "NA")
tail(adj_co)
## well, there are islands... and they are alone... maybe the radius is a better 
## otherwise, you would want to know the county counts for travelers to your island.



# fips_adj_co <- adj_co %>% select(X2, X4)
# names(fips_adj_co) <- c("fips", "adjacent_fips")
# fips_adj_co <- fill(fips_adj_co, fips, .direction = "down")










## county json file from https://eric.clst.org/tech/usgeojson/
# library(jsonlite)
# county_json <- read_json("raw_data/test.json")
# ## without the ? characters it works
# county_json %>% class()
# # bad at Comer*0 line 1403
# 
# county_json$features[[2]]$geometry$coordinates[[1]][[1]][[2]]
# county_json$features[[2]]$properties$GEO_ID
# ## does this one match "any" other, thats a lot of running
# 
# 
# ##
# ## try another json input method
# ##
# r_object <- fromJSON(readLines("raw_data/gz_2010_us_050_00_20m.json"))
# class(r_object)
# r_object$features$properties$GEO_ID[1]
# r_object$features$geometry$coordinates[1][1]
# r_object$type
# class(r_object$features)
# 
# features <- r_object$features
# ## multipoligon is an area with islands that are not connected by coordinates, so idea 1 would not work...
# names(features)
# # features <- as_tibble(features)
# features$geometry$type %>% as.factor() %>% levels()
# 
# ## 13 and 14
# features$geometry$coordinates[13] %>% class() ## polygon
# features$geometry$coordinates[14] %>% class() ## multipolygon
# 
# ## same list but multi has more sets
# features$geometry$coordinates[14]
# features$geometry$coordinates[14][[1]][[1]][[1]] ## -173.1169
# head(features, 20)
# 
# features$geometry$coordinates[1][[1]][[1]]
# 
# library(data.table)
# features <- rbindlist(features, fill=TRUE)
