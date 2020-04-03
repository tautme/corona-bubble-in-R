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


adj_co <- adj_co %>% mutate(UID = row_number(), adj.logic = is.na(X1))
adj_co <- adj_co  %>% select(-X1, -X3)
# head(adj_co, 22) %>% as.data.frame()
names(adj_co) <- c("fips", "fips.adjacent", "UID", "adj.logic")
adj_co <- fill(adj_co, fips, .direction = "down")
adj_co <- as.data.frame(adj_co)
# write_csv(adj_co, "data/adjacent_fips_county_usa.csv")

## rearrange #####
# adj_co %>% spread(fips, fips.adjacent)
dt_adj_co <- adj_co
# adj_co %>% group_by(fips)


## stackoverflow question #######
dat <- read.table(header=TRUE, stringsAsFactors=TRUE, text="
                        head          child UID     logic
                    1  01001         01001   1     FALSE
                    2  01001         01021   2      TRUE
                    3  01001         01047   3      TRUE
                    4  01001         01051   4      TRUE
                    5  01001         01085   5      TRUE
                    6  01001         01101   6      TRUE
                    7  01003         01003   7     FALSE
                    8  01003         01025   8      TRUE
                    9  01003         01053   9      TRUE
                    10 01003         01097  10      TRUE
                    11 01003         01099  11      TRUE
                    12 01003         01129  12      TRUE
                    13 01003         12033  13      TRUE
                    14 01005         01005  14     FALSE
                    15 01005         01011  15      TRUE
                    16 01005         01045  16      TRUE
                    17 01005         01067  17      TRUE
                    18 01005         01109  18      TRUE
                    19 01005         01113  19      TRUE
                    20 01005         13061  20      TRUE
                    21 01005         13239  21      TRUE
                    22 01005         13259  22      TRUE")

dat
dat %>% class()
glimpse(dat)

# # make graph ##########
# library(tidyverse)
# library(igraph)
# library(jsonlite)
# gdat <- select(dat, head, child)
# mdat <- as.matrix(gdat)
# edge_dat <- graph_from_edgelist(mdat)
# # plot.igraph(edge_dat)
# jdat <- toJSON(mdat, matrix = "rowmajor")

df_adj_co
glimpse(df_adj_co)
library(data.table)
setDT(dat)
dat %>% class()
glimpse(dat)
dat_child <- dat[(logic)]
dat_child <- dat
dat_child %>% class()
dat_child <- dat_child[,.(list(unique(child))), by = "head"]
dat_child[1,2]
class(dat_child$V1)
dat_child[3,2]
dat_child
dat_child$V1[[3]]

##
##
##

## use stackexchange soultion from linog
names(adj_co)
head(adj_co)
library(data.table)
setDT(adj_co)
adj_co
class(adj_co)
adj_co <- adj_co[("adj.logic")]
head(adj_co)
adj_co_short <- adj_co[,.(list(unique("fips.adjacent"))), by = "fips"]
## You have many options, depending on how you want to handle them later. 
## You can use list columns as you proposed, standard lists (use split(dat_child, by = "head") instead), 
## nested data (either nested data.table or nested tibble). 
## This is really what you will do later that will determine the most appropriate structure. -linog 





## make number
# adj_co$fips <- as.numeric(adj_co$fips)
# adj_co$fips.adjacent <- as.numeric(adj_co$fips.adjacent)

## only arkansas
# adj_co_ar <- adj_co %>% filter(fips >= 5000, fips < 6000, adj.logic == TRUE)

# mtcars
# complete(mtcars, cyl, gear, carb)
# expand(mtcars, cyl, gear, carb)

# ##repeat in X1
# adj_co[grepl("Todd County, MN", adj_co$X1), ]
# # 1 Todd County, MN 27111 Becker County, MN 27005 wrong
# # 2 Todd County, MN 27153 Cass County, MN   27021 correct
# ## find row
# grep("Todd County, MN", adj_co$X1)
# adj_co[9436:9448, ]
# ## remove 9438:9445
# adj_co <- adj_co[-c(9438:9445), ]
# adj_co[9436:9446, ]



## put adjacnet in column of list############
# test <- adj_co %>% head(23)
# # test <- test %>% select(-X2, -X4)
# # test
# # names(test) <- c("county", "county.adjacent", "UID", "adj.logic")
# # test <- test %>% mutate(list.logic = is.na(county))
# test
# ## make a list
# list(c(test[1,2], test[2,2], test[3,2], test[4,2], test[5,2], test[6,2]))
# 
# c_list <- data.frame(co = test[1,1],
#                      ca = I(list(test[1,2], test[2,2], test[3,2], test[4,2], test[5,2], test[6,2])))
# 
# c_list %>% glimpse()
# ## count true to false
# dimen <- dim(test)
# sum(test$list.logic) - dimen[1]



## spread table #######
# test %>% spread()



# make graph ##########
# install.packages("igraph")
# library(igraph)
# test <- adj_co_ar
# # test <- adj_co %>% select(X2, X4)
# gtest <- head(test, 500)
# gtest <- select(gtest, fips, fips.adjacent)
# # gtest <- test %>% select(-list.logic, -UID)
# # gtest <- fill(gtest, county, .direction = "down")
# # gtest <- fill(test, X2, .direction = "down")
# mtest <- as.matrix(gtest)
# class(mtest)
# edge_test <- graph_from_edgelist(mtest)
# class(edge_test)
# # # coords <- layout_(edge_test, as_star())
# # coords <- layout_with_lgl(edge_test)
# # plot.igraph(edge_test, layout = coords)
# 
# plot.igraph(edge_test)
# 
# head(edge_test)
# edge_json <- toJSON(mtest, matrix = "rowmajor")
# write_json(edge_json, "data/edge_test.json")





##
# adj_co <- data.frame(adj_co)

# rownames(adj_co) <- adj_co$UID
# head(adj_co, 13)
## need adj in column as list


# names(adj_co) <- c("")
## to JSON
# library(jsonlite)
# co_boundary <- read_json("raw_data/geoJSON_test_boundary.json")
# co_boundary %>% glimpse()

# adj_co_json <- toJSON(adj_co, pretty = FALSE)
# cat(adj_co_json) %>% head()
# class(adj_co_json)
# head(adj_co_json)
# write_json(adj_co_json, "data/county_adjacency.json")
# 
# 
# # Stringify some data
# jsoncars <- toJSON(mtcars, pretty=TRUE)
# head(jsoncars)
# cat(jsoncars) %>% head()
# write_json(jsoncars, "data/jsoncars.json")


# adj_co <- adj_co %>% mutate(X5 = X2)
# adj_co
# adj_co <- adj_co %>% fill(X1, .direction = "down")
# adj_co
# adj_co <- adj_co %>% fill(X2, .direction = "down")
# adj_co
# ## remove the duplicate for county to itself?
# # adj_co %>% filter(X5 != "NA")
# tail(adj_co)
# ## well, there are islands... and they are alone... maybe the radius is a better 
# ## otherwise, you would want to know the county counts for travelers to your island.
# 
# ## lets move on to make a nested list and then try to ouput as JSON
# fip_adj_co <- adj_co %>% select(X2, X4)
# names(fip_adj_co) <- c("fips", "adj_fips")
# fip_adj_co
# list




# fips_adj_co <- adj_co %>% select(X2, X4)
# names(fips_adj_co) <- c("fips", "adjacent_fips")
# fips_adj_co <- fill(fips_adj_co, fips, .direction = "down")



## from stackoverflow 
## https://stackoverflow.com/questions/38376624/how-to-transform-a-messy-list-into-multiple-adjacency-lists-or-an-edge-list-in

# df <- read.csv("raw_data/county_adjacency.txt", sep="\t", stringsAsFactors = FALSE, header = FALSE)
# 
# # Drop the names for the counties, you don't need them      
# df <- df[ ,c("V2","V4")]
# install.packages("zoo")
# library(zoo)
# df$V2 <- na.locf(df$V2)
# dim(df)
# fips <-unique(df$V2)
# length(fips)
# fips.matrix <- matrix(data=0, nrow = length(fips), ncol = length(fips), dimnames = list(fips,fips))
# df <- as.character(df)
# class(df)
# fips.matrix[as.matrix(df)] <- 1
# class(fips.matrix)
# df[1][1][1]


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
