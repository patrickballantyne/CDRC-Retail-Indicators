## Catchments for CDRC Retail Centres

library(sf)
library(tidyverse)
library(hereR)
source("Source Code/Helper Functions.R")
set_key("fxlFcjInGbtGpLoW0oqS14vGRcpGbEE7Na6R3hykrlk")

# Catchments are Drive-Time Isolines using the HERE API
# However, code has been modified to reflect new changes w/ HERE API request per second limit
# lapply and Sys.time() are used to bypass the 1 request per second limit

# Catchments are delineated differently based on the position of the centres in the hierarchy
# 20 mins for Regional Centres & Out of Town Shopping Centres
# 15 mins for Major Town Centres & Retail Parks
# 10 mins for Town Centres
# 5 mins for District Centres, Local Centres and Market Towns

## Pull in the 2021 Retail Centre Boundaries > Centroids
rc <- st_read("Input Data/Retail_Centres_UK_Final.gpkg")
retail_centroids <- st_sf(st_transform(st_centroid(st_transform(rc,27700)),4326))

## Drop out 'Small Local Centres' (< 100 units)
retail_centroids <- retail_centroids %>% filter(Classification != "Small Local Centre")

## Delineating the catchments (fairly slow)
ls <- split(retail_centroids, seq(nrow(retail_centroids))) ## Split each centre into element of list
out <- do.call(rbind, lapply(ls, get_drivetimes)) ## Apply function
#st_write(out, "CDRC_RetailCentre_2021_DriveTimes.gpkg")
