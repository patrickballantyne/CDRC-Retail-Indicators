## Catchments for CDRC Retail Centres

library(sf)
library(tidyverse)
library(hereR)
library(tmap)
source("Source Code/Helper Functions.R")
set_key("5zgYrNtYojJ0DPRRAnufXo_dLijIAav_a6-3j-bg768")

# 1. Data - CDRC Retail Centre Boundaries ---------------------------------

## Pull in the 2021 Retail Centre Boundaries > Centroids
rc <- st_read("Output Data/CDRC_Retail_Centres_RECLASSIFIED.gpkg")
retail_centroids <- st_sf(st_transform(st_centroid(st_transform(rc,27700)),4326))

## Identifying the centres we don't want catchments for - removing Small Local Centres
retail_centroids <- retail_centroids %>%
  filter(Classification != "Small Local Centre") %>%
  filter(RC_ID != "RC_EW_3290")

# 2. Drive-Time Catchments ------------------------------------------------

# Catchments are Drive-Time Isolines using the HERE API
# However, code has been modified to reflect new changes w/ HERE API request per second limit
# lapply and Sys.time() are used to bypass the 1 request per second limit

# Catchments are delineated differently based on the position of the centres in the hierarchy
# 20 mins for Regional Centres & Out of Town Shopping Centres
# 15 mins for Major Town Centres & Retail Parks (larger than 25 units)
# 10 mins for Town Centres & Retail Parks (smaller than 25 units)
# 5 mins for District Centres, Local Centres and Market Towns

## Delineating the catchments (fairly slow)
ls <- split(retail_centroids, seq(nrow(retail_centroids))) ## Split each centre into element of list
out <- do.call(rbind, lapply(ls, get_drivetimes)) ## Apply function
st_write(out, "Output Data/CDRC_RetailCentre_2021_DriveTimes.gpkg")

# 3. Walking Catchments ---------------------------------------------------

# Catchments are Walking-based Isolines using the HERE API again, but in a slightly different way to the drive-time catchments.

# These walking catchments are delineated by computing multiple walking catchments from various points within the
# retail centre  boundary, before merging into one large retail centre catchment. This removes the effect of 
# small catchment sizes, and minor issues with pedestrian routing in HERE.

# NOTE: Due to the sheer volume of API requests with this approach, you need to have a HERE developer account with greater
# priveleges than 'Freemium'. This approach utilises the 'Add-on' account, funded by the CDRC. Please contact Patrick Ballantyne
# or Alex Singleton for more information.

# NOTE: The getWalkingCatchment() function is designed to operate in parallel, please switch to a Linux mission, or alter the function
# to remove the mclapply() command.

# The walking catchments are delineated differently based on the position of the centres in the hierarchy
# 15 mins for Regional, Out of Town, Major Town Centres, Town Centres
# 10 mins for 
# 7 mins for Small, Local Centres

## Delineating the catchments
ls2 <- split(rc, seq(nrow(rc)))
out2 <- do.call(rbind, lapply(ls2, getWalkingCatchment))
st_write(out2, "Output Data/CDRC_RetailCentre_2021_WalkingIsolines.gpkg")


