## Extracting Typology Variables - Open Variables

library(lwgeom)
library(sf)
library(tidyverse)
library(tmap)
options(scipen = 999)


# 1. Existing Indicators --------------------------------------------------

## Read in data
rc_sf <- st_read("Output Data/CDRC_RetailCentre_Boundaries.gpkg")
rc <- rc_sf %>% 
  select(RC_ID, RC_Name)

## Read in additional indicators
ind <- read.csv("Output Data/CDRC_RetailCentre_Indicators.csv")
ind <- ind %>%
  select(RC_ID, RC_Name, n.LDC.2020, 
         propVacant, PropStructuralVacant, propVacantChange,
         onlineExposure) %>%
  rename(propStructuralVacant = PropStructuralVacant)

## Merge on 
rc <- merge(rc, ind, by = c("RC_ID", "RC_Name"))

# 2. New Indicators -------------------------------------------------------

# 2.1. Size and Function Domain ------------------------------------------------

## Centre size
rc$area <- st_area(rc)
rc$area <- as.numeric(rc$area / 1000000)

## Catchment size
catchment_sf <- st_transform(st_read("Output Data/CDRC_RetailCentre_2021_DriveTimes.gpkg"), 27700)
catchment <- catchment_sf %>%
  select(RC_ID) %>%
  st_transform(27700)
catchment$area <- st_area(catchment)
catchment$area <- as.numeric(catchment$area / 1000000)
catchment <- catchment %>% 
  as.data.frame() %>%
  select(RC_ID, area) %>%
  rename(catchment_area = area)
rc <- merge(rc, catchment, by = "RC_ID", all.x = TRUE)

## Centre morphology
### Compute minimum bounding circle and it's area
circ <- st_minimum_bounding_circle(rc)
circ$m_area <- st_area(circ)
circ$m_area <- as.numeric(circ$m_area / 1000000)
### Calculate roeck score
circ <- circ %>%
  select(RC_ID, area, m_area) %>%
  mutate(roeck = area / m_area)
circ$roeck <- scales::rescale(circ$roeck, to = c(0, 1))
circ <- circ %>% as.data.frame() %>% select(RC_ID, roeck)
rc <- merge(rc, circ, by = "RC_ID")


# 2.2. Economic health domain ---------------------------------------------

## Number of competing destinations
rc_sub <- rc_sf %>% filter(Classification != "Small Local Centre")
searches <- c("Regional Centre", "Major Town Centre", "Town Centre")

### Function that calculates the total number of directly competing retail centres in the
### retail centre catchments
assessCompetition <- function(x) {
  
  ## Get list of centres within catchment
  obs <- st_intersection(x, rc_sub)
  
  ## Calculate number of direct competitors - varies depending on position in hierarchy
  if (x$Classification %in% searches) {
    obs <- obs %>%
      as.data.frame() %>%
      filter(Classification.1 %in% searches) %>%
      select(RC_ID, RC_ID.1) %>%
      filter(RC_ID.1 != x$RC_ID) %>%
      group_by(RC_ID) %>%
      summarise(n_competing = n_distinct(RC_ID.1))
    obs
  } else if (!x$Classification %in% searches) {
    obs <- obs %>% 
      as.data.frame() %>%
      select(RC_ID, RC_ID.1) %>%
      filter(RC_ID.1 != x$RC_ID) %>%
      group_by(RC_ID) %>%
      summarise(n_competing = n_distinct(RC_ID.1))
    obs
  }
  return(obs)
}

### Run it for all retail centres
competitionList <- do.call(rbind, lapply(split(catchment_sf, catchment_sf$RC_ID), assessCompetition))
rc <- merge(rc, competitionList, by = "RC_ID", all.x = TRUE)
rc <- rc %>%
  mutate(n_competing = coalesce(n_competing, 0))


## Crime rates

## Unemployment rates

## Affluence

## Night-time population

## Day-time population

## Visitors

## Rateable value




