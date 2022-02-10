## Extracting Typology Variables - Open Variables

library(lwgeom)
library(sf)
library(tidyverse)
library(tmap)
options(scipen = 999)


# 1. Existing Indicators --------------------------------------------------

## Read in data
rc <- st_read("Output Data/CDRC_RetailCentre_Boundaries.gpkg")
rc <- rc %>% 
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

## Size
rc$area <- st_area(rc)
rc$area <- as.numeric(rc$area / 1000000)

## Morphology - Roeck Score

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


