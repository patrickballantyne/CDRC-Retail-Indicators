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

## Affluence -------------------------------------------------------------------------------

## Use the deprivation profiles for walking catchments
imd <- st_read("Output Data/Deprivation/CDRC_RetailCentre_WalkingDeprivation.gpkg")
imd <- imd %>%
  as.data.frame() %>%
  select(RC_ID, AvgIMDScore) %>%
  rename(affluence = AvgIMDScore)
rc <- merge(rc, imd, by = "RC_ID", all.x = TRUE)


## Night-time population -------------------------------------------------------------------

### England & Wales
eng_wal <- readxl::read_excel("Input Data/2019_EW_estimates.xlsx", sheet = 4, skip = 5)
eng_wal <- eng_wal %>%
  select(1:2, 7) %>%
  setNames(c("Area_Code", "Area_Name", "Total_Population_2019")) %>%
  mutate_if(is.character, as.factor) %>%
  drop_na()

### Scotland
scot <- readxl::read_excel("Input Data/2019_S_estimates.xlsx", sheet = 1,  skip = 5)
scot <- scot %>%
  select(1:2, 4) %>%
  setNames(c("Area_Code", "Area_Name", "Total_Population_2019")) %>%
  mutate_if(is.character, as.factor) %>%
  drop_na()

### UK Population
pop <- rbind(eng_wal, scot)

### Shapefiles
pop_shp <- st_read("Output Data/Deprivation/GB_Deprivation.gpkg")
pop_shp <- pop_shp %>% select(Area_Code, geom)
pop <- merge(pop_shp, pop, by = "Area_Code")
st_write(pop, "Output Data/2019_Population.gpkg")

## Calculate the total population of the walking cacthments
w_catch <- st_read("Output Data/CDRC_RetailCentre_2021_WalkingIsolines_v4.gpkg")
w_catch <- w_catch %>%
  filter(RC_ID %in% rc$RC_ID)
int <- st_intersection(w_catch, pop)
int_out <- int %>%
  as.data.frame() %>%
  select(RC_ID, Area_Code, Total_Population_2019) %>%
  group_by(RC_ID) %>%
  summarise(Population = sum(Total_Population_2019)) %>%
  rename(nightPopulation = Population)
rc <- merge(rc, int_out, by = "RC_ID", all.x = TRUE)

## Day-time population ---------------------------------------------------------

### Read in the 2011 WZ Population Data
wz_pop <- read.csv("Input Data/WZ/WZ_Population_2011.csv")
wz_pop <- wz_pop %>%
  select(3:4) %>%
  setNames(c("Area_Code", "Population"))
wz_shp <- merge(pop_shp, wz_pop, by = "Area_Code", all.x = TRUE)
wz_shp <- wz_shp %>%
  filter(!is.na(Population))

### Calculate total population in catchment
int <- st_intersection(w_catch, wz_shp)
int_out <- int %>%
  as.data.frame() %>%
  select(RC_ID, Area_Code, Population) %>%
  group_by(RC_ID) %>%
  summarise(dayPopulation = sum(Population))
rc <- merge(rc, int_out, by = "RC_ID", all.x = TRUE)

## Rateable value ???



# Appendices --------------------------------------------------------------

## Writing Out
st_write(rc, "Output Data/Multidimensional Typology/Inputs.gpkg")

