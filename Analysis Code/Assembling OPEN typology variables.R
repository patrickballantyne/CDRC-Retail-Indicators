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
rc_out <- rc %>%
  as.data.frame() %>%
  select(-c(geom))

## Read in additional indicators
ind <- read.csv("Output Data/CDRC_RetailCentre_Indicators.csv")
ind <- ind %>%
  select(RC_ID, RC_Name, n.LDC.2020, 
         propVacant, PropStructuralVacant, propVacantChange,
         onlineExposure) %>%
  rename(propStructuralVacant = PropStructuralVacant)

## Merge on 
rc_out <- merge(rc_out, ind, by = c("RC_ID", "RC_Name"), all.y = TRUE)

# 2. New Indicators -------------------------------------------------------

# 2.1. Size and Function Domain ------------------------------------------------

## Centre size
rc$centreArea <- st_area(rc)
rc$centreArea <- as.numeric(rc$centreArea / 1000000)
area <- rc %>%
  as.data.frame() %>%
  select(RC_ID, centreArea)
rc_out <- merge(rc_out, area, by = "RC_ID", all.x = TRUE)

## Catchment size
catchment_sf <- st_transform(st_read("Output Data/CDRC_RetailCentre_2021_DriveTimes.gpkg"), 27700)
catchment <- catchment_sf %>%
  select(RC_ID) %>%
  st_transform(27700)
catchment$geographicReach <- st_area(catchment)
catchment$geographicReach <- as.numeric(catchment$geographicReach / 1000000)
catchment <- catchment %>% 
  as.data.frame() %>%
  select(RC_ID, geographicReach)
rc_out <- merge(rc_out, catchment, by = "RC_ID", all.x = TRUE)

## Centre morphology
### Compute minimum bounding circle and it's area
circ <- st_minimum_bounding_circle(rc)
circ$m_area <- st_area(circ)
circ$m_area <- as.numeric(circ$m_area / 1000000)
### Calculate roeck score
circ <- circ %>%
  select(RC_ID, centreArea, m_area) %>%
  mutate(roeckScore = centreArea / m_area)
circ$roeckScore <- scales::rescale(circ$roeckScore, to = c(0, 1))
circ <- circ %>% as.data.frame() %>% select(RC_ID, roeckScore)
rc_out <- merge(rc_out, circ, by = "RC_ID", all.x = TRUE)

# 2.2. Economic health domain ---------------------------------------------

## Number of competing destinations
rc_sub <- rc_sf %>% filter(Classification != "Small Local Centre")
searches <- c("Regional Centre", "Major Town Centre", "Town Centre", "Large Shopping Centre", "Large Retail Park")

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
rc_out <- merge(rc_out, competitionList, by = "RC_ID", all.x = TRUE)
rc_out <- rc_out %>%
  mutate(n_competing = coalesce(n_competing, 0)) %>%
  rename(nCompeting = n_competing)


## Affluence -------------------------------------------------------------------------------

## Use the deprivation profiles for walking catchments
imd <- st_read("Output Data/Deprivation/CDRC_RetailCentre_WalkingDeprivation.gpkg")
imd <- imd %>%
  as.data.frame() %>%
  select(RC_ID, AvgIMDScore) %>%
  rename(averageIMD = AvgIMDScore)
rc_out <- merge(rc_out, imd, by = "RC_ID", all.x = TRUE)

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
#st_write(pop, "Output Data/2019_Population.gpkg")

## Calculate the total population of the walking cacthments
w_catch <- st_read("Output Data/CDRC_RetailCentre_2021_WalkingIsolines_v4.gpkg")
w_catch <- w_catch %>%
  filter(RC_ID %in% rc_out$RC_ID)
int <- st_intersection(st_make_valid(w_catch), st_make_valid(pop))
int_out <- int %>%
  as.data.frame() %>%
  select(RC_ID, Area_Code, Total_Population_2019) %>%
  group_by(RC_ID) %>%
  summarise(nightPopulation = sum(Total_Population_2019))
rc_out <- merge(rc_out, int_out, by = "RC_ID", all.x = TRUE)

## Day-time population ---------------------------------------------------------

# ### Read in the 2011 WZ Population Data
# wz_pop <- read.csv("Input Data/WZ/WZ_Population_2011.csv")
# wz_pop <- wz_pop %>%
#   select(3:4) %>%
#   setNames(c("Area_Code", "Population"))
# wz_shp <- merge(pop_shp, wz_pop, by = "Area_Code", all.x = TRUE)
# wz_shp <- wz_shp %>%
#   filter(!is.na(Population))
# 
# ### Calculate total population in catchment
# 
# int <- st_intersection(st_make_valid(w_catch[, ]), st_make_valid(wz_shp))
# int_out <- int %>%
#   as.data.frame() %>%
#   select(RC_ID, Area_Code, Population) %>%
#   group_by(RC_ID) %>%
#   summarise(dayPopulation = sum(Population))
# rc_out <- merge(rc_out, int_out, by = "RC_ID", all.x = TRUE)

# Appendices --------------------------------------------------------------

## Writing Out
st_write(rc_out, "Output Data/Multidimensional Typology/Open_Inputs.csv", append = FALSE)
head(rc_out)
