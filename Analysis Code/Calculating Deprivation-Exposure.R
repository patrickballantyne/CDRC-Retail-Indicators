## Exposure to IMD

library(sf)
library(tidyverse)
library(data.table)
library(readxl)


# 1. Data -----------------------------------------------------------------

#### RETAIL CENTRES & CATCHMENTS ########################################

## Retail Centres - drop out small, local ones and Northern Irish
rc <- st_read("Input Data/Retail_Centres_UK_Final.gpkg")
rc <- rc %>% 
  filter(Classification != "Small Local Centre") %>%
  filter(RC_Name != "Banbridge Village Outlet; Armagh City, Banbridge and Craigavon (Northern Ireland)") %>%
  filter(RC_Name != "Belfast City. Belfast (Northern Ireland)")

## Catchments - drop out small, local ones and Northern Irish
catch <- st_transform(st_read("Output Data/CDRC_RetailCentre_2021_DriveTimes.gpkg"), 27700)
catch <- catch %>%
  filter(RC_Name != "Banbridge Village Outlet; Armagh City, Banbridge and Craigavon (Northern Ireland)") %>%
  filter(RC_Name != "Belfast City. Belfast (Northern Ireland)")

#########################################################################

#### IMD ################################################################

## English IMD (2019)
eng_imd <- fread("Input Data/IMD/IMD_England_2015.csv")
eng_imd <- eng_imd %>%
  select(1, 7) %>%
  setNames(c("Area_Code", "IMD_Decile")) 

## Welsh IMD (2019)
wal_imd <- fread("Input Data/IMD/IMD_Wales_2014.csv")
wal_imd <- wal_imd %>%
  select(1, 6) %>%
  setNames(c("Area_Code", "IMD_Decile"))

## Scottish IMD (2019)
scot_imd <- fread("Input Data/IMD/IMD_Scotland_2016.csv")
scot_imd <- scot_imd %>%
  select(1, 9) %>%
  setNames(c("Area_Code", "IMD_Decile"))

## Join 
uk_imd <- rbind(eng_imd, wal_imd, scot_imd)

###########################################################################

#### Population  ###########################################################

## England & Wales
eng_wal <- read_excel("Input Data/2019_EW_estimates.xlsx", sheet = 4, skip = 5)
eng_wal <- eng_wal %>%
  select(1, 7) %>%
  setNames(c("Area_Code", "Total_Population_2019")) %>%
  mutate_if(is.character, as.factor) %>%
  drop_na()

### Scotland
scot <- read_excel("Input Data/2019_S_estimates.xlsx", sheet = 1,  skip = 5)
scot <- scot %>%
  select(1, 4) %>%
  setNames(c("Area_Code", "Total_Population_2019")) %>%
  mutate_if(is.character, as.factor) %>%
  drop_na()

### UK Population
pop <- rbind(eng_wal, scot)

###########################################################################

#### Population Weighted Centroids ########################################

### England/Wales
cent_lsoa <- st_read("Input Data/LSOA_pop_centroids/lsoa_centroids.shp")
cent_lsoa <- cent_lsoa %>%
  select(2) %>%
  setNames(c("Area_Code", "geometry")) %>%
  mutate_if(is.character, as.factor)

### Scotland
cent_dz <- st_read("Input Data/DZ_pop_centroids/SG_DataZone_Cent_2011.shp")
cent_dz <- cent_dz %>%
  select(1) %>%
  setNames(c("Area_Code", "geometry")) %>%
  mutate_if(is.character, as.factor)

### Compile
cent <- rbind(cent_lsoa, cent_dz)

###########################################################################

#### Preparing for Spatial Join ###########################################

## Join IMD and Population data together
imd_pop <- merge(uk_imd, pop, by = "Area_Code")
## Join to centroids
cent_db <- merge(cent, imd_pop, by = "Area_Code", all.x = TRUE)
cent_db <- cent_db %>%
  drop_na()

############################################################################

#### Calculating Deprivation Exposure

## Identifying centroids in catchments
pip <- st_join(cent_db, catch, join = st_within)
pip <- pip %>%
  mutate_if(is.character, as.factor) %>%
  drop_na()

## Computing total population in each 
catch_pop <- pip %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, Total_Population_2019) %>%
  group_by(RC_ID, RC_Name) %>%
  summarise(Total_Catchment_Population = sum(Total_Population_2019))

## Compute total catchment population occupied by each IMD decile
pip <- merge(pip, catch_pop, by = c("RC_ID", "RC_Name"), all.x = TRUE)
group_pop <- pip %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, IMD_Decile, Total_Population_2019, Total_Catchment_Population) %>%
  group_by(RC_ID, RC_Name, IMD_Decile) %>%
  summarise(Total_IMD_Population_2019 = sum(Total_Population_2019))

## Calculate proprtion catchment population occupied by each IMD decile
pip_sub <- pip %>%
  select(RC_ID, RC_Name, IMD_Decile, Total_Catchment_Population) %>%
  distinct()
group_pop <- merge(group_pop, pip_sub, by = c("RC_ID", "RC_Name", "IMD_Decile"), all.x = TRUE)
group_pop <- group_pop %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, IMD_Decile, Total_IMD_Population_2019, Total_Catchment_Population) %>%
  distinct() %>%
  mutate(IMD_Population_Proportion = (Total_IMD_Population_2019 / Total_Catchment_Population) * 100) %>%
  select(RC_ID, RC_Name, IMD_Decile, IMD_Population_Proportion)

## Apply weights???

## Get a deprivation exposure score for each centre??
deprivation_exposure <- group_pop %>%
  select(RC_ID, RC_Name, w_IMD_Population_2019) %>%
  group_by(RC_ID, RC_Name) %>%
  summarise(deprivationExposure = sum(w_IMD_Population_2019))
