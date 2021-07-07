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
eng_imd <- st_read("Input Data/IMD/IMD_2019.shp")
eng_imd <- eng_imd %>%
  select(lsoa11cd, lsoa11nm, IMDScore, IMD_Rank, IMD_Decile, IncScore, EmpScore, EduScore, HDDScore, CriScore, BHSScore, EnvScore) %>%
  setNames(c("Area_Code", "Area_Name", "IMD_Score", "IMD_Rank", "IMD_Decile", "Income", "Employment", "Education", 
             "Health", "Crime", "Barriers", "Environment", "geometry")) 

## Welsh IMD (2019)
wal_imd <- st_read("Input Data/IMD/wimd2019_overall.shp")
wal_imd <- wal_imd %>%
  select(lsoa_code, lsoa_name_, rank, decile) %>%
  setNames(c("Area_Code", "Area_Name", "IMD_Rank", "IMD_Decile", "geometry"))
wal_scores <- fread("Input Data/IMD/Wales_IMD_Scores.csv")
wal_scores <- wal_scores %>% 
  select(-c(LSOA_Name)) %>%
  setNames(c("Area_Code", "IMD_Score",  "Income", "Employment", "Health", 
           "Education", "Services", "Housing", "Crime", "Environment"))
wal_imd <- merge(wal_imd, wal_scores, by = "Area_Code", all.x = TRUE)

## Scottish IMD (2019)
# scot_imd <- st_read("Input Data/IMD/SG_SIMD_2016.shp")
# scot_imd <- scot_imd %>%
#   select(DataZone, SAPE2014, Ranl, IncRate, EmpRate, ) %>%
#   setNames(c("Area_Code", "Area_Name", "geometry"))



###########################################################################

#### Population  ###########################################################

## England & Wales
eng_wal_pop <- read_excel("Input Data/2019_EW_estimates.xlsx", sheet = 4, skip = 5)
eng_wal_pop <- eng_wal_pop %>%
  select(1, 7) %>%
  setNames(c("Area_Code", "Total_Population_2019")) %>%
  mutate_if(is.character, as.factor) %>%
  drop_na()
eng_imd_pop <- merge(eng_imd, eng_wal_pop, by = "Area_Code", all.x = TRUE)
eng_imd_pop <- eng_imd_pop %>% drop_na()
wal_imd_pop <- merge(wal_imd, eng_wal_pop, by = "Area_Code", all.x = TRUE)


# ### Scotland
# scot <- read_excel("Input Data/2019_S_estimates.xlsx", sheet = 1,  skip = 5)
# scot <- scot %>%
#   select(1, 4) %>%
#   setNames(c("Area_Code", "Total_Population_2019")) %>%
#   mutate_if(is.character, as.factor) %>%
#   drop_na()
# 
# ### UK Population
# pop <- rbind(eng_wal, scot)

###########################################################################

#### Population Weighted Centroids ########################################

# ### England/Wales
# cent_lsoa <- st_read("Input Data/LSOA_pop_centroids/lsoa_centroids.shp")
# cent_lsoa <- cent_lsoa %>%
#   select(2) %>%
#   setNames(c("Area_Code", "geometry")) %>%
#   mutate_if(is.character, as.factor)
# 
# ### Scotland
# cent_dz <- st_read("Input Data/DZ_pop_centroids/SG_DataZone_Cent_2011.shp")
# cent_dz <- cent_dz %>%
#   select(1) %>%
#   setNames(c("Area_Code", "geometry")) %>%
#   mutate_if(is.character, as.factor)
# 
# ### Compile
# cent <- rbind(cent_lsoa, cent_dz)

###########################################################################

#### Preparing for Spatial Join ###########################################

# ## Join IMD and Population data together
# imd_pop <- merge(uk_imd, pop, by = "Area_Code")
# ## Join to centroids
# cent_db <- merge(cent, imd_pop, by = "Area_Code", all.x = TRUE)
# cent_db <- cent_db %>%
#   drop_na()

############################################################################

#### Calculating Deprivation Exposure

## Identifying LSOAs in catchments
wales_int <- st_intersection(wal_imd_pop, catch, join = st_contains)
england_int <- st_intersection(eng_imd_pop, catch, join = st_contains)

## Computing total population in each retail centre catchment
wales_catch_pop <- wales_int %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, Total_Population_2019) %>%
  group_by(RC_ID, RC_Name) %>%
  summarise(Total_Catchment_Population = sum(Total_Population_2019))
england_catch_pop <- england_int %>%
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
