## Propensity for Online Shopping

library(sf)
library(tidyverse)
library(data.table)
library(readxl)

# 1. Data -----------------------------------------------------------------

## Retail Centres & Catchments #############################################

### Read in centres and drop Northern Irish ones
rc <- st_read("Input Data/Retail_Centres_UK_Final.gpkg")
rc <- rc %>% 
  filter(Classification != "Small Local Centre") %>%
  filter(RC_Name != "Banbridge Village Outlet; Armagh City, Banbridge and Craigavon (Northern Ireland)") %>%
  filter(RC_Name != "Belfast City. Belfast (Northern Ireland)")

### Read in catchments and drop Northern Irish ones
catch <- st_transform(st_read("Output Data/CDRC_RetailCentre_2021_DriveTimes.gpkg"), 27700)
catch <- catch %>%
  filter(RC_Name != "Banbridge Village Outlet; Armagh City, Banbridge and Craigavon (Northern Ireland)") %>%
  filter(RC_Name != "Belfast City. Belfast (Northern Ireland)")


################################################################################

## Internet User Classification - England, Scotland & Wales ####################
iuc <- fread("Input Data/iuc2018.csv")
iuc <- iuc %>%
  select(2:5) %>%
  setNames(c("SA_CD", "SA_NM", "IUC_GRP_CD", "IUC_GRP_LABEL")) %>%
  mutate_if(is.character, as.factor)

## Z Scores
z <- fread("Input Data/iuc2018_zscores.csv")
z$weight <- z$Sum/100

################################################################################

## Population  ###########################################################
### England & Wales
eng_wal <- read_excel("Input Data/2019_EW_estimates.xlsx", sheet = 4, skip = 5)
eng_wal <- eng_wal %>%
  select(1:2, 7) %>%
  setNames(c("SA_CD", "SA_NM", "Total_Population_2019")) %>%
  mutate_if(is.character, as.factor) %>%
  drop_na()

### Scotland
scot <- read_excel("Input Data/2019_S_estimates.xlsx", sheet = 1,  skip = 5)

scot <- scot %>%
  select(1:2, 4) %>%
  setNames(c("SA_CD", "SA_NM", "Total_Population_2019")) %>%
  mutate_if(is.character, as.factor) %>%
  drop_na()

### UK Population
pop <- rbind(eng_wal, scot)

################################################################################

## Population Weighted Centroids

### England/Wales
cent_lsoa <- st_read("Input Data/LSOA_pop_centroids/lsoa_centroids.shp")
cent_lsoa <- cent_lsoa %>%
  select(2:3) %>%
  setNames(c("SA_CD", "SA_NM", "geometry")) %>%
  mutate_if(is.character, as.factor)

### Scotland
cent_dz <- st_read("Input Data/DZ_pop_centroids/SG_DataZone_Cent_2011.shp")
cent_dz <- cent_dz %>%
  select(1:2) %>%
  setNames(c("SA_CD", "SA_NM", "geometry")) %>%
  mutate_if(is.character, as.factor)

### Compile
cent <- rbind(cent_lsoa, cent_dz)
cent <- cent %>% select(-c(SA_NM))

#############################################################################


# 2. Calculating Online Exposure ------------------------------------------

### Join population data to IUC groups
pop_iuc <- merge(pop, iuc, by = c("SA_CD"))
pop_iuc <- pop_iuc %>%
  select(SA_CD, SA_NM.x, Total_Population_2019, IUC_GRP_CD, IUC_GRP_LABEL) %>%
  rename(SA_NM = SA_NM.x)

## Merge on weights from IUC Z scores
z_sub <- z %>%
  select(1:2, 30) %>%
  setNames(c("IUC_GRP_CD", "IUC_GRP_LABEL", "Weight"))
pop_iuc <- merge(pop_iuc, z_sub, by = c("IUC_GRP_CD", "IUC_GRP_LABEL"))

## Merge population, IUC and z scores onto the population weighted centroids
cent_db <- merge(cent, pop_iuc, by = c("SA_CD"), all.x = TRUE)

#############################################################################

### Point in Polygon - get list of LSOAs/DZs in each catchment
pip <- st_join(cent_db, catch, join = st_within)
pip <- pip %>%
  mutate_if(is.character, as.factor) %>%
  drop_na()

### Compute total population in each catchment
catch_pop <- pip %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, Total_Population_2019) %>%
  group_by(RC_ID, RC_Name) %>%
  summarise(Total_Catchment_Population = sum(Total_Population_2019))

## Compute proportion of catchment population occupied by each IUC group
pip <- merge(pip, catch_pop, by = c("RC_ID", "RC_Name"), all.x = TRUE)
group_pop <- pip %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, IUC_GRP_CD, IUC_GRP_LABEL, Total_Population_2019, Total_Catchment_Population, Weight) %>%
  group_by(RC_ID, RC_Name, IUC_GRP_LABEL, IUC_GRP_CD) %>%
  summarise(Total_IUC_Population_2019 = sum(Total_Population_2019))

## Merge on total catchment pop and calculate proportion catchment pop occupied by each IUC group
pip_sub <- pip %>%
  select(RC_ID, RC_Name, IUC_GRP_CD, IUC_GRP_LABEL, Total_Catchment_Population) %>%
  distinct()
group_pop <- merge(group_pop, pip_sub, by = c("RC_ID", "RC_Name", "IUC_GRP_CD", "IUC_GRP_LABEL"), all.x = TRUE)
group_pop <- group_pop %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, IUC_GRP_CD, IUC_GRP_LABEL, Total_IUC_Population_2019, Total_Catchment_Population) %>%
  distinct() %>%
  mutate(IUC_Population_Proportion = (Total_IUC_Population_2019 / Total_Catchment_Population) * 100) %>%
  select(RC_ID, RC_Name, IUC_GRP_CD, IUC_GRP_LABEL, IUC_Population_Proportion)


## Apply weights
group_pop <- merge(group_pop, z_sub, by = c("IUC_GRP_CD", "IUC_GRP_LABEL"),  all.x = TRUE)
group_pop$w_IUC_Population_2019 <- group_pop$IUC_Population_Proportion * group_pop$Weight
head(group_pop)

### Get total weighted pop per Retail Centre
online_exposure <- group_pop %>%
  select(RC_ID, RC_Name, w_IUC_Population_2019) %>%
  group_by(RC_ID, RC_Name) %>%
  summarise(OE = sum(w_IUC_Population_2019))

## Merge back onto main dataset and add 0 score for those not overlapping pop centroids
out <- merge(rc, online_exposure, by = c("RC_ID", "RC_Name"), all.x  = TRUE)
out <- out %>%
  as.data.frame() %>%
  mutate_if(is.numeric, ~replace_na(., 0)) %>%
  select(-c(tr_retailN, geometry)) %>%
  rename(onlineExposure = OE) %>%
  mutate_at(c("onlineExposure"), ~(scale(.) %>% as.vector))

# ## Print Top & Bottom 10
# top_10 <- online_exposure %>%
#   arrange(desc(OE)) 
# top_10[1:10,]
# bottom_10 <- online_exposure %>%
#   arrange(OE)
# bottom_10[1:10,]

### Write out
write.csv(out, "Output Data/CDRC_Retail_Centre_2021_OnlineExposure.csv")
  