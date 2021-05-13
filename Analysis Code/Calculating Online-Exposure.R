## Propensity for Online Shopping

library(sf)
library(tidyverse)
library(data.table)
library(readxl)

# 1. Data -----------------------------------------------------------------

## Retail Centres & Catchments #############################################
rc <- st_read("Input Data/Retail_Centres_UK_Final.gpkg")
rc <- rc %>% filter(Classification != "Small Local Centre")
catch <- st_transform(st_read("Output Data/CDRC_RetailCentre_2021_DriveTimes.gpkg"), 27700)

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

#############################################################################


# 2. Calculating Online Exposure ------------------------------------------

### Joining data
pop_iuc <- merge(pop, iuc, by = c("SA_CD", "SA_NM"))
pop_iuc <- merge(pop_iuc, z[,c ("Cluster Hierarchy", "Cluster Group", "weight")],
                 by.x = c("IUC_GRP_CD", "IUC_GRP_LABEL"), by.y = c("Cluster Hierarchy", "Cluster Group"))
cent <- merge(cent, pop_iuc, by = c("SA_CD", "SA_NM"))

### Point in Polygon
pip <- st_join(cent, catch, join = st_within)
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
  select(RC_ID, RC_Name, IUC_GRP_CD, IUC_GRP_LABEL, Total_Population_2019, Total_Catchment_Population, weight) %>%
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
zsub <- z[, c("Cluster Hierarchy", "Cluster Group", "weight")]
group_pop <- merge(group_pop, zsub, by.x = c("IUC_GRP_CD", "IUC_GRP_LABEL"), by.y = c("Cluster Hierarchy", "Cluster Group"), all.x = TRUE)
group_pop$w_IUC_Population_2019 <- group_pop$IUC_Population_Proportion * group_pop$weight

### Get total weighted pop per Centre
online_exposure <- group_pop %>%
  select(RC_ID, RC_Name, w_IUC_Population_2019) %>%
  group_by(RC_ID, RC_Name) %>%
  summarise(OE = sum(w_IUC_Population_2019))
  