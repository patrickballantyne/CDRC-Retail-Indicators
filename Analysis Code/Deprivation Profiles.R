## Exposure to IMD

library(sf)
library(tidyverse)
library(data.table)
library(readxl)
library(tmap)


# 1. Data -----------------------------------------------------------------

#### RETAIL CENTRES & CATCHMENTS ########################################

## Retail Centres - drop out small, local ones
rc <- st_read("Output Data/CDRC_Retail_Centres_RECLASSIFIED.gpkg")
rc <- rc %>% 
  filter(Classification != "Small Local Centre") %>%
  filter(RC_Name != "Manchester Piccadilly Station Shopping Area; Manchester (North West; England)") %>%
  filter(RC_Name != "Belfast City. Belfast (Northern Ireland)") %>%
  filter(RC_Name != "Banbridge Village Outlet; Armagh City, Banbridge and Craigavon (Northern Ireland)")

## Catchments
drive_catch <- st_transform(st_read("Output Data/CDRC_RetailCentre_2021_DriveTimes.gpkg"), 27700)
walk_catch <-  st_transform(st_read("Output Data/CDRC_RetailCentre_2021_WalkingIsolines.gpkg"), 27700)

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
scot_imd <- st_read("Input Data/IMD/SG_SIMD_2020.shp")
scot_imd <- scot_imd %>%
  select(DataZone, DZName, Rankv2, Decilev2,
         IncRankv2, EmpRank, HlthRank, EduRank, GAccRank, CrimeRank, HouseRank) %>%
  setNames(c("Area_Code", "Area_Name", "IMD_Rank", "IMD_Decile",
             "Income_Rank", "Employment_Rank", "Health_Rank", "Education_Rank", "GeographicAccess_Rank",
             "Crime_Rank", "Housing_Rank", "geometry"))



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
scot <- read_excel("Input Data/2019_S_estimates.xlsx", sheet = 1,  skip = 5)
scot <- scot %>%
  select(1, 4) %>%
  setNames(c("Area_Code", "Total_Population_2019")) %>%
  mutate_if(is.character, as.factor) %>%
  drop_na()
scot_imd_pop <- merge(scot_imd, scot, by = "Area_Code", all.x = TRUE)

###########################################################################

#### Splitting Centres and Catchments

## Wales centres and catchments
wales_rc <- rc[grepl("(Wales)", rc$RC_Name),]
wales_drive_catch <- drive_catch[grepl("(Wales)", drive_catch$RC_Name),]
wales_walk_catch <- walk_catch[grepl("(Wales)", walk_catch$RC_Name),]

## England centres and catchments
england_rc <- rc[grepl("(England)", rc$RC_Name),]
england_drive_catch <- drive_catch[grepl("(England)", drive_catch$RC_Name),]
england_walk_catch <- walk_catch[grepl("(England)", walk_catch$RC_Name),]

## Scotland centres and catchments
scotland_rc <- rc[grepl("(Scotland)", rc$RC_Name),]
scotland_drive_catch <- drive_catch[grepl("(Scotland)", drive_catch$RC_Name),]
scotland_walk_catch <- walk_catch[grepl("(Scotland)", walk_catch$RC_Name),]

###########################################################################

#### Population weighted centroids

### England/Wales
cent_lsoa <- st_read("Input Data/LSOA_pop_centroids/new_centroids.shp")
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

#### Joining and separating 
wal_imd_pop_df <- wal_imd_pop %>% as.data.frame() %>% select(-c(geometry))
eng_imd_pop_df <- eng_imd_pop %>% as.data.frame() %>% select(-c(geometry))
scot_imd_pop_df <- scot_imd_pop %>% as.data.frame() %>% select(-c(geometry))

eng_cent_imd <- merge(cent_lsoa, eng_imd_pop_df, by = "Area_Code", all.y = TRUE)
wal_cent_imd <- merge(cent_lsoa, wal_imd_pop_df, by = "Area_Code", all.y = TRUE)
scot_cent_imd <- merge(cent_dz, scot_imd_pop_df, by = "Area_Code", all.x = TRUE)

##############################################################################

#### Calculating Deprivation Exposure (by country)

## Identify the LSOAs/DZs in each catchment
wales_drive_int <- st_intersection(wal_cent_imd, wales_drive_catch, join = st_contains)
wales_walk_int <- st_intersection(wal_cent_imd, wales_walk_catch, join = st_contains)

england_drive_int <- st_intersection(eng_cent_imd, england_drive_catch, join = st_contains)
england_walk_int <- st_intersection(eng_cent_imd, england_walk_catch, join = st_contains)

scotland_drive_int <- st_intersection(scot_cent_imd, scotland_drive_catch, join = st_contains)
scotland_walk_int <- st_intersection(scot_cent_imd, scotland_walk_catch, join = st_contains)

## Compute total population in each catchment
wales_drive_catch_pop <- wales_drive_int %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, Total_Population_2019) %>%
  group_by(RC_ID, RC_Name) %>%
  summarise(Total_Catchment_Population = sum(Total_Population_2019))
wales_walk_catch_pop <- wales_walk_int %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, Total_Population_2019) %>%
  group_by(RC_ID, RC_Name) %>%
  summarise(Total_Catchment_Population = sum(Total_Population_2019))

england_drive_catch_pop <- england_drive_int %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, Total_Population_2019) %>%
  group_by(RC_ID, RC_Name) %>%
  summarise(Total_Catchment_Population = sum(Total_Population_2019))
england_walk_catch_pop <- england_walk_int %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, Total_Population_2019) %>%
  group_by(RC_ID, RC_Name) %>%
  summarise(Total_Catchment_Population = sum(Total_Population_2019))

scotland_drive_catch_pop <- scotland_drive_int %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, Total_Population_2019) %>%
  group_by(RC_ID, RC_Name) %>%
  summarise(Total_Catchment_Population = sum(Total_Population_2019))
scotland_walk_catch_pop <- scotland_walk_int %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, Total_Population_2019) %>%
  group_by(RC_ID, RC_Name) %>%
  summarise(Total_Catchment_Population = sum(Total_Population_2019))


# 1. Decile Index ---------------------------------------------------------

## Compute total catchment population occupied by each IMD decile
wales_drive_dec <- merge(wales_drive_int, wales_drive_catch_pop, by = c("RC_ID", "RC_Name"), all.x = TRUE)
wales_drive_dec_sub <- wales_drive_dec %>% as.data.frame() %>% select(RC_ID, RC_Name, IMD_Decile, Total_Catchment_Population)
wales_drive_group_dec <- wales_drive_dec %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, IMD_Decile, Total_Population_2019, Total_Catchment_Population) %>%
  group_by(RC_ID, RC_Name, IMD_Decile) %>%
  summarise(Total_IMD_Population_2019 = sum(Total_Population_2019))
wales_walk_dec <- merge(wales_walk_int, wales_walk_catch_pop, by = c("RC_ID", "RC_Name"), all.x = TRUE)
wales_walk_dec_sub <- wales_walk_dec %>% as.data.frame() %>% select(RC_ID, RC_Name, IMD_Decile, Total_Catchment_Population)
wales_walk_group_dec <- wales_walk_dec %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, IMD_Decile, Total_Population_2019, Total_Catchment_Population) %>%
  group_by(RC_ID, RC_Name, IMD_Decile) %>%
  summarise(Total_IMD_Population_2019 = sum(Total_Population_2019))

england_drive_dec <- merge(england_drive_int, england_drive_catch_pop, by = c("RC_ID", "RC_Name"), all.x = TRUE)
england_drive_dec_sub <- england_drive_dec %>% as.data.frame() %>% select(RC_ID, RC_Name, IMD_Decile, Total_Catchment_Population)
england_drive_group_dec <- england_drive_dec %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, IMD_Decile, Total_Population_2019, Total_Catchment_Population) %>%
  group_by(RC_ID, RC_Name, IMD_Decile) %>%
  summarise(Total_IMD_Population_2019 = sum(Total_Population_2019))
england_walk_dec <- merge(england_walk_int, england_walk_catch_pop, by = c("RC_ID", "RC_Name"), all.x = TRUE)
england_walk_dec_sub <- england_walk_dec %>% as.data.frame() %>% select(RC_ID, RC_Name, IMD_Decile, Total_Catchment_Population)
england_walk_group_dec <- england_walk_dec %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, IMD_Decile, Total_Population_2019, Total_Catchment_Population) %>%
  group_by(RC_ID, RC_Name, IMD_Decile) %>%
  summarise(Total_IMD_Population_2019 = sum(Total_Population_2019))

scotland_drive_dec <- merge(scotland_drive_int, scotland_drive_catch_pop, by = c("RC_ID", "RC_Name"), all.x = TRUE)
scotland_drive_dec_sub <- scotland_drive_dec %>% as.data.frame() %>% select(RC_ID, RC_Name, IMD_Decile, Total_Catchment_Population)
scotland_drive_group_dec <- scotland_drive_dec %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, IMD_Decile, Total_Population_2019, Total_Catchment_Population) %>%
  group_by(RC_ID, RC_Name, IMD_Decile) %>%
  summarise(Total_IMD_Population_2019 = sum(Total_Population_2019))

## Join together
# uk_dec <- rbind(england_dec_sub, wales_dec_sub, scotland_dec_sub)
# uk_group_dec <- rbind(england_group_dec, wales_group_dec, scotland_group_dec)


## Calculate proprtion catchment population occupied by each IMD decile
uk_group_pop <- merge(uk_group_dec, uk_dec, by = c("RC_ID", "RC_Name", "IMD_Decile"), all.x = TRUE)
uk_group_pop <- uk_group_pop %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, IMD_Decile, Total_IMD_Population_2019, Total_Catchment_Population) %>%
  distinct() %>%
  mutate(IMD_Population_Proportion = (Total_IMD_Population_2019 / Total_Catchment_Population) * 100) %>%
  select(RC_ID, RC_Name, IMD_Decile, IMD_Population_Proportion)

## Apply weights
weights <- read.csv("Input Data/IMD/IMD_Decile_Weights.csv")
uk_group_pop <- merge(uk_group_pop, weights, by = "IMD_Decile", all.x = TRUE)

## Get a deprivation exposure score for each centre
deprivation_exposure <- uk_group_pop %>%
  mutate(w_IMD_Population_Proportion = IMD_Population_Proportion * Weight) %>%
  select(RC_ID, RC_Name, w_IMD_Population_Proportion) %>%
  group_by(RC_ID, RC_Name) %>%
  summarise(deprivationExposure = sum(w_IMD_Population_Proportion))

rc_out <- rc %>% select(RC_ID, RC_Name, Classification)
dep_out <- merge(rc_out, deprivation_exposure, by = c("RC_ID", "RC_Name"), all.x = TRUE)
dep_out <- dep_out %>%
  as.data.frame() %>%
  select(-c(geometry)) %>%
  mutate_if(is.numeric, ~replace_na(., 0)) %>%
  arrange(desc(deprivationExposure))
dep_out$deprivationExposure <- scales::rescale(dep_out$deprivationExposure, to = c(0, 1))
## NEED TO ADD IN TOTAL POPULATION PER RETAIL CENTRE CATCHMENT!!

write.csv(dep_out, "Output Data/Deprivation/DeprivationExposure.csv")

# 2. Deprivation Profile --------------------------------------------------

## Join on regional information 
eng_lookup <- read.csv("Input Data/England_Lookup.csv")
eng_lookup <- eng_lookup  %>%
  select(LSOA11CD, RGN11NM, CTRY11NM) %>%
  distinct() %>%
  setNames(c("Area_Code", "Region", "Country"))

eng_drive_int_sub <- england_drive_int  %>% as.data.frame() %>%  select(Area_Code, IMD_Score)
eng_drive_int_lookup <- merge(eng_drive_int_sub, eng_lookup, by.x = "Area_Code", by.y = "LSOA11CD", all.x = TRUE)
eng_drive_int_lookup <- eng_drive_int_lookup %>% distinct() %>% select(-c(IMD_Score)) %>% setNames(c("Area_Code", "Region", "Country"))

## Compute average values for each of the IMD indicators and domains per catchment
wales_drive_catch_imd <- wales_drive_int %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, IMD_Score) %>%
  group_by(RC_ID, RC_Name) %>%
  summarise_if(is.numeric, mean)
wales_walk_catch_imd <- wales_walk_int %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, IMD_Score) %>%
  group_by(RC_ID, RC_Name) %>%
  summarise_if(is.numeric, mean)

england_drive_catch_imd <- england_drive_int %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, IMD_Score) %>%
  group_by(RC_ID, RC_Name) %>%
  summarise_if(is.numeric, mean)
england_walk_catch_imd <- england_walk_int %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, IMD_Score) %>%
  group_by(RC_ID, RC_Name) %>%
  summarise_if(is.numeric, mean)

scotland_drive_catch_imd <- scotland_drive_int %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, IMD_Rank) %>%
  group_by(RC_ID, RC_Name) %>%
  summarise_if(is.numeric, mean)
scotland_walk_catch_imd <- scotland_walk_int %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, IMD_Rank) %>%
  group_by(RC_ID, RC_Name) %>%
  summarise_if(is.numeric, mean)



#### Merge on regions
averages <- eng_imd %>%
  as.data.frame() %>%
  select(Area_Code, IMD_Score) %>%
  distinct()
averages <- merge(averages, eng_lookup, by = "Area_Code", all.x = TRUE)

## Compute National & Regional Average
eng_nat_avg <- averages %>%
  as.data.frame() %>%
  group_by(Country) %>%
  summarise(IMDNational = mean(IMD_Score))
eng_reg_avg <- averages %>%
  as.data.frame() %>%
  group_by(Region) %>%
  summarise(IMDRegional = mean(IMD_Score)) %>%
  arrange(desc(IMDRegional))

wal_nat_avg <- wal_imd %>%
  as.data.frame() %>%
  summarise(IMDNational = mean(IMD_Score))

scot_nat_avg <- scot_imd %>%
  as.data.frame() %>%
  summarise(IMDNational = mean(IMD_Rank))


## Merge on National and Regional Averages
eng_drive_imd_final <- merge(eng_drive_imd_for_reg, eng_nat_avg, by = "Country", all.x = TRUE)
eng_drive_imd_final <- merge(eng_drive_imd_final, eng_reg_avg, by = "Region", all.x = TRUE)
eng_walk_imd_final <- merge(england_walk_int, eng_lookup, by = "Area_Code", all.x = TRUE)
eng_walk_imd_final$IMDNational <- eng_nat_avg$IMDNational
eng_walk_imd_final <- merge(eng_walk_imd_final, eng_reg_avg, by = "Region", all.x = TRUE)

wales_drive_imd_final <- wales_drive_catch_imd
wales_drive_imd_final$Country <- "Wales"
wales_drive_imd_final$IMDNational <- wal_nat_avg$IMDNational 
wales_walk_imd_final <- wales_walk_catch_imd
wales_walk_imd_final$Country <- "Wales"
wales_walk_imd_final$IMDNational <- wal_nat_avg$IMDNational 

scot_drive_imd_final <- scotland_drive_catch_imd
scot_drive_imd_final$Country <- "Scotland"
scot_drive_imd_final$IMDNational <- scot_nat_avg$IMDNational
scot_walk_imd_final <- scotland_walk_catch_imd
scot_walk_imd_final$Country <- "Scotland"
scot_walk_imd_final$IMDNational <- scot_nat_avg$IMDNational

# 1. ENGLAND DEPRIVATION PROFILE ------------------------------------------


### 1.1 DRIVE-TIME CATCHMENTS

## Compute average IMD in each retail centre
eng_drive_imd_out <- eng_drive_imd_final %>%
  as.data.frame() %>%
  group_by(RC_ID, RC_Name, Classification, Duration, Region, Country, IMDNational, IMDRegional) %>%
  summarise(AverageIMD = mean(IMD_Score))

## Create identifier to illustrate whether IMD & domains are higher/lower than national average
eng_drive_imd_clean <- eng_drive_imd_out  %>%
  mutate(diff_IMDNational = AverageIMD - IMDNational,
         diff_IMDRegional = AverageIMD - IMDRegional) %>%
  mutate(r_IMDNational = case_when(diff_IMDNational < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
         r_IMDRegional = case_when(diff_IMDRegional < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED")) %>%
  ungroup() %>%
  select(RC_ID, RC_Name, Region, Country, AverageIMD, IMDRegional, diff_IMDRegional, r_IMDRegional,
         IMDNational, diff_IMDNational, r_IMDNational) %>%
  mutate_if(is.character, as.factor)
#write.csv(eng_drive_imd_clean, "Output Data/Deprivation/England_RetailCentres_Deprivation_Profile_DRIVETIMES.csv")

### 1.2 WALKING CATCHMENTS

## Compute average IMD in each centre
eng_walk_imd_out <- eng_walk_imd_final %>%
  as.data.frame() %>%
  group_by(RC_ID, RC_Name, Classification, Duration, Region, Country, IMDNational, IMDRegional) %>%
  summarise(AverageIMD = mean(IMD_Score))

## Create identifier to illustrate whether IMD & domains are higher/lower than national average
eng_walk_imd_clean <- eng_walk_imd_out  %>%
  mutate(diff_IMDNational = AverageIMD - IMDNational,
         diff_IMDRegional = AverageIMD - IMDRegional) %>%
  mutate(r_IMDNational = case_when(diff_IMDNational < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
         r_IMDRegional = case_when(diff_IMDRegional < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED")) %>%
  ungroup() %>%
  select(RC_ID, RC_Name, Region, Country, AverageIMD, IMDRegional, diff_IMDRegional, r_IMDRegional,
         IMDNational, diff_IMDNational, r_IMDNational) %>%
  mutate_if(is.character, as.factor)

## Create identifier to illustrate population-weighted centroids have been used
eng_walk_imd_clean$whichCentroid <- "Population-Weighted"

## Create list of retail centres with no population-weighted centroids in them 
list <- england_rc %>% filter(!RC_ID %in% eng_walk_imd_clean$RC_ID) %>% as.data.frame() %>% select(-c(geom))


########
# Missing England Walking Catchments

## Get geometric centroids for IMD data
geom_cent <- st_centroid(eng_imd_pop)

## Get catchments
xtra_eng_walk_catch <- england_walk_catch %>%
   filter(RC_ID %in% list$RC_ID)

## Intersect
xtra_england_walk_int <- st_intersection(geom_cent, xtra_eng_walk_catch, join = st_contains)
xtra_england_walk_int <- merge(xtra_england_walk_int, eng_lookup, by = "Area_Code", all.x = TRUE)

## Compute total population and mean IMD
xtra_walk_catch_pop <- xtra_england_walk_int %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, Region, Country, IMD_Score, Total_Population_2019) %>%
  group_by(RC_ID, RC_Name, Region, Country) %>%
  summarise(Total_Catchment_Population = sum(Total_Population_2019),
            AverageIMD = mean(IMD_Score))

xtra_walk_catch_pop$IMDNational <- eng_nat_avg$IMDNational
xtra_walk_catch_pop <- merge(xtra_walk_catch_pop, eng_reg_avg, by = "Region", all.x = TRUE)

## Compute variables
xtra_eng_walk_imd_clean <- xtra_walk_catch_pop  %>%
  mutate(diff_IMDNational = AverageIMD - IMDNational,
         diff_IMDRegional = AverageIMD - IMDRegional) %>%
  mutate(r_IMDNational = case_when(diff_IMDNational < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
         r_IMDRegional = case_when(diff_IMDRegional < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED")) %>%
  ungroup() %>%
  select(RC_ID, RC_Name, Region, Country, AverageIMD, IMDRegional, diff_IMDRegional, r_IMDRegional,
         IMDNational, diff_IMDNational, r_IMDNational) %>%
  mutate_if(is.character, as.factor)
xtra_eng_walk_imd_clean$whichCentroid <- "Geometric"

 
### Bringing together final set of data
england_rc_out <- england_rc %>% as.data.frame() %>% select(-c(geom))
eng_imd_complete <- rbind(xtra_eng_walk_imd_clean, eng_walk_imd_clean)

## Merge and then fill missing centres with NA
england_walk_imd_final <- merge(england_rc_out, eng_imd_complete, by = c("RC_ID", "RC_Name"), all.x = TRUE)
write.csv(england_walk_imd_final, "Output Data/Deprivation/England_RetailCentres_Deprivation_Profile_WALKING.csv")

# 2. WALES DEPRIVATION PROFILE --------------------------------------------

### 2.1 DRIVE-TIME CATCHMENTS

## Create identifier to illustrate whether IMD & domains are higher/lower than national average
wales_drive_imd_clean <- wales_drive_imd_final %>%
  rename(AverageIMD = IMD_Score ) %>%
  mutate(diff_IMDNational = AverageIMD - IMDNational) %>%
  mutate(r_IMDNational = case_when(diff_IMDNational < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED")) %>%
  ungroup() %>%
  select(RC_ID, RC_Name, Country, AverageIMD,
         IMDNational, diff_IMDNational, r_IMDNational) %>%
  mutate_if(is.character, as.factor)
#write.csv(wales_drive_imd_clean, "Output Data/Deprivation/Wales_RetailCentres_Deprivation_Profile_DRIVETIMES.csv")


### 2.2 WALKING CATCHMENTS

## Create identifier to illustrate whether IMD & domains are higher/lower than national average
wales_walk_imd_clean <- wales_walk_imd_final %>%
  rename(AverageIMD = IMD_Score ) %>%
  mutate(diff_IMDNational = AverageIMD - IMDNational) %>%
  mutate(r_IMDNational = case_when(diff_IMDNational < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED")) %>%
  ungroup() %>%
  select(RC_ID, RC_Name, Country, AverageIMD,
         IMDNational, diff_IMDNational, r_IMDNational) %>%
  mutate_if(is.character, as.factor)
write.csv(wales_walk_imd_clean, "Output Data/Deprivation/Wales_RetailCentres_Deprivation_Profile_WALKING.csv")



# 3. SCOTLAND DEPRIVATION PROFILE -----------------------------------------

### 3.1 DRIVE-TIMES

## Create identifier to illustrate whether IMD & domains are higher/lower than national average
scot_drive_imd_clean <- scot_drive_imd_final %>%
  rename(AverageIMD = IMD_Rank) %>%
  mutate(diff_IMDNational = AverageIMD - IMDNational) %>%
  mutate(r_IMDNational = case_when(diff_IMDNational > 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED")) %>%
  ungroup() %>%
  select(RC_ID, RC_Name, Country, AverageIMD,
         IMDNational, diff_IMDNational, r_IMDNational) %>%
  mutate_if(is.character, as.factor)
#write.csv(scot_drive_imd_clean, "Output Data/Deprivation/Scotland_RetailCentres_Deprivation_Profile_DRIVETIMES.csv")

### 3.2 WALKING 

## Create identifier to illustrate whether IMD & domains are higher/lower than national average
scot_walk_imd_clean <- scot_walk_imd_final %>%
  rename(AverageIMD = IMD_Rank) %>%
  mutate(diff_IMDNational = AverageIMD - IMDNational) %>%
  mutate(r_IMDNational = case_when(diff_IMDNational > 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED")) %>%
  ungroup() %>%
  select(RC_ID, RC_Name, Country, AverageIMD,
         IMDNational, diff_IMDNational, r_IMDNational) %>%
  mutate_if(is.character, as.factor)
#write.csv(scot_walk_imd_clean, "Output Data/Deprivation/Scotland_RetailCentres_Deprivation_Profile_WALKING.csv")



# 4. Missing --------------------------------------------------------------

## Compute total number missing from English, Welsh and Scottish walking catchment deprivation profiles
eng_missing <- nrow(eng_drive_imd_clean) - nrow(eng_walk_imd_clean) 
wal_missing <- nrow(wales_drive_imd_clean) - nrow(wales_walk_imd_clean)
scot_missing <- nrow(scot_drive_imd_clean) - nrow(scot_walk_imd_clean)

# 3.  Maps ----------------------------------------------------------------

## Get the retail centre and catchment we want
sub_rc <- england_rc %>%
  filter(RC_ID == "RC_EW_3089")
sub_drive_catch <- england_drive_catch %>%
  filter(RC_ID == "RC_EW_3089")
sub_walk_catch <- england_walk_catch %>%
  filter(RC_ID == "RC_EW_3089")

## Get the IMD data and pop centroids in catchment
sub_drive_cent <- cent_lsoa[sub_drive_catch, op = st_within]
sub_walk_cent <- cent_lsoa[sub_walk_catch, op = st_within]

waz_cent <- cent_lsoa[grepl("Warrington", cent_lsoa$lsoa01nm),]

library(tmap)
tmap_mode("plot")
tm_shape(waz_cent) +
  tm_dots(col = "black", size = 0.2) +
  tm_shape(sub_walk_catch) +
  tm_fill(col = "orange", alpha = 0.75) +
  tm_text("RC_Name", size = 0.55) +
  tm_layout(frame = FALSE) +
  tm_add_legend("fill", col = "orange", labels = "walking catchment", border.col = "orange") +
  tm_add_legend("symbol", size = 0.2, col = "black", labels = "population weighted centroids")

sub_drive_imd <- eng_imd %>%
  filter(Area_Code %in% sub_drive_cent$Area_Code) %>%
  mutate_if(is.numeric, as.factor)
sub_walk_imd <- eng_imd %>%
  filter(Area_Code %in% sub_walk_cent$Area_Code) %>%
  mutate_if(is.numeric, as.factor)

## Get population data 
sub_drive_imd_pop <- eng_imd_pop %>%
  filter(Area_Code %in% sub_drive_cent$Area_Code)
sub_walk_imd_pop <- eng_imd_pop %>%
  filter(Area_Code %in% sub_walk_cent$Area_Code)

## Setup basemap
library(ceramic)
Sys.setenv(MAPBOX_API_KEY=
             "pk.eyJ1Ijoic2dwYmFsbGEiLCJhIjoiY2tncnZxc3FqMGhzaTJ6bzcxNTk3bDNldCJ9.tYEAVCBey8y5tzXx9i0lPw") 
sp <- as(sub_drive_catch, 'Spatial')
basemap <- ceramic::cc_location(loc = sp, zoom = 14,
                                base_url = "https://basemaps.cartocdn.com/rastertiles/voyager_labels_under/{zoom}/{x}/{y}.png")

## Map 1 - Retail Centre &  Catchment
p1 <- tm_shape(basemap) +
  tm_rgb() +
  tm_shape(sub_drive_catch) +
  tm_fill(col = "orange", alpha = 0.05) +
  tm_borders(col = "Orange", lwd = 3) +
  tm_shape(sub_rc) +
  tm_fill(col = "orange", alpha = 0.75) +
  tm_borders(col = "black") +
  tm_layout(frame = FALSE, legend.outside = FALSE, legend.position = c("left", "bottom"),
            legend.frame = TRUE, title = "A", title.fontface = "bold", 
            title.position = c("left", "bottom")) +
  tm_add_legend("fill", labels = "Retail Centre Boundary", col = "orange", border.col = "black") +
  tm_add_legend("fill", labels = "Drive-Time Catchment", col = "white", border.col = "orange")

p2 <- tm_shape(basemap) +
  tm_rgb() +
  tm_shape(sub_walk_catch) +
  tm_fill(col = "orange", alpha = 0.05) +
  tm_borders(col = "orange", lwd = 3) +
  tm_shape(sub_rc) +
  tm_fill(col = "orange", alpha = 0.75) +
  tm_borders(col = "black") +
  tm_layout(frame = FALSE, legend.outside = FALSE, legend.position = c("left", "bottom"),
            legend.frame = TRUE, title = "B", title.bg.color = "white", title.fontface = "bold",
            title.position = c("left", "bottom")) +
  tm_add_legend("fill", labels = "Retail Centre Boundary", col = "orange", border.col = "black") +
  tm_add_legend("fill", labels = "Walking Catchment", col = "white", border.col = "orange")

map1 <- tmap_arrange(p1, p2)
map1

## Map 2 - IMD & Centroids
p3 <- tm_shape(basemap) +
  tm_rgb() +
  tm_shape(sub_drive_imd) +
  tm_fill(col = "IMD_Decile", alpha = 0.5, palette = "-Greys", title = "IMD Decile") +
  tm_borders(col = "black") +
  tm_shape(sub_drive_cent) +
  tm_dots(col = "black", size = 0.2) +
  tm_shape(sub_drive_catch) +
  tm_fill(col = "black", alpha = 0.05) +
  tm_borders(col = "orange", lwd = 3) +
  tm_layout(frame = FALSE, legend.outside = FALSE, legend.position = c("left", "bottom"),
            legend.frame = TRUE, title = "A", title.bg.color = "white", title.fontface = "bold",
            title.position = c("left", "bottom")) +
  tm_add_legend("symbol", labels = "Population-Weighted Centroid", col= "black", size = 0.4) +
  tm_add_legend("fill", labels = "Drive-Time Catchment", col = "white", border.col = "orange")

p4 <- tm_shape(basemap) +
  tm_rgb() +
  tm_shape(sub_walk_imd) +
  tm_fill(col = "IMD_Decile", alpha = 0.5, palette = "-Greys", title = "IMD Decile") +
  tm_borders(col = "black") +
  tm_shape(sub_walk_cent) +
  tm_dots(col = "black", size = 0.2) +
  tm_shape(sub_walk_catch) +
  tm_fill(col = "black", alpha = 0.05) +
  tm_borders(col = "orange", lwd = 3) +
  tm_layout(frame = FALSE, legend.outside = FALSE, legend.position = c("left", "bottom"),
            legend.frame = TRUE, title = "B", title.bg.color = "white", title.fontface = "bold",
            title.position = c("left", "bottom")) +
  tm_add_legend("symbol", labels = "Population-Weighted Centroid", col= "black", size = 0.4) +
  tm_add_legend("fill", labels = "Walking Catchment", col = "white", border.col = "orange")

map2 <- tmap_arrange(p3, p4)
map2


## Map 3 - Population 
p5 <- tm_shape(basemap) +
  tm_rgb() +
  tm_shape(sub_drive_catch) +
  tm_fill(col = "black", alpha = 0.05) +
  tm_borders(col = "orange", lwd = 3) +
  tm_shape(sub_drive_imd_pop) +
  tm_fill(col = "Total_Population_2019", alpha = 0.5, palette = "Greys", title = "Total Population",
          style = "equal", n = 3) +
  tm_borders(col = "black") +
  tm_shape(sub_drive_cent) +
  tm_dots(col = "black", size = 0.2) +
  tm_layout(frame = FALSE, legend.outside = FALSE, legend.position = c("left", "bottom"),
            legend.frame = TRUE, title = "A", title.bg.color = "white", title.fontface = "bold",
            title.position = c("left", "bottom")) +
  tm_add_legend("symbol", labels = "Population-Weighted Centroid", col= "black", size = 0.4) +
  tm_add_legend("fill", labels = "Drive-Time Catchment", col = "white", border.col = "orange")

p6 <- tm_shape(basemap) +
  tm_rgb() +
  tm_shape(sub_walk_catch) +
  tm_fill(col = "black", alpha = 0.05) +
  tm_borders(col = "orange", lwd = 3) +
  tm_shape(sub_walk_imd_pop) +
  tm_fill(col = "Total_Population_2019", alpha = 0.5, palette = "Greys", title = "Total Population",
          style = "equal", n = 3) +
  tm_borders(col = "black") +
  tm_shape(sub_walk_cent) +
  tm_dots(col = "black", size = 0.2) +
  tm_layout(frame = FALSE, legend.outside = FALSE, legend.position = c("left", "bottom"),
            legend.frame = TRUE, title = "B", title.bg.color = "white", title.fontface = "bold",
            title.position = c("left", "bottom")) +
  tm_add_legend("symbol", labels = "Population-Weighted Centroid", col= "black", size = 0.4) +
  tm_add_legend("fill", labels = "Walking Catchment", col = "white", border.col = "orange")

map3 <- tmap_arrange(p5, p6)
map3


## Table 1
table1 <- england_drive_dec %>%
  as.data.frame() %>%
  filter(RC_ID == "RC_EW_5568") %>%
  select(RC_ID, Area_Code, IMD_Decile, Total_Population_2019) %>%
  rename(TotalPopulation2019 = Total_Population_2019) %>%
  filter(Area_Code %in% sub_drive_imd_pop$Area_Code)
write.csv(table1, "Output Data/Deprivation/Table1.csv")


## Table 2
table2 <- england_drive_dec %>%
  as.data.frame() %>%
  filter(RC_ID == "RC_EW_5568") %>%
  select(RC_ID, Total_Catchment_Population) %>%
  rename(TotalCatchmentPopulation = Total_Catchment_Population) %>%
  distinct()
write.csv(table2, "Output Data/Deprivation/Table2.csv")

## Table 3
table3 <- england_drive_dec %>%
  as.data.frame() %>%
  filter(RC_ID == "RC_EW_5568") %>%
  group_by(IMD_Decile) %>%
  summarise(TotalDecilePopulation = sum(Total_Population_2019))
write.csv(table3, "Output Data/Deprivation/Table3.csv")

## Table 4
table4 <- merge(table1, table2, by = "RC_ID", all.x = TRUE)
table4 <- merge(table4, table3, by = "IMD_Decile", all.x = TRUE)
table4 <- table4 %>%
  as.data.frame() %>%
  select(-c(Area_Code, TotalPopulation2019)) %>%
  arrange(IMD_Decile) %>%
  distinct() %>%
  select(RC_ID, IMD_Decile, TotalCatchmentPopulation, TotalDecilePopulation) %>%
  mutate(PropDecileCatchmentPopulation = (TotalDecilePopulation/TotalCatchmentPopulation) * 100)
write.csv(table4, "Output Data/Deprivation/Table4.csv")

## Table 5
table5 <- table4 %>%
  mutate(Weight = case_when(IMD_Decile == 2 ~ 0.8,
                            IMD_Decile == 3 ~ 0.6,
                            IMD_Decile == 4 ~ 0.4,
                            IMD_Decile == 5 ~ 0.2)) %>%
  mutate(w_PropDecileCatchmentProportion = PropDecileCatchmentPopulation * Weight) %>%
  select(RC_ID, IMD_Decile, PropDecileCatchmentPopulation, Weight, w_PropDecileCatchmentProportion)
write.csv(table5, "Output Data/Deprivation/Table5.csv")

## Table 6
table6 <- table5 %>%
  summarise(deprivationExposure = sum(w_PropDecileCatchmentProportion))
write.csv(table6, "Output Data/Deprivation/Table6.csv")



# ### England 
# england_catch_imd <- england_catch_imd %>%
#   mutate(IMD_Score_diff = IMD_Score - england_nat_avg$IMD_Score,
#          Income_diff = Income - england_nat_avg$Income, 
#          Employment_diff = Employment - england_nat_avg$Employment,
#          Health_diff = Health - england_nat_avg$Health,
#          Education_diff = Education -england_nat_avg$Education,
#          Barriers_diff = Barriers - england_nat_avg$Barriers,
#          Crime_diff = Crime - england_nat_avg$Crime,
#          Environment_diff = Environment - england_nat_avg$Environment) %>%
#   mutate(IMD_Score_r = case_when(IMD_Score_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
#          Income_r = case_when(Income_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
#          Employment_r = case_when(Employment_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
#          Health_r = case_when(Health_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
#          Education_r = case_when(Education_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
#          Barriers_r = case_when(Barriers_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
#          Crime_r = case_when(Crime_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
#          Environment_r = case_when(Environment_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED")) %>%
#   select(RC_ID, RC_Name, IMD_Score, IMD_Score_r, Income, Income_r, Employment, Employment_r, Health, Health_r,
#          Education, Education_r, Barriers, Barriers_r, Crime, Crime_r, Environment, Environment_r) %>%
#   setNames(c("RC_ID", "RC_Name", "imdAverage", "imdNational", "incomeAverage", "incomeNational",
#              "employmentAverage", "employmentNational", "healthAverage", "healthNational", 
#              "educationAverage", "educationNational", "barriersAverage", "barriersNational",
#              "crimeAverage", "crimeNational", "environmentAverage", "environmentNational"))
# england_profile_out <- england_rc %>% as.data.frame() %>% select(RC_ID, RC_Name, Classification)
# england_profile_out <- merge(england_profile_out, england_catch_imd, by = c("RC_ID", "RC_Name"), all.x = TRUE)
# england_profile_out <- england_profile_out %>% mutate_if(is.numeric, ~replace_na(., 0))
# write.csv(england_profile_out, "Output Data/Deprivation/England_RetailCentres_Deprivation_Profile.csv")
# 
# ### Wales
# wales_catch_imd <- wales_catch_imd %>%
#   mutate(IMD_Score_diff = IMD_Score - wales_nat_avg$IMD_Score,
#          Income_diff = Income - wales_nat_avg$Income, 
#          Employment_diff = Employment - wales_nat_avg$Employment,
#          Health_diff = Health - wales_nat_avg$Health,
#          Education_diff = Education - wales_nat_avg$Education,
#          Services_diff = Services - wales_nat_avg$Services,
#          Housing_diff = Housing - wales_nat_avg$Housing,
#          Crime_diff = Crime - wales_nat_avg$Crime,
#          Environment_diff = Environment - wales_nat_avg$Environment) %>%
#   mutate(IMD_Score_r = case_when(IMD_Score_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
#          Income_r = case_when(Income_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
#          Employment_r = case_when(Employment_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
#          Health_r = case_when(Health_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
#          Education_r = case_when(Education_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
#          Services_r = case_when(Services_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
#          Housing_r = case_when(Housing_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
#          Crime_r = case_when(Crime_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
#          Environment_r = case_when(Environment_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED")) %>%
#   select(RC_ID, RC_Name, IMD_Score, IMD_Score_r, Income, Income_r, Employment, Employment_r, Health, Health_r,
#          Education, Education_r, Services, Services_r, Housing, Housing_r, Crime, Crime_r, Environment, Environment_r) %>%
#   setNames(c("RC_ID", "RC_Name", "imdAverage", "imdNational", "incomeAverage", "incomeNational",
#              "employmentAverage", "employmentNational", "healthAverage", "healthNational", 
#              "educationAverage", "educationNational", "servicesAverage", "servicesNational",
#              "housingAverage", "housingNational", "crimeAverage", "crimeNational", "environmentAverage", "environmentNational"))
# wales_profile_out <- wales_rc %>% as.data.frame() %>% select(RC_ID, RC_Name, Classification)
# wales_profile_out <- merge(wales_profile_out, wales_catch_imd, by = c("RC_ID", "RC_Name"), all.x = TRUE)
# wales_profile_out <- wales_profile_out %>% mutate_if(is.numeric, ~replace_na(., 0))
# write.csv(wales_profile_out, "Output Data/Deprivation/Wales_RetailCentres_Deprivation_Profile.csv")
# 
# 
# ### Scotland
# scotland_catch_imd <- scotland_catch_imd %>%
#   mutate(IMD_Rank_diff = IMD_Rank - scotland_nat_avg$IMD_Rank,
#          Income_diff = Income_Rank - scotland_nat_avg$Income_Rank, 
#          Employment_diff = Employment_Rank - scotland_nat_avg$Employment_Rank,
#          Health_diff = Health_Rank - scotland_nat_avg$Health_Rank,
#          Education_diff = Education_Rank - scotland_nat_avg$Education_Rank,
#          GeographicAccess_diff = GeographicAccess_Rank - scotland_nat_avg$GeographicAccess_Rank,
#          Housing_diff = Housing_Rank - scotland_nat_avg$Housing_Rank,
#          Crime_diff = Crime_Rank - scotland_nat_avg$Crime_Rank) %>%
#   mutate(IMD_Rank_r = case_when(IMD_Rank_diff > 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
#          Income_r = case_when(Income_diff > 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
#          Employment_r = case_when(Employment_diff > 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
#          Health_r = case_when(Health_diff > 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
#          Education_r = case_when(Education_diff > 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
#          GeographicAccess_r = case_when(GeographicAccess_diff > 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
#          Housing_r = case_when(Housing_diff > 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
#          Crime_r = case_when(Crime_diff > 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED")) %>%
#   select(RC_ID, RC_Name, IMD_Rank, IMD_Rank_r, Income_Rank, Income_r, Employment_Rank, Employment_r, Health_Rank, Health_r,
#          Education_Rank, Education_r, GeographicAccess_Rank, GeographicAccess_r, Housing_Rank, Housing_r, Crime_Rank, Crime_r) %>%
#   setNames(c("RC_ID", "RC_Name", "imdAverage", "imdNational", "incomeAverage", "incomeNational",
#              "employmentAverage", "employmentNational", "healthAverage", "healthNational", 
#              "educationAverage", "educationNational", "geographicaccessAverage", "geographicaccessNational",
#              "housingAverage", "housingNational", "crimeAverage", "crimeNational"))
# scotland_profile_out <- scotland_rc %>% as.data.frame() %>% select(RC_ID, RC_Name, Classification)
# scotland_profile_out <- merge(scotland_profile_out, scotland_catch_imd, by = c("RC_ID", "RC_Name"), all.x = TRUE)
# scotland_profile_out <- scotland_profile_out %>% mutate_if(is.numeric, ~replace_na(., 0))
# write.csv(scotland_profile_out, "Output Data/Deprivation/Scotland_RetailCentres_Deprivation_Profile.csv")
