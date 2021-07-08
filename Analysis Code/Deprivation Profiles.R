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
  filter(Classification != "Small Local Centre")

## Catchments
catch <- st_transform(st_read("Output Data/CDRC_RetailCentre_2021_DriveTimes.gpkg"), 27700)

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
wales_catch <- catch[grepl("(Wales)", catch$RC_Name),]
## England centres and catchments
england_rc <- rc[grepl("(England)", rc$RC_Name),]
england_catch <- catch[grepl("(England)", catch$RC_Name),]
## Scotland centres and catchments
scotland_rc <- rc[grepl("(Scotland)", rc$RC_Name),]
scotland_catch <- catch[grepl("(Scotland)", catch$RC_Name),]

###########################################################################

#### Population weighted centroids

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
wales_int <- st_intersection(wal_cent_imd, wales_catch, join = st_contains)
england_int <- st_intersection(eng_cent_imd, england_catch, join = st_contains)
scotland_int <- st_intersection(scot_cent_imd, scotland_catch, join = st_contains)

## Compute total population in each catchment
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
scotland_catch_pop <- scotland_int %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, Total_Population_2019) %>%
  group_by(RC_ID, RC_Name) %>%
  summarise(Total_Catchment_Population = sum(Total_Population_2019))


# 1. Decile Index ---------------------------------------------------------

## Compute total catchment population occupied by each IMD decile
wales_dec <- merge(wales_int, wales_catch_pop, by = c("RC_ID", "RC_Name"), all.x = TRUE)
wales_dec_sub <- wales_dec %>% as.data.frame() %>% select(RC_ID, RC_Name, IMD_Decile, Total_Catchment_Population)
wales_group_dec <- wales_dec %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, IMD_Decile, Total_Population_2019, Total_Catchment_Population) %>%
  group_by(RC_ID, RC_Name, IMD_Decile) %>%
  summarise(Total_IMD_Population_2019 = sum(Total_Population_2019))
england_dec <- merge(england_int, england_catch_pop, by = c("RC_ID", "RC_Name"), all.x = TRUE)
england_dec_sub <- england_dec %>% as.data.frame() %>% select(RC_ID, RC_Name, IMD_Decile, Total_Catchment_Population)
england_group_dec <- england_dec %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, IMD_Decile, Total_Population_2019, Total_Catchment_Population) %>%
  group_by(RC_ID, RC_Name, IMD_Decile) %>%
  summarise(Total_IMD_Population_2019 = sum(Total_Population_2019))
scotland_dec <- merge(scotland_int, scotland_catch_pop, by = c("RC_ID", "RC_Name"), all.x = TRUE)
scotland_dec_sub <- scotland_dec %>% as.data.frame() %>% select(RC_ID, RC_Name, IMD_Decile, Total_Catchment_Population)
scotland_group_dec <- scotland_dec %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, IMD_Decile, Total_Population_2019, Total_Catchment_Population) %>%
  group_by(RC_ID, RC_Name, IMD_Decile) %>%
  summarise(Total_IMD_Population_2019 = sum(Total_Population_2019))
## Join together
uk_dec <- rbind(england_dec_sub, wales_dec_sub, scotland_dec_sub)
uk_group_dec <- rbind(england_group_dec, wales_group_dec, scotland_group_dec)


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

## Get a deprivation exposure score for each centre??
deprivation_exposure <- uk_group_pop %>%
  mutate(w_IMD_Population_Proportion = IMD_Population_Proportion * weight) %>%
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
write.csv(dep_out, "Output Data/Deprivation/DeprivationExposure.csv")

# 2. Deprivation Profile --------------------------------------------------

## Compute average values for each of the IMD indicators and domains per catchment
wales_catch_imd <- wales_int %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, IMD_Score,
         Income, Employment, Health, Education, Services, Housing, Crime, Environment) %>%
  group_by(RC_ID, RC_Name) %>%
  summarise_if(is.numeric, mean)
england_catch_imd <- england_int %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, IMD_Score,
         Income, Employment, Health, Education, Crime, Barriers, Environment) %>%
  group_by(RC_ID, RC_Name) %>%
  summarise_if(is.numeric, mean)
scotland_catch_imd <- scotland_int %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, IMD_Rank,
         Income_Rank, Employment_Rank, Health_Rank, Education_Rank, GeographicAccess_Rank, Crime_Rank, Housing_Rank) %>%
  group_by(RC_ID, RC_Name) %>%
  summarise_if(is.numeric, mean)

## Compute national averages
wales_nat_avg <- wal_imd %>%
  as.data.frame() %>%
  select(-c(Area_Code, Area_Name, geometry)) %>%
  summarise_all(mean)
england_nat_avg <- eng_imd %>%
  as.data.frame() %>%
  select(-c(Area_Code, Area_Name, geometry)) %>%
  summarise_all(mean)
scotland_nat_avg <- scot_imd %>%
  as.data.frame() %>%
  select(-c(Area_Code, Area_Name, geometry)) %>%
  summarise_all(mean)
  
## Create identifier to illustrate whether IMD & domains are higher/lower than national average

### England
england_catch_imd <- england_catch_imd %>%
  mutate(IMD_Score_diff = IMD_Score - england_nat_avg$IMD_Score,
         Income_diff = Income - england_nat_avg$Income, 
         Employment_diff = Employment - england_nat_avg$Employment,
         Health_diff = Health - england_nat_avg$Health,
         Education_diff = Education -england_nat_avg$Education,
         Barriers_diff = Barriers - england_nat_avg$Barriers,
         Crime_diff = Crime - england_nat_avg$Crime,
         Environment_diff = Environment - england_nat_avg$Environment) %>%
  mutate(IMD_Score_r = case_when(IMD_Score_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
         Income_r = case_when(Income_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
         Employment_r = case_when(Employment_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
         Health_r = case_when(Health_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
         Education_r = case_when(Education_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
         Barriers_r = case_when(Barriers_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
         Crime_r = case_when(Crime_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
         Environment_r = case_when(Environment_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED")) %>%
  select(RC_ID, RC_Name, IMD_Score, IMD_Score_r, Income, Income_r, Employment, Employment_r, Health, Health_r,
         Education, Education_r, Barriers, Barriers_r, Crime, Crime_r, Environment, Environment_r) %>%
  setNames(c("RC_ID", "RC_Name", "imdAverage", "imdNational", "incomeAverage", "incomeNational",
             "employmentAverage", "employmentNational", "healthAverage", "healthNational", 
             "educationAverage", "educationNational", "barriersAverage", "barriersNational",
             "crimeAverage", "crimeNational", "environmentAverage", "environmentNational"))
england_profile_out <- england_rc %>% as.data.frame() %>% select(RC_ID, RC_Name, Classification)
england_profile_out <- merge(england_profile_out, england_catch_imd, by = c("RC_ID", "RC_Name"), all.x = TRUE)
england_profile_out <- england_profile_out %>% mutate_if(is.numeric, ~replace_na(., 0))
write.csv(england_profile_out, "Output Data/Deprivation/England_RetailCentres_Deprivation_Profile.csv")

### Wales
wales_catch_imd <- wales_catch_imd %>%
  mutate(IMD_Score_diff = IMD_Score - wales_nat_avg$IMD_Score,
         Income_diff = Income - wales_nat_avg$Income, 
         Employment_diff = Employment - wales_nat_avg$Employment,
         Health_diff = Health - wales_nat_avg$Health,
         Education_diff = Education - wales_nat_avg$Education,
         Services_diff = Services - wales_nat_avg$Services,
         Housing_diff = Housing - wales_nat_avg$Housing,
         Crime_diff = Crime - wales_nat_avg$Crime,
         Environment_diff = Environment - wales_nat_avg$Environment) %>%
  mutate(IMD_Score_r = case_when(IMD_Score_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
         Income_r = case_when(Income_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
         Employment_r = case_when(Employment_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
         Health_r = case_when(Health_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
         Education_r = case_when(Education_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
         Services_r = case_when(Services_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
         Housing_r = case_when(Housing_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
         Crime_r = case_when(Crime_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
         Environment_r = case_when(Environment_diff < 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED")) %>%
  select(RC_ID, RC_Name, IMD_Score, IMD_Score_r, Income, Income_r, Employment, Employment_r, Health, Health_r,
         Education, Education_r, Services, Services_r, Housing, Housing_r, Crime, Crime_r, Environment, Environment_r) %>%
  setNames(c("RC_ID", "RC_Name", "imdAverage", "imdNational", "incomeAverage", "incomeNational",
             "employmentAverage", "employmentNational", "healthAverage", "healthNational", 
             "educationAverage", "educationNational", "servicesAverage", "servicesNational",
             "housingAverage", "housingNational", "crimeAverage", "crimeNational", "environmentAverage", "environmentNational"))
wales_profile_out <- wales_rc %>% as.data.frame() %>% select(RC_ID, RC_Name, Classification)
wales_profile_out <- merge(wales_profile_out, wales_catch_imd, by = c("RC_ID", "RC_Name"), all.x = TRUE)
wales_profile_out <- wales_profile_out %>% mutate_if(is.numeric, ~replace_na(., 0))
write.csv(wales_profile_out, "Output Data/Deprivation/Wales_RetailCentres_Deprivation_Profile.csv")


### Scotland
scotland_catch_imd <- scotland_catch_imd %>%
  mutate(IMD_Rank_diff = IMD_Rank - scotland_nat_avg$IMD_Rank,
         Income_diff = Income_Rank - scotland_nat_avg$Income_Rank, 
         Employment_diff = Employment_Rank - scotland_nat_avg$Employment_Rank,
         Health_diff = Health_Rank - scotland_nat_avg$Health_Rank,
         Education_diff = Education_Rank - scotland_nat_avg$Education_Rank,
         GeographicAccess_diff = GeographicAccess_Rank - scotland_nat_avg$GeographicAccess_Rank,
         Housing_diff = Housing_Rank - scotland_nat_avg$Housing_Rank,
         Crime_diff = Crime_Rank - scotland_nat_avg$Crime_Rank) %>%
  mutate(IMD_Rank_r = case_when(IMD_Rank_diff > 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
         Income_r = case_when(Income_diff > 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
         Employment_r = case_when(Employment_diff > 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
         Health_r = case_when(Health_diff > 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
         Education_r = case_when(Education_diff > 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
         GeographicAccess_r = case_when(GeographicAccess_diff > 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
         Housing_r = case_when(Housing_diff > 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED"),
         Crime_r = case_when(Crime_diff > 0 ~ "LESS DEPRIVED", TRUE ~ "MORE DEPRIVED")) %>%
  select(RC_ID, RC_Name, IMD_Rank, IMD_Rank_r, Income_Rank, Income_r, Employment_Rank, Employment_r, Health_Rank, Health_r,
         Education_Rank, Education_r, GeographicAccess_Rank, GeographicAccess_r, Housing_Rank, Housing_r, Crime_Rank, Crime_r) %>%
  setNames(c("RC_ID", "RC_Name", "imdAverage", "imdNational", "incomeAverage", "incomeNational",
             "employmentAverage", "employmentNational", "healthAverage", "healthNational", 
             "educationAverage", "educationNational", "geographicaccessAverage", "geographicaccessNational",
             "housingAverage", "housingNational", "crimeAverage", "crimeNational"))
scotland_profile_out <- scotland_rc %>% as.data.frame() %>% select(RC_ID, RC_Name, Classification)
scotland_profile_out <- merge(scotland_profile_out, scotland_catch_imd, by = c("RC_ID", "RC_Name"), all.x = TRUE)
scotland_profile_out <- scotland_profile_out %>% mutate_if(is.numeric, ~replace_na(., 0))
write.csv(scotland_profile_out, "Output Data/Deprivation/Scotland_RetailCentres_Deprivation_Profile.csv")
