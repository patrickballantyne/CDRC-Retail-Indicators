## Deprivation Indicators - Postcode-based method

library(tidyverse)
library(sf)
library(data.table)

# 1. Data -----------------------------------------------------------------

## Walking catchments for retail centres
# catchments <- st_read("Output Data/CDRC_RetailCentre_2021_WalkingIsolines_v4.gpkg")
## Drivetime catchments for retail centres
catchments <- st_read("Output Data/CDRC_RetailCentre_2021_DriveTimes.gpkg")
catchments <- catchments %>%
  st_transform(27700)

## Retail centre boundaries
rc <- st_read("Output Data/200721_PB_437-01_RC_Boundaries_UPDATED.gpkg")

## IMD by LSOA/DZ 
## We are using the most recent version of the IMD for each nation; 2019 for England & Wales, and 2020 for Scotland.
## Scottish IMD scores were back calculated from Ranks, using the SIMD open GitHub repo; 
## https://github.com/TheDataLabScotland/openSIMD/blob/master/scripts/calculations/openSIMD.R 
eng_imd <- st_read("Input Data/IMD/IMD_2019.shp")
scot_imd <- read.csv("Input Data/IMD/SIMD2020_SCORES.csv")
wal_imd <- read.csv("Input Data/IMD/Wales_IMD_Scores.csv")

## Postcode centroids
pc <- st_read("Input Data/GB_Postcodes.gpkg")

## Postcode to LSOA/DZ lookup 
lookup <- fread("Input Data/Postcode_Lookup_v2.csv")

## LSOA & DataZone shapefile
lsoa <- st_read("Input Data/LSOA/Lower_Layer_Super_Output_Areas_December_2011_Generalised_Clipped__Boundaries_in_England_and_Wales.shp")
dz <- st_read("Input Data/DZ/SG_SIMD_2020.shp")
  
# 2. Cleaning datasets ---------------------------------------------------------------------

## Clean IMD data for each country
eng_imd <- eng_imd %>%
  as.data.frame() %>%
  select(lsoa11cd,  IMDScore) %>%
  rename(Area_Code = lsoa11cd)
scot_imd <- scot_imd %>%
  as.data.frame() %>%
  select(data_zone, simd_score) %>%
  rename(Area_Code = data_zone, IMDScore = simd_score)
wal_imd <- wal_imd %>%
  as.data.frame() %>%
  select(LSOA_Code, WIMD2019_Score) %>%
  rename(Area_Code= LSOA_Code, IMDScore = WIMD2019_Score)

## Assemble GB IMD
imd <- rbind(eng_imd, scot_imd, wal_imd)
imd_sf <- rbind(eng_imd_sf, scot_imd_sf, wal_imd_sf)

## Clean LSOA & DZ shapefile
lsoa <- lsoa %>%
  select(lsoa11cd, geometry) %>%
  rename(Area_Code = lsoa11cd) %>%
  st_transform(27700)
dz <- dz %>%
  select(DataZone) %>%
  rename(Area_Code = DataZone) %>%
  st_transform(27700)
area <- rbind(lsoa, dz)

## Clean postcode centroids
pc_clean <- pc %>%
  select(Postcode, geom) %>%
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))
  
## Clean lookup
lookup <- lookup %>%
  select(`Postcode 1`,  `Lower Super Output Area Code`) %>%
  rename(Postcode = `Postcode 1`, Area_Code = `Lower Super Output Area Code`) %>%
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))

## Join datasets together - postcodes with attached LSOA/DZ information
pc_merge <- merge(pc_clean, lookup, by = "Postcode", all.x = TRUE)

## Some of the postcodes do not have direct matches to the LSOA/DZ lookup, so in these cases we
## will spatially join on the LSOA DZ information 

### Extract those with no LSOA
noLSOA <- pc_merge %>%
  filter(is.na(Area_Code)) %>%
  select(Postcode)
## Join those with no LSOA/DZ to LSOA/DZ shapefile
m <- st_join(noLSOA, left = FALSE, area)

## Assemble final set of postcodes with LSOA/DZ information
pc_merge <- pc_merge %>%
  filter(!is.na(Area_Code))
db <- rbind(pc_merge, m)

## Attach IMD data to postcodes, by LSOA/DZ
db_imd <- merge(db, imd, by = "Area_Code", all.x = TRUE)

# 3. Compute useful indicators --------------------------------------------

## Number of postcodes per LSOA - this will help us apply weights to postcode-level
## IMD data 
n_pc <- db_imd %>%
  as.data.frame() %>%
  select(-c(geometry, IMDScore)) %>%
  group_by(Area_Code) %>%
  summarise(n_pc = n_distinct(Postcode))

# 4. Calculating Catchment Deprivation ------------------------------------

## Function - calculates average deprivation profile of retail centre catchments, 
## by pulling in postcode level deprivation data, weighting it and using it to 
## generate an average score.
calculateProfile <- function(x) {
  ## Join LSOAs
  m <- st_join(x, left = FALSE, db_imd)
  ## Calculate number of distinct postcodes per LSOA
  n_t <- m %>%
    as.data.frame() %>%
    select(RC_ID, RC_Name, Area_Code, Postcode, IMDScore) %>%
    group_by(RC_ID, RC_Name, Area_Code, IMDScore) %>%
    summarise(n_active_pc = n_distinct(Postcode))
  ## Merge on total number of postcodes
  n_t <- merge(n_t, n_pc, by = "Area_Code", all.x = TRUE)
  ## Calculate weight based on number of unique postcodes in catchment, and 
  ## total number of unique postcodes per LSOA
  n_t$Weight <- (n_t$n_active_pc / n_t$n_pc) 
  ## Calculate average deprivation of catchment
  avg_dep <- n_t %>%
    mutate(w_IMDScore = IMDScore * Weight) %>%
    group_by(RC_ID, RC_Name) %>%
    summarise(Avg_IMDScore = mean(w_IMDScore))
  ## Merge result onto retail centre polygon
  rc_imd <- merge(rc, avg_dep, by = c("RC_ID", "RC_Name"), all.y = TRUE)
  ## Print retail centre completed
  print(paste0("DEPRIVATION PROFILE FOR", " ", rc_imd$RC_Name, " ", "COMPLETED"))
  st_write(rc_imd, "Output Data/Deprivation/RetailCentre_AverageDeprivation_DriveTimes.gpkg", append = TRUE)}

## Split dataset up - IMD by postcodes
input <- split(catchments, catchments$RC_ID)

## Apply the calculateProfile function, to extract deprivation profile for each retail
## centre - will perform slowly, better approach would be to use mclapply() if on Linux
out <- do.call(rbind, lapply(input, calculateProfile))


# 4. Presentation of Results ----------------------------------------------

## Read in data
full <- st_read("Output Data/Deprivation/CDRC_RetailCentre_DeprivationProfiles.gpkg")

## Split by country, calculate quintiles and tidy up 

### Scotland
scot_drive_dep <- full %>%
  filter(grepl("Scotland", RC_Name)) %>%
  select(-c(w_AvgIMDScore)) %>% 
  rename(AvgIMDScore = d_AvgIMDScore) %>%
  mutate(IMDDecile = case_when(AvgIMDScore < 5.4 ~ 1,
                               AvgIMDScore >= 5.4 & AvgIMDScore <9.2 ~ 2,
                               AvgIMDScore >= 9.2 & AvgIMDScore <12.1 ~ 3,
                               AvgIMDScore >= 12.1 & AvgIMDScore <14.7 ~ 4,
                               AvgIMDScore >= 14.7 & AvgIMDScore <17.0 ~ 5,
                               AvgIMDScore >= 17.0 & AvgIMDScore <19.1 ~ 6,
                               AvgIMDScore >= 19.1 & AvgIMDScore <22.0 ~ 7,
                               AvgIMDScore >= 22.0 & AvgIMDScore <24.9 ~ 8,
                               AvgIMDScore >= 24.9 & AvgIMDScore <28.4 ~ 9,
                               AvgIMDScore >= 28.4 ~ 10)) %>%
  mutate(IMDLookup = case_when(IMDDecile == 1 ~ "10% Least Deprived Nationally",
                               IMDDecile == 2 ~ "20% Least Deprived Nationally",
                               IMDDecile == 3 ~ "30% Least Deprived Nationally",
                               IMDDecile == 4 ~ "40% Least Deprived Nationally",
                               IMDDecile == 5 ~ "50% Least Deprived Nationally",
                               IMDDecile == 6 ~ "50% Most Deprived Nationally",
                               IMDDecile == 7 ~ "40% Most Deprived Nationally",
                               IMDDecile == 8 ~ "30% Most Deprived Nationally",
                               IMDDecile == 9 ~ "20% Most Deprived Nationally",
                               IMDDecile == 10 ~ "10% Most Deprived Nationally")) %>%
  select(RC_ID, RC_Name, Classification, AvgIMDScore, IMDDecile, IMDLookup)
scot_walk_dep <- full %>%
  filter(grepl("Scotland", RC_Name)) %>%
  select(-c(d_AvgIMDScore)) %>% 
  rename(AvgIMDScore = w_AvgIMDScore) %>%
  mutate(IMDDecile = case_when(AvgIMDScore < 3.4 ~ 1,
                               AvgIMDScore >= 3.4 & AvgIMDScore <6.6 ~ 2,
                               AvgIMDScore >= 6.6 & AvgIMDScore <8.9 ~ 3,
                               AvgIMDScore >= 8.9 & AvgIMDScore <10.9 ~ 4,
                               AvgIMDScore >= 10.9 & AvgIMDScore <13.0 ~ 5,
                               AvgIMDScore >= 13.0 & AvgIMDScore <16.8 ~ 6,
                               AvgIMDScore >= 16.8 & AvgIMDScore <20.5 ~ 7,
                               AvgIMDScore >= 20.5 & AvgIMDScore <25.3 ~ 8,
                               AvgIMDScore >= 25.3 & AvgIMDScore <29.6 ~ 9,
                               AvgIMDScore >= 29.6 ~ 10)) %>%
  mutate(IMDLookup = case_when(IMDDecile == 1 ~ "10% Least Deprived Nationally",
                               IMDDecile == 2 ~ "20% Least Deprived Nationally",
                               IMDDecile == 3 ~ "30% Least Deprived Nationally",
                               IMDDecile == 4 ~ "40% Least Deprived Nationally",
                               IMDDecile == 5 ~ "50% Least Deprived Nationally",
                               IMDDecile == 6 ~ "50% Most Deprived Nationally",
                               IMDDecile == 7 ~ "40% Most Deprived Nationally",
                               IMDDecile == 8 ~ "30% Most Deprived Nationally",
                               IMDDecile == 9 ~ "20% Most Deprived Nationally",
                               IMDDecile == 10 ~ "10% Most Deprived Nationally")) %>%
  select(RC_ID, RC_Name, Classification, AvgIMDScore, IMDDecile, IMDLookup)



## England
eng_drive_dep <- full %>%
  filter(grepl("England", RC_Name)) %>%
  select(-c(w_AvgIMDScore)) %>% 
  rename(AvgIMDScore = d_AvgIMDScore) %>%
  mutate(IMDDecile = case_when(AvgIMDScore < 6.0 ~ 1,
                               AvgIMDScore >= 6.0 & AvgIMDScore <8.8 ~ 2,
                               AvgIMDScore >= 8.8 & AvgIMDScore <11.6 ~ 3,
                               AvgIMDScore >= 11.6 & AvgIMDScore <14.6 ~ 4,
                               AvgIMDScore >= 14.6 & AvgIMDScore <17.9 ~ 5,
                               AvgIMDScore >= 17.9 & AvgIMDScore <21.2 ~ 6,
                               AvgIMDScore >= 21.2 & AvgIMDScore <24.7 ~ 7,
                               AvgIMDScore >= 24.7 & AvgIMDScore <28.9 ~ 8,
                               AvgIMDScore >= 28.9 & AvgIMDScore <34.1 ~ 9,
                               AvgIMDScore >= 34.1 ~ 10)) %>%
  mutate(IMDLookup = case_when(IMDDecile == 1 ~ "10% Least Deprived Nationally",
                               IMDDecile == 2 ~ "20% Least Deprived Nationally",
                               IMDDecile == 3 ~ "30% Least Deprived Nationally",
                               IMDDecile == 4 ~ "40% Least Deprived Nationally",
                               IMDDecile == 5 ~ "50% Least Deprived Nationally",
                               IMDDecile == 6 ~ "50% Most Deprived Nationally",
                               IMDDecile == 7 ~ "40% Most Deprived Nationally",
                               IMDDecile == 8 ~ "30% Most Deprived Nationally",
                               IMDDecile == 9 ~ "20% Most Deprived Nationally",
                               IMDDecile == 10 ~ "10% Most Deprived Nationally")) %>%
  select(RC_ID, RC_Name, Classification, AvgIMDScore, IMDDecile, IMDLookup)
eng_walk_dep <- full %>%
  filter(grepl("England", RC_Name)) %>%
  select(-c(d_AvgIMDScore)) %>% 
  rename(AvgIMDScore = w_AvgIMDScore) %>%
  mutate(IMDDecile = case_when(AvgIMDScore < 3.8 ~ 1,
                               AvgIMDScore >= 3.8 & AvgIMDScore <6.4 ~ 2,
                               AvgIMDScore >= 6.4 & AvgIMDScore <9.2 ~ 3,
                               AvgIMDScore >= 9.2 & AvgIMDScore <12.0 ~ 4,
                               AvgIMDScore >= 12.0 & AvgIMDScore <15.0 ~ 5,
                               AvgIMDScore >= 15.0 & AvgIMDScore <18.4 ~ 6,
                               AvgIMDScore >= 18.4 & AvgIMDScore <22.7 ~ 7,
                               AvgIMDScore >= 22.7 & AvgIMDScore <29.0 ~ 8,
                               AvgIMDScore >= 29.0 & AvgIMDScore <38.7 ~ 9,
                               AvgIMDScore >= 38.7 ~ 10)) %>%
  mutate(IMDLookup = case_when(IMDDecile == 1 ~ "10% Least Deprived Nationally",
                               IMDDecile == 2 ~ "20% Least Deprived Nationally",
                               IMDDecile == 3 ~ "30% Least Deprived Nationally",
                               IMDDecile == 4 ~ "40% Least Deprived Nationally",
                               IMDDecile == 5 ~ "50% Least Deprived Nationally",
                               IMDDecile == 6 ~ "50% Most Deprived Nationally",
                               IMDDecile == 7 ~ "40% Most Deprived Nationally",
                               IMDDecile == 8 ~ "30% Most Deprived Nationally",
                               IMDDecile == 9 ~ "20% Most Deprived Nationally",
                               IMDDecile == 10 ~ "10% Most Deprived Nationally")) %>%
  select(RC_ID, RC_Name, Classification, AvgIMDScore, IMDDecile, IMDLookup)


### Wales
wales_drive_dep <- full %>%
  filter(grepl("Wales", RC_Name)) %>%
  select(-c(w_AvgIMDScore)) %>% 
  rename(AvgIMDScore = d_AvgIMDScore) %>%
  mutate(IMDDecile = case_when(AvgIMDScore < 4.6 ~ 1,
                               AvgIMDScore >= 4.6 & AvgIMDScore <8.3 ~ 2,
                               AvgIMDScore >= 8.3 & AvgIMDScore <11.5 ~ 3,
                               AvgIMDScore >= 11.5 & AvgIMDScore <14.0 ~ 4,
                               AvgIMDScore >= 14.0 & AvgIMDScore <16.7 ~ 5,
                               AvgIMDScore >= 16.7 & AvgIMDScore <19.1 ~ 6,
                               AvgIMDScore >= 19.1 & AvgIMDScore <22.0 ~ 7,
                               AvgIMDScore >= 22.0 & AvgIMDScore <25.8 ~ 8,
                               AvgIMDScore >= 25.8 & AvgIMDScore <33.2 ~ 9,
                               AvgIMDScore >= 33.1 ~ 10)) %>%
  mutate(IMDLookup = case_when(IMDDecile == 1 ~ "10% Least Deprived Nationally",
                               IMDDecile == 2 ~ "20% Least Deprived Nationally",
                               IMDDecile == 3 ~ "30% Least Deprived Nationally",
                               IMDDecile == 4 ~ "40% Least Deprived Nationally",
                               IMDDecile == 5 ~ "50% Least Deprived Nationally",
                               IMDDecile == 6 ~ "50% Most Deprived Nationally",
                               IMDDecile == 7 ~ "40% Most Deprived Nationally",
                               IMDDecile == 8 ~ "30% Most Deprived Nationally",
                               IMDDecile == 9 ~ "20% Most Deprived Nationally",
                               IMDDecile == 10 ~ "10% Most Deprived Nationally")) %>%
  select(RC_ID, RC_Name, Classification, AvgIMDScore, IMDDecile, IMDLookup)
wales_walk_dep <- full %>%
  filter(grepl("Wales", RC_Name)) %>%
  select(-c(d_AvgIMDScore)) %>% 
  rename(AvgIMDScore = w_AvgIMDScore) %>%
  mutate(IMDDecile = case_when(AvgIMDScore < 3.0 ~ 1,
                               AvgIMDScore >= 3.0 & AvgIMDScore <5.1 ~ 2,
                               AvgIMDScore >= 5.1 & AvgIMDScore <7.3 ~ 3,
                               AvgIMDScore >= 7.3 & AvgIMDScore <10.0 ~ 4,
                               AvgIMDScore >= 10.0 & AvgIMDScore <11.8 ~ 5,
                               AvgIMDScore >= 11.8 & AvgIMDScore <14.3 ~ 6,
                               AvgIMDScore >= 14.3 & AvgIMDScore <17.6 ~ 7,
                               AvgIMDScore >= 17.6 & AvgIMDScore <21.7 ~ 8,
                               AvgIMDScore >= 21.7 & AvgIMDScore <42.1 ~ 9,
                               AvgIMDScore >= 42.1 ~ 10)) %>%
  mutate(IMDLookup = case_when(IMDDecile == 1 ~ "10% Least Deprived Nationally",
                               IMDDecile == 2 ~ "20% Least Deprived Nationally",
                               IMDDecile == 3 ~ "30% Least Deprived Nationally",
                               IMDDecile == 4 ~ "40% Least Deprived Nationally",
                               IMDDecile == 5 ~ "50% Least Deprived Nationally",
                               IMDDecile == 6 ~ "50% Most Deprived Nationally",
                               IMDDecile == 7 ~ "40% Most Deprived Nationally",
                               IMDDecile == 8 ~ "30% Most Deprived Nationally",
                               IMDDecile == 9 ~ "20% Most Deprived Nationally",
                               IMDDecile == 10 ~ "10% Most Deprived Nationally")) %>%
  select(RC_ID, RC_Name, Classification, AvgIMDScore, IMDDecile, IMDLookup)









### Assemble for release
walk_dep <- rbind(scot_walk_dep, eng_walk_dep, wales_walk_dep)
walk_dep <- walk_dep %>%
  mutate(Country = case_when(grepl("Scotland", RC_Name) ~ "Scotland",
                             grepl("England", RC_Name) ~ "England", 
                             grepl("Wales", RC_Name) ~ "Wales")) %>%
  select(RC_ID, RC_Name, Country, Classification, AvgIMDScore, IMDDecile, IMDLookup, geom) %>%
  arrange(-IMDDecile, Country)
st_write(walk_dep, "Output Data/Deprivation/CDRC_RetailCentre_WalkingDeprivation_v2.gpkg")


drive_dep <- rbind(scot_drive_dep, eng_drive_dep, wales_drive_dep)
drive_dep <- drive_dep %>%
  mutate(Country = case_when(grepl("Scotland", RC_Name) ~ "Scotland",
                             grepl("England", RC_Name) ~ "England", 
                             grepl("Wales", RC_Name) ~ "Wales")) %>%
  select(RC_ID, RC_Name, Country, Classification, AvgIMDScore, IMDDecile, IMDLookup, geom) %>%
  arrange(-IMDDecile, Country)
st_write(drive_dep, "Output Data/Deprivation/CDRC_RetailCentre_DrivingDeprivation_v2.gpkg")



# Appendices --------------------------------------------------------------

## Assembling a spatial GB IMD layer ##################################

## IMD by LSOA/DZ - Scottish IMD scores were back calculated from Ranks, using the SIMD open GitHub repo
eng_imd <- st_read("Input Data/IMD/IMD_2019.shp")
scot_imd <- read.csv("Input Data/IMD/SIMD2020_SCORES.csv")
wal_imd <- read.csv("Input Data/IMD/Wales_IMD_Scores.csv")

## Clean English IMD
eng_imd <- eng_imd %>%
  select(lsoa11cd,  IMDScore) %>%
  rename(Area_Code = lsoa11cd)
scot_imd <- scot_imd %>%
  select(data_zone, simd_score) %>%
  rename(Area_Code = data_zone, IMDScore = simd_score)
wal_imd <- wal_imd %>%
  select(LSOA_Code, WIMD2019_Score) %>%
  rename(Area_Code= LSOA_Code, IMDScore = WIMD2019_Score)

## Merge scottish IMD onto DZ
dz_scot <- merge(dz, scot_imd, by = "Area_Code")
## Merge welsh IMD onto LSOAs
lsoa_wal <- merge(lsoa, wal_imd, by = "Area_Code", all.y = TRUE)

## Assemble shapefile
gb_imd <- rbind(eng_imd, dz_scot, lsoa_wal)
st_write(gb_imd, "Output Data/Deprivation/GB_Deprivation.gpkg")


## Checking output & obtaining results ######################################

## Read in and check dep - we are missing the one for Belfast City Centre, 
## due to lack of NI deprivation data
dep <- st_read("Output Data/Deprivation/RetailCentre_AverageDeprivation.gpkg")

## Identify top 10 most deprived
top10 <- dep %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, Avg_IMDScore) %>%
  arrange(desc(Avg_IMDScore)) %>%
  slice(1:10)

## Ten least deprived
bottom10 <- dep %>%
  as.data.frame() %>%
  select(RC_ID, RC_Name, Avg_IMDScore) %>%
  arrange(Avg_IMDScore) %>%
  slice(1:10)
