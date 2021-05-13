## Propensity for Online Shopping

library(sf)
library(tidyverse)
library(data.table)
library(readxl)

# 1. Data -----------------------------------------------------------------

## Retail Centres & Catchments
rc <- st_read("Input Data/Retail_Centres_UK_Final.gpkg")
rc <- rc %>% filter(Classification != "Small Local Centre")
catch <- st_read("Output Data/CDRC_RetailCentre_2021_DriveTimes.gpkg")

## Internet User Classification - England, Scotland & Wales
iuc <- fread("Input Data/iuc2018.csv")
iuc <- iuc[, c("LSOA11_CD", "LSOA11_NM", "GRP_CD", "GRP_LABEL")]

## Z Scores
z <- fread("Input Data/iuc2018_zscores.csv")

## Population 

### England & Wales
eng_wal <- read_excel("Input Data/2019_EW_estimates.xlsx", sheet = 4, skip = 5)
eng_wal <- eng_wal %>%
  select(1:2, 7) %>%
  setNames(c("LSOA11_CD", "LSOA11_NM", "Total_Population_2019"))
