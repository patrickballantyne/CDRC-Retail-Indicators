# CDRC-Retail-Indicators 

Development of Indicators for CDRC Retail Centres (2021) Release. 

## Data

2021 Retail Centres are available to download for free from the Consumer Data Research Centre (CDRC) [HERE](https://data.cdrc.ac.uk/dataset/retail-centre-boundaries).

We also utilise a series of other datasets to develop the indicators for the centres:

- Local Data Company (LDC) Retail Type & Vacancy dataset [SECURE], from 2018-2020, available [HERE](https://data.cdrc.ac.uk/dataset/local-data-company-retail-type-or-vacancy-classification).
- Internet User Classification (IUC) from 2018, available [HERE](https://data.cdrc.ac.uk/dataset/internet-user-classification).
- Population Estimates for England/Wales (LSOA) & Scotland (DataZone), available [HERE](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates) and [HERE](https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/2011-based-special-area-population-estimates/small-area-population-estimates/mid-2019).

## Indicators

Indicators are developed for a subset of the 6,423 Retail Centres, with sufficient LDC coverage and those larger than 'Small, Local Centres'. The indicators are:

- Retail Centre Composition - proportions of different types of retail (e.g. Comparison, Service).
- Retail Centre Vacancy - proportions of vacant units, short and longer term change in vacancy rates.
- Retail Centre Diversity - proportions of independents and national chains, 'clone town' score.
- Online Exposure -  exposure to neighbourhoods with high/low internet engagement.
- Catchments - drive-time catchments, varying in size by centre classification (e.g. Regional Centre 20mins, Local Centre 10 mins).

## Code

Code is available for some of the indicators:

- [Online Exposure](https://github.com/patrickballantyne/CDRC-Retail-Indicators/blob/main/Analysis%20Code/Calculating%20Online-Exposure.R).
- [Catchments](https://github.com/patrickballantyne/CDRC-Retail-Indicators/blob/main/Analysis%20Code/Drive-Time%20Catchments.R). 

Code for the other variables is unfortunately not available, as these indicators were derived using the secure LDC dataset.
