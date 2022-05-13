# CDRC-Retail-Indicators 
### Patrick Ballantyne, Les Dolega, Alex Singleton

----

Welcome to the CDRC Retail Centre Indicators repository. Here we provide detailed supporting information about the creation of some additional indicators and a multidimensional typology for the 2021 release of the CDRC Retail Centres data product.

## 1. Indicators

Indicators are developed for a subset of the 2021 release of the CDRC 'Retail Centre Boundaries' data product, available [HERE](https://data.cdrc.ac.uk/dataset/retail-centre-boundaries).

You can explore our indicators on the new CDRC 'Mapmaker' platform; check it out [HERE](https://mapmaker.cdrc.ac.uk/#/retail-centres?m=pctclon&lon=-2.9737&lat=53.4065&zoom=13.42).

### 1.1 Download the Indicators

You can download the indicators in two difference formats; Open-Access and SafeGuarded. For the latter, a formal application to the CDRC will be required, outlining how you intend to use the indicators.

### Open-Access - available [HERE](https://data.cdrc.ac.uk/dataset/retail-centre-boundaries). 

For a subset of the retail centres we provide:
- Clone town measure - presence of 'clone' retailers and retail categories.
- Updated E-resilience score - an updated version of the score developed in [Singleton et al. (2016)](https://www.sciencedirect.com/science/article/pii/S0016718515301500).
- Deprivation indices - Average deprivation of retail centre catchments, utilising a postcode-based method.

We also provide retail centre catchments, delineated using the HERE REST API:
- Drive-time catchments for all centres not classified as 'Small Local Centres'.
- Walking catchments for all centres not classified as 'Small Local Centres'.

### Safeguarded - available [HERE](https://data.cdrc.ac.uk/dataset/retail-centre-indicators). 

For a subset of the retail centres we provide:
- Composition indicators: proportions of different types of retail (e.g. Comparison, Service).
- Diversity indicators: ownership (independent, chains), clone town likelihood score.
- Vacancy indicators: proportions of vacant units, short & long-term vacancy rates

### 1.2 Code

Code is available to see how some of these indicators were assembled:

- [E-Resilience Score: Calculating Online Exposure](https://github.com/patrickballantyne/CDRC-Retail-Indicators/blob/main/Analysis%20Code/Calculating%20Online-Exposure.R).
- [Drive-Time & Walking Catchments](https://github.com/patrickballantyne/CDRC-Retail-Indicators/blob/main/Analysis%20Code/Retail%20Centre%20Catchments.R).
- [Deprivation: Calculating Deprivation Exposure](https://github.com/patrickballantyne/CDRC-Retail-Indicators/blob/main/Analysis%20Code/Postcode-based%20Deprivation%20Profiles.R).

Code for the other indicators (Clone Town likelihood, full E-Resilience score) is unfortunately not available, as these indicators were derived using the secure LDC dataset in the UCL Data Safe Haven.

----

## 2. Retail Centre (Multidimensional) Typology

The 2022 Retail Centre Typology presents an updated multidimensional typology of retail and consumption spaces across Great Britain. Retail presence within towns and city centres has become increasingly combined with other uses such as services and leisure activities, and the expansion of online retailing is causing a net loss of demand for some forms of retail floor space and change in function for others. This data product can be used to understand and comprehend the spatial provision of retailing and service activity, and better unpack the transformed roles and functions of consumption spaces. 

### 2.1 Download the Typology

The typology is available to download as a geopackage [HERE](https://github.com/patrickballantyne/CDRC-Retail-Indicators/blob/main/Output%20Data/Multidimensional%20Typology/RELEASE/typology_2022.gpkg), or as a lookup table [HERE](https://github.com/patrickballantyne/CDRC-Retail-Indicators/blob/main/Output%20Data/Multidimensional%20Typology/RELEASE/typology_lookup_2022.csv). 

### 2.2 Interpret the Typology

A supporting manual detailing the creation of the typology and pen portraits for the different supergroups and groups is available to download [HERE](https://github.com/patrickballantyne/CDRC-Retail-Indicators/blob/main/Output%20Data/Multidimensional%20Typology/RELEASE/typology2022userguide_v2.docx). 

### 2.3 Code

Code is also available to see how parts of the typology were assembled:

- [Calculating input variables using datasets available outside the DSH](https://github.com/patrickballantyne/CDRC-Retail-Indicators/blob/main/Analysis%20Code/Assembling%20OPEN%20typology%20variables.R)
- [Assembling the retail centre typology, including feature selection](https://github.com/patrickballantyne/CDRC-Retail-Indicators/blob/main/Analysis%20Code/Multidimensional%20Typology.R)
