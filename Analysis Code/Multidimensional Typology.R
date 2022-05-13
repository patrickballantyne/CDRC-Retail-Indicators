## Multidimensional Typology
library(tidyverse)
library(sf)
library(data.table)
library(corrplot)
library(caret)
library(plyr)
library(cluster)
library(ggcorrplot)
library(ggstatsplot)
library(lares)
source("Source Code/Michalis_Clustergram.R")
source("C:/Users/sgpballa/Google Drive/Patrick Academic/POSTGRAD/PhD/PhD Work/Papers/PAPER TWO/USRetailCentres/Source Code/Helper Functions - Typology.R")
options(scipen = 999)


# 1.  Data & Pre-Processing -----------------------------------------------

## Read in the inputs
openInputs <- read.csv("Output Data/Multidimensional Typology/Open_Inputs.csv")
secureInputs <- read.csv("Output Data/Multidimensional Typology/Secure_Inputs_v2.csv")

## Assemble
inputs <- merge(openInputs, secureInputs, by = "RC_ID")
inputs <- inputs %>%
  select(RC_ID,
         
         propClothingandFootwear, propDIYandHousehold, propElectricalandHomeEntertainment, propRecreational,
         propFood, propCTNOffLicenceForecourt, propChemist, propRestaurantsandFastFood, propPubsBarsandClubs, 
         propCafe, propEntertainmentandFitness, propHealthandBeauty, propConsumerServices, propHouseholdServices, propBusinessServices,
         
         propIndependent, propSmallMultiple, propLargeMultiple, 
         tenantMix, comparisonDiversity, convenienceDiversity, leisureDiversity, serviceDiversity,
         propCloneRetailers, propCloneCategories,
         
         n.LDC.2020.x, centreArea, geographicReach, roeckScore, 
         attractivenessScore, propAnchors, propPremium, propMass, 
         propValue, propCharity, propPawnbrokersBetting, propConcession,
         
         nCompeting, propVacant, propStructuralVacant, propVacantChange,
         onlineExposure, retailService, retailChange, averageIMD, nightPopulation) %>%
  dplyr::rename(nUnits = n.LDC.2020.x, propAnchor = propAnchors)

## Get ready for analysis
inputs <- as.data.table (inputs)
rc <- inputs %>%
  select(RC_ID)

## Identify highest correlations
corr_cross(inputs, # name of dataset
           max_pvalue = 0.05, # display only significant correlations (at 5% level)
           top = 20 # display top 10 couples of variables (by correlation coefficient)
)

## Vars to remove
rm_vars <- c("RC_ID", 
             "propLargeMultiple", 
             "tenantMix",
             "propCloneCategories", 
             "propStructuralVacant")
inputs_clean <- inputs %>%
  select(-c(RC_ID, propLargeMultiple, tenantMix, propCloneCategories, propStructuralVacant))

## Recheck correlations
corr_cross(inputs_clean, # name of dataset
           max_pvalue = 0.05, # display only significant correlations (at 5% level)
           top = 20 # display top 10 couples of variables (by correlation coefficient)
)


# 2.  Identifying k -------------------------------------------------------

## Transform - Box Cox (Michalis Method)
t_inputs <- do.call(cbind, lapply(1:ncol(inputs_clean), function(x) unlist(ifelse(min(inputs_clean[, x, with = F]) < 1, inputs_clean[,x, with = F] + 1, inputs_clean[,x, with = F]))))
colnames(t_inputs) <- names(inputs_clean)
trans <- preProcess(t_inputs, method = "BoxCox")
data_sele_trans <- as.data.frame(predict(trans, t_inputs))

## Transform
typ_z <- as.data.frame(scale(inputs_clean, center = TRUE, scale = TRUE))
scale2 <- function(x) (scales::rescale(x, to = c(0 , 1)))
typ_s <- typ_z %>% mutate_all(., scale2)

## Clustergram - z_std = F set to show variation between clusters
cl_list <- get_clustergram(Df = data_sele_trans, z_std = F, which_many = "many_pam") 
plot(cl_list[[2]])

#ggsave("Output Data/Multidimensional Typology/Clustergram_bw.tiff", plot(cl_list[[2]]))

## Silhouette scores
ss <- get_silhouette_scores(typ_z, 999)
ss

# 3.  PAM cluster analysis (GROUPS) ------------------------------------------------

## Run typology
pm <- run_typology(typ_z, 4)
md <- pm[[2]]

## Plot medoids
plot_medoids(pm)

## Extract results
supergroups <- pm[[1]]
supergroups <- supergroups %>%
  select(cluster) %>%
  dplyr::rename(supergroupID = cluster) %>% 
  mutate(supergroupName = case_when(supergroupID == 1 ~ "Local 'everyday' goods and service centres",
                                    supergroupID == 2 ~ "Retail and shopping parks",
                                    supergroupID == 3 ~ "Leading comparison and leisure destinations",
                                    supergroupID == 4 ~ "Traditional high streets and market towns")) %>% 
  cbind(., rc) %>%
  select(RC_ID, supergroupID, supergroupName) %>%
  mutate(RC_ID = as.factor(RC_ID),
         supergroupID = as.factor(supergroupID),
         supergroupName = as.factor(supergroupName))
  

## Plot them individually
cl1 <- md %>%
  filter(cluster == 1)
ggplot(data = cl1) +
  aes(x = reorder(variable, -cluster_vals), y = cluster_vals, fill = pos) +
  geom_col(position = "identity", size = 0.25, colour = "black") + 
  scale_fill_manual(values = c("#FFDDDD", "#CCEEFF"), guide = FALSE) +
  xlab("Variable") +
  ylab("Median Values") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), axis.ticks = element_line(),
  axis.line = element_line(colour = "black"), 
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank())

cl2 <- md %>%
  filter(cluster == 2)
ggplot(data = cl2) +
  aes(x = reorder(variable, -cluster_vals), y = cluster_vals, fill = pos) +
  geom_col(position = "identity", size = 0.25, colour = "black") + 
  scale_fill_manual(values = c("#FFDDDD", "#CCEEFF"), guide = FALSE) +
  xlab("Variable") +
  ylab("Median Values") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), axis.ticks = element_line(),
        axis.line = element_line(colour = "black"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

cl3 <- md %>%
  filter(cluster == 3)
ggplot(data = cl3) +
  aes(x = reorder(variable, -cluster_vals), y = cluster_vals, fill = pos) +
  geom_col(position = "identity", size = 0.25, colour = "black") + 
  scale_fill_manual(values = c("#FFDDDD", "#CCEEFF"), guide = FALSE) +
  xlab("Variable") +
  ylab("Median Values") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), axis.ticks = element_line(),
        axis.line = element_line(colour = "black"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

cl4 <- md %>%
  filter(cluster == 4)
ggplot(data = cl4) +
  aes(x = reorder(variable, -cluster_vals), y = cluster_vals, fill = pos) +
  geom_col(position = "identity", size = 0.25, colour = "black") + 
  scale_fill_manual(values = c("#FFDDDD", "#CCEEFF"), guide = FALSE) +
  xlab("Variable") +
  ylab("Median Values") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), axis.ticks = element_line(),
        axis.line = element_line(colour = "black"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

# ## Radial plot - getting data ready
# md <- pm[[2]]
# md$cluster <- as.factor(md$cluster)
# var_order <- as.data.frame(colnames(inputs_clean))
# var_order <- rowid_to_column(var_order, "order")
# var_order <- var_order %>%
#   setNames(c("order", "variable"))
# md <- merge(md, var_order, by = "variable", all.x = TRUE)
# 
# ## Build plot
# ggplot(md, aes(y = cluster_vals, x = reorder(variable, order),
#                 group = cluster, colour = cluster)) +
#   coord_polar() + 
#   geom_point() + 
#   geom_path() + 
#   labs(x = NULL, y = "Cluster values",
#        group = "Cluster",
#        title = "Multidimensional Typology Radial Plot") +
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
# 
# ## Dot chart
# ggpubr::ggdotchart(
#   md, x = "variable", y = "cluster_vals", 
#   group = "cluster", color = "cluster", palette = "jco",
#   add = "segment", position = position_dodge(0.3),
#   sorting = "descending"
# )

# 4.  PAM Cluster Analysis (GROUPS) --------------------------------------

## Get data - group-level classification w/ input variables
pm_out <- pm[[1]]
pm_out$rcID <- inputs$RC_ID

## Number in each
count <- pm_out %>%
  select(cluster) %>%
  group_by(cluster) %>%
  add_count()

## Extract nested types - group 1
types_g1 <- lapply(n, function(x) get_nested_types(pm_out, cl = 1, medoids = TRUE))
types_g1_n <- lapply(n, function(x) get_nested_types(pm_out, cl = 1, medoids = FALSE))
types_g1_un <- do.call(rbind, types_g1)
types_g1_n <- do.call(rbind, types_g1_n)
plot_type_medoids(types_g1_un)

## Extract nested types - group 2
types_g2 <- lapply(n, function(x) get_nested_types(pm_out, cl = 2, medoids = TRUE))
types_g2_n <- lapply(n, function(x) get_nested_types(pm_out, cl = 2, medoids = FALSE))
types_g2_un <- do.call(rbind, types_g2)
types_g2_n <- do.call(rbind, types_g2_n)
plot_type_medoids(types_g2_un)

## Extract nested types - group 3
types_g3 <- lapply(n, function(x) get_nested_types(pm_out, cl = 3, medoids = TRUE))
types_g3_n <- lapply(n, function(x) get_nested_types(pm_out, cl = 3, medoids = FALSE))
types_g3_un <- do.call(rbind, types_g3)
types_g3_n <- do.call(rbind, types_g3_n)
plot_type_medoids(types_g3_un)

## Extract nested types - group 4
types_g4 <- lapply(n, function(x) get_nested_types(pm_out, cl = 4, medoids = TRUE))
types_g4_n <- lapply(n, function(x) get_nested_types(pm_out, cl = 4, medoids = FALSE))
types_g4_un <- do.call(rbind, types_g4)
types_g4_n <- do.call(rbind, types_g4_n)
plot_type_medoids(types_g4_un)

## Formatting for joining to the main dataset
groups <- rbind(types_g1_n, types_g2_n, types_g3_n, types_g4_n)
groups <- groups %>% 
  select(-c(type)) %>%
  setNames(c("RC_ID", "supergroupID", "groupID")) %>%
  mutate(groupIDnew = case_when(groupID == "2.1" ~ "2.2",
                                groupID == "2.2" ~ "2.1",
                                TRUE ~ groupID)) %>%
  select(-c(groupID)) %>%
  dplyr::rename(groupID = groupIDnew) %>%
  mutate(groupName = case_when(groupID == "1.1" ~ "Local urban convenience centres",
                               groupID == "1.2" ~ "District urban service centres",
                               groupID == "2.1" ~ "Primary shopping centres and premium destinations",
                               groupID == "2.2" ~ "Secondary retail parks and shopping centres",
                               groupID == "3.1" ~ "Large regional retail and leisure destinations",
                               groupID == "3.2" ~ "Sub-regional retail and leisure destinations",
                               groupID == "4.1" ~ "Mass and value high streets",
                               groupID == "4.2" ~ "Indie high streets")) %>%
  mutate(RC_ID = as.factor(RC_ID),
         supergroupID = as.factor(supergroupID),
         groupID = as.factor(groupID),
         groupName = as.factor(groupName))




# 5. Outputs --------------------------------------------------------------

## Retail centre shapefile to append to
rc_shp <- st_read("Output Data/200721_PB_437-01_RC_Boundaries_UPDATED.gpkg")
rc_shp <- rc_shp %>%
  select(RC_ID, RC_Name)

## Assemble full list of Supergroups and Groups
typology <- merge(supergroups, groups, by = c("RC_ID", "supergroupID"))
write.csv(typology, "Output Data/Multidimensional Typology/RELEASE/typology_lookup_2022.csv")

## Assemble shapefile
rc_typ <- merge(rc_shp, typology, by = "RC_ID", all.y = TRUE)
st_write(rc_typ, "Output Data/Multidimensional Typology/RELEASE/typology_2022.gpkg")
