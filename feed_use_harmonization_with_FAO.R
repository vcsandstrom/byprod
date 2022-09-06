# Calculate livestock + aquafeed feed use for 2016-2018 and harmonize livestock + aquafeed feed use numbers with FAO  SUA

# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# data should be its subfolder
  
library(tidyr)

years = c(2016,2017,2018) # Choose the mean of which three years you want to assess
# For the harmonization we will use 2016-2018

# Read in feed use as-fed basis
source("fish_feed.R") # unit: tonnes
fish_feed = fish_feed(years)
#fish_feed = read.csv("outputs/fish_feed_updated.csv", check.names = F) # Read in the saved outputs for time saving
source("livestock_feed.R") # Unit: tonnes
livestock_feed = livestock_feed(years)
source("Byproducts.R")

fish_feed$Item = "fish_feed"

# Feed use
Total_feed = rbind(x=fish_feed, y=livestock_feed)

feed_itemcodes = read.csv("data/feed_itemcodes.csv", check.names = F)
Total_feed = merge(Total_feed, feed_itemcodes, by="feed_material" )
feed_use <- Total_feed %>%
  group_by(subgroup2) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  select(-c(`Area Code`, `Item Code`, cf))
# aggregate the feed use to match the FAO SUA aggregated groups
# FAO SUA does not have data for by-product feed use so only the major feed/food groups remain here
# livestock and aquaculture by-products are not included in the harmonization

# FAO SUA
# load prod data (previously read from csv files and saved as Rdata)
#FAO_SUA = read.csv("Data/SUA_Crops_Livestock_E_All_Data_(Normalized).csv", check.names = F)
#save(FAO_SUA, file="data/FAO_SUA.Rdata")
load("data/FAO_SUA.Rdata")
FAO_SUA = FAO_SUA[FAO_SUA$`Element Code` %in% c(5520),] # choose elements feed 
FAO_SUA = spread(FAO_SUA[,-c(7,9,11)],  Year,Value)
FAO_SUA$Value = rowSums(FAO_SUA[,which(colnames(FAO_SUA) %in% years)])/3
FAO_SUA = spread(FAO_SUA[,-c(7:11)],Element, Value)

# cereals
FAO_SUA_cereals = FAO_SUA[FAO_SUA$`Item Code` %in% c(56, 15,44, 125, 83, 79, 27),c(3,4,6)] #choose maize, wheat, barley, cassava, sorghum, millet, rice

# pulses
FAO_SUA_pulses = FAO_SUA[FAO_SUA$`Item Code` %in% c(203,176,181, 191, 195, 201, 210, 187, 197, 211, 205),c(3,4,6)] #choose bambara beans, beans dry,broad beans, chick peas, cow peas 
# dry, lentils, lupins, peas dry, pigeon peas, pulses nes, vetches

# oilseed, oils
FAO_SUA_oil = FAO_SUA[FAO_SUA$`Item Code` %in% c(271, 237, 268, 257, 340, 290),c(3,4,6)] #choose rapeseed oil, soybean oil, sunflower oil, oil palm, oil vegetable origin nes, oil sesame seed
FAO_SUA_oil = sum(FAO_SUA_oil$Feed, na.rm=T) 
# Split the oilseeds to oilseed oils and add to oilseed oils
FAO_SUA_oilseed = FAO_SUA[FAO_SUA$`Item Code` %in% c(270,236, 267, 256, 339, 289),c(3,4,6)] #choose rapeseed, soybean, sunflower seed, palm kernels, oilseed nes, sesame seed
FAO_SUA_oilseed = sum(FAO_SUA_oilseed$Feed, na.rm=T) 
FAO_SUA_oil_oilseed = FAO_SUA_oilseed*0.18 # conversion factors 0.18 for oilseed oils and 0.79 for oilseed meals
FAO_SUA_oil = FAO_SUA_oil+FAO_SUA_oil_oilseed

#molasses
FAO_SUA_molasses = FAO_SUA[FAO_SUA$`Item Code` %in% c(165),c(3,4,6)] #choose molasses 

# oilseed meals
# There is no data of oilseed meals production in FAO SUA and the FAO commodity balances have data only until 2013
# so we harmonize the oilseed meals data so that the feed use can be max the production
omeals_prod = crop_byprod(years)
omeals_prod_sum <- omeals_prod[omeals_prod$feed_material=="oilseed_meals",] %>%
  group_by(feed_material) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))


# Calculate the harmonization factors
harmonization_SUA <-cbind(feed_use[,1],rowMedians(as.matrix(feed_use[,c(2:501)]),na.rm=TRUE))
colnames(harmonization_SUA)[2]="feed_material_use"
harmonization_SUA$FAO_feed_use = NA
harmonization_SUA <- harmonization_SUA %>%
  mutate(FAO_feed_use = ifelse(subgroup2 == "cereals", sum(FAO_SUA_cereals$Feed, na.rm=T),
                               ifelse(subgroup2 == "pulses", sum(FAO_SUA_pulses$Feed, na.rm=T) ,
                                      ifelse(subgroup2 == "oilseed_oils", FAO_SUA_oil+FAO_SUA_oil_oilseed,
                                             ifelse(subgroup2 == "molasses", sum(FAO_SUA_molasses$Feed, na.rm=T), 
                                                    ifelse(subgroup2 == "oilseed_meals", omeals_prod_sum$byprod_value ,NA))))))
harmonization_SUA$hf = harmonization_SUA$FAO_feed_use/harmonization_SUA$feed_material_use



# Export harmonization factors
harmonization_SUA <- harmonization_SUA[complete.cases(harmonization_SUA), ]
write.csv(harmonization_SUA[,c(1,4)], file= "outputs/hf_feed.csv", row.names=F)

