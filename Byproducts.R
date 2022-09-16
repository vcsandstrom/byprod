# Compiling data of by-products and residues of crop , livestock and fish production

# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# data should be its subfolder

library(tidyr)
library(matrixStats)
library(dplyr)

###parallel processing ###
library(parallel)

# x=c(2016,2017,2018) # The years used in this study


# 1. Crops

# 1.1. Crop residues

crop_res <- function(x) {
  
  ##creating clusters ###
  # Use the detectCores() function to find the number of cores in system
  no_cores <- detectCores()
  # Setup cluster
  clust <- makeCluster(no_cores) #This line will take time
  
  start.time <- Sys.time()
# import data on the crop production and the residue production rates
#FAO_prod = read.csv("data/Production_Crops_E_All_Data_(Normalized).csv", header=T, sep=",", check.names = F)
#save(FAO_prod, file="data/FAO_prod.Rdata")

# load prod data (previously read from csv files and saved as Rdata)
load("data/FAO_prod.Rdata")

RPR= read.csv("data/RPR_updated_long.csv", header=T, sep=",", check.names = F) # Residues to production ratios

FAO_prod = FAO_prod[which(FAO_prod$`Element Code`=="5510"),] # choose only production

# Calculate the mean for selected years
FAO_prod= FAO_prod[FAO_prod$Year %in% x,]
FAO_prod = spread(FAO_prod[,-c(7,11)], Year, Value)
FAO_prod$mean = rowMeans(FAO_prod[,c(8,9,10)])

# Multiply the production quantities with the Residues-to-Production-Ratios (RPR) and the used proportions
# First select only crop residues from rice, cereals, sugar cane and pulses that are most used as feed
selected = c(44,89,94,56,79,75,27,71,83,97,15,203,176,181,191,195,201,187,197,211,125,414,417,420,156)
crop_res = merge(x=FAO_prod, y=RPR[RPR$`Item Code` %in% selected,c(1,2,5,7,8)], by=c("Item Code", "Area Code"))
crop_res$Residues = crop_res$mean* crop_res$Dry_matter *crop_res$RPR *crop_res$UsedRes
# Here we have the amount of crop residues presented as tonnes of dry matter

### Monte carlos to add uncertainty in the RPRs ###
## with a cv 0.1 and normal distribution
cv<-0.1
clusterExport(clust, "cv",envir=environment())
## define function for parallel processing ##
rand_sample<-function(x,y) {
  round(rnorm(1,x,y*x),2)}
environment(rand_sample) <- .GlobalEnv
#################################

crop_res_sens<-matrix(NA, dim(crop_res)[1],500)

set.seed(3)
for (i in 1:500){
  crop_res_sens[,i]<-crop_res$mean*(crop_res$Dry_matter)*crop_res$UsedRes*parSapply(clust,crop_res$RPR,FUN=rand_sample,y=cv)

}

crop_res_sens = cbind(crop_res[,c(2,3)], crop_res_sens)
#Aggregate to country-level
crop_res_sens <- crop_res_sens %>%
  group_by(Area, `Area Code`) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))

# Remove China 41 and separate China provinces to avoid double accounting (there is China 351)
crop_res_sens <- crop_res_sens%>%
  filter(! `Area Code` %in% c(41,96,214)) # Remove the separate China provinces

end.time <- Sys.time()
time.taken <- end.time - start.time
show(time.taken)
stopCluster(clust)
return(crop_res_sens)

}
#write.csv(crop_res_sens, file="outputs/crop_res_results_2016_2018.csv", row.names =F )


# 1.2 Agro-industrial by-products from crop production

crop_byprod <- function(x) {
  
# load prod data (previously read from csv files and saved as Rdata)
#FAO_SUA = read.csv("Data/SUA_Crops_Livestock_E_All_Data_(Normalized).csv", check.names = F)
#save(FAO_SUA, file="data/FAO_SUA.Rdata")
load("data/FAO_SUA.Rdata")

FAO_SUA_prod = FAO_SUA[which(FAO_SUA$`Element Code`=="5510"),] # choose only elements production 

# brans
bran = FAO_SUA_prod[which(FAO_SUA_prod$`Item Code`%in% c(17,35,47,59,81,73,77,85,85,91,96,99,112,213)),]
bran = bran[bran$Year %in% x, ] # choose the selected years
bran = spread(bran[,-7], Year,Value)
bran$byprod_value = rowMeans(bran[9:11])
bran = aggregate(data=bran, byprod_value~Area +`Area Code`, FUN=sum)
bran$feed_material = "bran"

# molasses
mol = FAO_SUA_prod[which(FAO_SUA_prod$`Item Code` == 165),]
mol = mol[mol$Year %in% x, ] # choose the selected years
mol = spread(mol[,-c(7,11)], Year,Value)
mol$byprod_value = rowMeans(mol[8:10])
mol$feed_material = "molasses"

# sugar beet pulp we need to calculate from the amounts of sugar beet going to processing
pulp = FAO_SUA[which(FAO_SUA$`Item Code` == 157 & FAO_SUA$`Element Code` == 5023),] # choose sugar beet processed
pulp = pulp[pulp$Year %in% x, ] # choose the selected years
pulp = spread(pulp[,-c(7,11)], Year,Value)
pulp$mean = rowMeans(pulp[8:10])
pulp$pulp = pulp$mean*0.07 # From FAO technical conversion factors, pulp = 7% of sugarbeet processing
# Include waste % for processing from FAO 2011
waste = read.csv("data/Foodloss_FAO_2011.csv", check.names = F)
countries = read.csv("data/countries.csv", check.names = F)
waste = merge(x=waste, countries[,c(2,16)], by="FAO_foodloss_country_group", all.x=T)
waste = waste[waste$FAO_foodloss_item_group== "roots and tubers",c(5,8)] #Choose the waste numbers fo
pulp = merge(waste, pulp, by="Area Code")
pulp$byprod_value =pulp$pulp*(1-pulp$Processing_packaging) 
pulp$feed_material = "pulp"

# Oilseed meals we need to calculate from the amounts of oilseeds going to processing
omeals = FAO_SUA[which(FAO_SUA$`Item Code` %in% c(270,236, 267, 256, 339, 289, 329, 243) & FAO_SUA$`Element Code` == 5023),] # choose rapeseed, soybean, sunflower seed, palm kernels, oilseed nes, sesame seed, groundnut 243, cottonseed processed
omeals = omeals[omeals$Year %in% x, ] # choose the selected years
omeals = spread(omeals[,-c(7,11)], Year,Value)
omeals$mean = rowMeans(omeals[8:10])
omeals = aggregate(data=omeals, mean~ `Area Code`+ Area, FUN=sum)
omeals$omeals = omeals$mean*0.79 # From FAO technical conversion factors, conversion factors 0.18 for oilseed oils and 0.79 for oilseed meals
# Include waste % for processing from FAO 2011
waste = read.csv("data/Foodloss_FAO_2011.csv", check.names = F)
countries = read.csv("data/countries.csv", check.names = F)
waste = merge(x=waste, countries[,c(2,16)], by="FAO_foodloss_country_group", all.x=T)
waste = waste[waste$FAO_foodloss_item_group== "Oilseeds and pulses",c(5,8)] #Choose the waste numbers
omeals = merge(x=waste, y=omeals, by="Area Code", all.y=T)
omeals$byprod_value =omeals$omeals*(1-omeals$Processing_packaging) 
omeals$feed_material = "oilseed_meals"

# Citrus pulp
citrusp = FAO_SUA[which(FAO_SUA$`Item Code` %in% c(490,497,495) & FAO_SUA$`Element Code` == 5023),] # choose item codes lemons and limes 497, Oranges 490, Tangerines, mandarins, clementines, satsumas 495
                                                                                      # processed element code 5023
citrusp = citrusp[citrusp$Year %in% x, ] # choose the selected years
citrusp = spread(citrusp[,-c(7,11)], Year,Value)
citrusp$mean = rowMeans(citrusp[8:10])
citrusp = aggregate(data=citrusp, mean~ `Area Code`+ Area, FUN=sum)
citrusp$citrusp = citrusp$mean*0.40 # From FAO technical conversion factors, citrus pulp = 40% of citrus fruit processing
# Include waste % for processing from FAO 2011
waste = read.csv("data/Foodloss_FAO_2011.csv", check.names = F)
countries = read.csv("data/countries.csv", check.names = F)
waste = merge(x=waste, countries[,c(2,16)], by="FAO_foodloss_country_group", all.x=T)
waste = waste[waste$FAO_foodloss_item_group== "Fruits and vegetables",c(5,8)] #Choose the waste numbers fo
citrusp = merge(waste, citrusp, by="Area Code")
citrusp$byprod_value =citrusp$citrusp*(1-citrusp$Processing_packaging) 
citrusp$feed_material = "citrusp"

# distillers grains
# Here we estimate distillers grains from corn biofuel industry and beer brewing
# First, beer production
distillers_grains = FAO_SUA[which(FAO_SUA$`Item Code` %in% c(51) & FAO_SUA$`Element Code` == 5510),] # choose item codes beer of barley 51 and production 5510
distillers_grains = distillers_grains[distillers_grains$Year %in% x, ] # choose the selected years
distillers_grains = spread(distillers_grains[,-c(7,11)], Year,Value)
distillers_grains$mean = rowMeans(distillers_grains[8:10])
distillers_grains = aggregate(data=distillers_grains, mean~ `Area Code`+ Area, FUN=sum)
distillers_grains$byprod_value = distillers_grains$mean*0.208 # 208kg of BSG from 1t of beer
# Then, add manually ddgs from corn biofuel for the major production countries
# World prod: 41100000 t
# USA 90% 37000000 t (https://link.springer.com/article/10.1007/s00253-020-10682-0)
# Brazil 2.2% 904200 t
# China 2.8% 1150800 t
# Canada 2% 822000 t
distillers_grains = rbind(distillers_grains, c(231, NA,0, 37000000))
distillers_grains[distillers_grains$`Area Code`== 231, 2] = "United States of America"
distillers_grains = rbind(distillers_grains, c(21, NA,0, 904200))
distillers_grains[distillers_grains$`Area Code`== 21, 2] = "Brazil"
distillers_grains = rbind(distillers_grains, c(41, NA,0, 1150800))
distillers_grains[distillers_grains$`Area Code`== 41, 2] = "China, mainland"
distillers_grains = rbind(distillers_grains, c(33, NA,0, 822000))
distillers_grains[distillers_grains$`Area Code`== 33, 2] = "Canada"
# Aggregate
distillers_grains = aggregate(data=distillers_grains,byprod_value~`Area Code`+ Area, na.rm=T, FUN=sum )
distillers_grains$feed_material = "distillers_grains"


# Merge together
crop_byprod = rbind(bran, mol[,c(1,2,11,12)])
crop_byprod = rbind(crop_byprod, pulp[,c(1,3,14,15)])
crop_byprod = rbind(crop_byprod, omeals[,c(1,3,6,7)])
crop_byprod = rbind(crop_byprod, citrusp[,c(3,1,6,7)])
crop_byprod = rbind(crop_byprod, distillers_grains)

# Choose only countries not regions
crop_byprod = crop_byprod[crop_byprod$`Area Code`<1000 ,] 

# Merge the different parts of China to China total (351)
sum_bran_China = sum(crop_byprod[crop_byprod$`Area Code` %in% c(41,96,214) & crop_byprod$feed_material == "bran", "byprod_value"],na.rm=T)
sum_mol_China = sum(crop_byprod[crop_byprod$`Area Code` %in% c(41,96,214) & crop_byprod$feed_material == "molasses", "byprod_value"],na.rm=T)
sum_pulp_China = sum(crop_byprod[crop_byprod$`Area Code` %in% c(41,96,214) & crop_byprod$feed_material == "pulp", "byprod_value"],na.rm=T)
sum_omeal_China = sum(crop_byprod[crop_byprod$`Area Code` %in% c(41,96,214) & crop_byprod$feed_material == "oilseed_meals", "byprod_value"],na.rm=T)
sum_citrusp_China = sum(crop_byprod[crop_byprod$`Area Code` %in% c(41,96,214) & crop_byprod$feed_material == "citrusp", "byprod_value"],na.rm=T)
sum_dg_China = sum(crop_byprod[crop_byprod$`Area Code` %in% c(41,96,214) & crop_byprod$feed_material == "distillers_grains", "byprod_value"],na.rm=T)

crop_byprod <- crop_byprod %>% 
  add_row(Area = "China", `Area Code` = 351, byprod_value = sum_bran_China, feed_material = "bran") %>%
  add_row(Area = "China", `Area Code` = 351, byprod_value = sum_mol_China, feed_material = "molasses") %>%
  add_row(Area = "China", `Area Code` = 351, byprod_value = sum_pulp_China, feed_material = "pulp") %>%
  add_row(Area = "China", `Area Code` = 351, byprod_value = sum_omeal_China, feed_material = "oilseed_meals") %>%
  add_row(Area = "China", `Area Code` = 351, byprod_value = sum_citrusp_China, feed_material = "citrusp") %>%
  add_row(Area = "China", `Area Code` = 351, byprod_value = sum_dg_China, feed_material = "distillers_grains") %>%
  filter(! `Area Code` %in% c(41,96,214)) # Remove the separate China provinces

# Convert to dry matter
dm = read.csv("data/feed_nutritional_tables_avg.csv", check.names = F)
crop_byprod = merge(x=crop_byprod, y=dm[,c(1,9)], by="feed_material", all.x=T)
crop_byprod$byprod_value_dm = crop_byprod$byprod_value*crop_byprod$dry_matter
crop_byprod = crop_byprod[,c(1,2,3,6)]
colnames(crop_byprod)[4]= "byprod_value"

return(crop_byprod)

}

#write.csv(crop_byprod, file="outputs/crop_byprod_results_2016_2018_updated.csv", row.names =F )



# 2a. Animal by-products with production data

anim_bp <- function(x) {

# import data on the animal production and the residue production rates
#FAO_animprod = read.csv("Data/Production_LivestockPrimary_E_All_Data_NOFLAG.csv", header=T, sep=",", check.names = F)
#save(FAO_animprod, file="data/FAO_animprod.Rdata")

  start.time <- Sys.time()

# load prod data (previously read from csv files and saved as Rdata)
load("data/FAO_animprod.Rdata")

names(FAO_animprod) <- sub("Y", "", names(FAO_animprod)) # Remove "Y" from the years
FAO_animprod$mean = rowSums(FAO_animprod[,which(colnames(FAO_animprod) %in% x)])/length(x)
FAO_animprod= FAO_animprod[,c(1:7,66)]

# Calculate separately the byproducts for pig and cattle meat(867,1035), poultry meat (1808) milk (1780) and eggs (1783)
# Multiply the production quantities with the dressing percentages to get the live weight 
# (FAOSTAT data for meat presented as dressed carcass weight, excluding offal and slaughter fats)

# 1. Cattle, pig and poultry meat
anim_bp_meat = FAO_animprod[FAO_animprod$`Element Code`%in% c(5510) & FAO_animprod$`Item Code` %in% c(867,1035, 1808),] 
# choose only production and yield/carcass weight and products meat (cattle, pig, poultry)
# Units: production t 
anim_bp_meat = spread(anim_bp_meat, Element, mean)
#Dressing percentages = Warm carcass weight / liveweight
dressing_perc = read.csv("data/DressingPerc.csv", sep=",", check.names = F) 
anim_bp_meat = merge(anim_bp_meat, dressing_perc[,c(1,3)], by="Item Code")
anim_bp_meat$live_weight = anim_bp_meat$Production/anim_bp_meat$dress_perc
# Multiply the live weight with the ratios of different rendering products
byprod = read.csv("data/livestock_byproducts.csv", check.names = F) # 
anim_bp_meat= merge(x= anim_bp_meat, y= byprod[,c(1,3,4)], by="Item Code")
anim_bp_meat$bp_amount = anim_bp_meat$live_weight* anim_bp_meat$ratio_rend_prod

# Remove aggregated regions
anim_bp_meat = anim_bp_meat[anim_bp_meat$`Area Code`<1000,]
# Remove different parts of China and leave only the aggregated on (country code 351)
anim_bp_meat = anim_bp_meat[!anim_bp_meat$`Area Code` %in% c(41,96,128,214),]

### Monte carlos to add uncertainty in the ratios of rendered products ###
## with a cv 0.1 and normal distribution
cv<-0.1

anim_bp_meat_sens<-matrix(NA, dim(anim_bp_meat)[1],500)
### estimating total feed use for the new production years
set.seed(3)
for (i in 1:500){
  anim_bp_meat_sens[,i]<-anim_bp_meat$live_weight*sapply(anim_bp_meat$ratio_rend_prod,function(x) round(rnorm(1,x,cv*x),2))
  
}

# Merge the average production median and 5% and 95% percentile
anim_bp_meat$bp_amount_median<-rowMedians(anim_bp_meat_sens,na.rm=TRUE)
anim_bp_meat$bp_amount_5th<-apply(anim_bp_meat_sens,MARGIN=1, function(x)quantile(x,0.05,na.rm=TRUE))
anim_bp_meat$bp_amount_95th<-apply(anim_bp_meat_sens,MARGIN=1, function(x)quantile(x,0.95,na.rm=TRUE))


# 2. Eggs
anim_bp_eggs = FAO_animprod[FAO_animprod$`Element Code`%in% c(5313) & FAO_animprod$`Item Code` %in% c(1783),]
# Units: 100 heads
anim_bp_eggs = spread(anim_bp_eggs[,-c(5,7)], Element, mean)
anim_bp_eggs$Laying_heads = anim_bp_eggs$Laying*1000
# Here we divide the numbers of laying hen with the average age at slaughtering,
# and multiply that with their average weight at the end of laying period and the % of them used as not food
gleam_poultry = read.csv("data/gleam_poultry_parameters.csv", check.names = F)
anim_bp_eggs = merge(anim_bp_eggs, gleam_poultry[,c(4,5,6,7)], by=c("Item Code"))
anim_bp_eggs = merge(anim_bp_eggs, byprod[,c(1,3,4)], by=c("Item Code"))
anim_bp_eggs$bp_amount = (anim_bp_eggs$Laying_heads/anim_bp_eggs$slaughter_age_y)*anim_bp_eggs$weight_tonnes*anim_bp_eggs$ratio_not_food*anim_bp_eggs$ratio_rend_prod

# Remove aggregated regions
anim_bp_eggs = anim_bp_eggs[anim_bp_eggs$`Area Code`<1000,]
# Remove different parts of China and leave only the aggregated on (country code 351)
anim_bp_eggs = anim_bp_eggs[!anim_bp_eggs$`Area Code` %in% c(41,96,128,214),]

### Monte carlos to add uncertainty in the ratios of rendered products ###
## with a cv 0.1 and normal distribution
cv<-0.1

anim_bp_eggs_sens<-matrix(NA, dim(anim_bp_eggs)[1],500)
### estimating total feed use for the new production years
set.seed(3)
for (i in 1:500){
  anim_bp_eggs_sens[,i]<-(anim_bp_eggs$Laying_heads/anim_bp_eggs$slaughter_age_y)*anim_bp_eggs$weight_tonnes*anim_bp_eggs$ratio_not_food*sapply(anim_bp_eggs$ratio_rend_prod,function(x) round(rnorm(1,x,cv*x),2))
  
}

# Merge with average production the median 5% and 95% percentile
anim_bp_eggs$bp_amount_median<-rowMedians(anim_bp_eggs_sens,na.rm=TRUE)
anim_bp_eggs$bp_amount_5th<-apply(anim_bp_eggs_sens,MARGIN=1, function(x)quantile(x,0.05,na.rm=TRUE))
anim_bp_eggs$bp_amount_95th<-apply(anim_bp_eggs_sens,MARGIN=1, function(x)quantile(x,0.95,na.rm=TRUE))


# 3. Dairy cows
# We don't need to include this since we are not considering livestock by-products from bovine origin

## Aggregate, merge and export data
anim_bp = rbind(cbind(anim_bp_meat[,c(2,3,10)], anim_bp_meat_sens), cbind(anim_bp_eggs[,c(2,3,10)], anim_bp_eggs_sens) )

# Convert to dry matter
dm = read.csv("data/feed_nutritional_tables_avg.csv", check.names = F)
colnames(dm)[1]="byprod"
anim_bp= merge(x=anim_bp[anim_bp$byprod %in% c("blood_meal_poultry","blood_meal_pig", "hydrolyzed_feather_meal", "meat_meal", "poultry_byproduct_meal", "poultry_oil"),], y=dm[,c(1,9)], by="byprod", all.x=T)
for(i in 4:503){
  anim_bp[,i]=anim_bp[,i]* anim_bp[,"dry_matter"]
}

end.time <- Sys.time()
time.taken <- end.time - start.time
show(time.taken)
return(anim_bp)


}

#write.csv(anim_bp, file="outputs/anim_bp_2016_2018.csv", row.names = F)

# 3. Fisheries by-products

fisheries_bp <- function(x) {
  start.time <- Sys.time()
  
  library(truncnorm)
  
# import data on the fish production
fishstat_aquaculture = read.csv("data/aquaculture_2000_2018.csv", header=T, sep=",", check.names = F)
aquacult = gather(fishstat_aquaculture, Year, Aquacult, 6:24) # from wide to long
colnames(aquacult)[1]="AreaName"

fishstat_capture= read.csv("data/capture_2000_2018.csv", header=T, sep=",", check.names = F)
capture = gather(fishstat_capture, Year,Capture, 6:24)
colnames(capture)[1]="AreaName"

# 1) Calculate first the average amounts

# Multiply production with average ratios of humancons vs. non-food use

human_cons = read.csv("data/HumanCons.csv", check.names = F)
human_cons = gather(human_cons, EconClass, human_cons_perc, 2:3)
countries = read.csv("data/Countries.csv", check.names = F)
capture = merge(capture, countries[,c(1,2,7,12,13,14)], by= "AreaName")
# Here some countries not included in the countrylist drop out

capture_non_food= merge(x=capture, y= human_cons, by=c("Year", "EconClass")) # other years than 1993-2017 will be dropped

# Correct for the species that there is information that bigger percentage goes from capture to non-food use 
# (from Shepherd and Jackson, 2013)
ind_forage = c("Gulf menhaden", "Atlantic menhaden", "Sandeels(=Sandlances) nei")
capture_non_food[which(capture_non_food$`Species (ASFIS species)` %in% ind_forage),"human_cons_perc"] <- 0
foodgrade_forage = c("Anchoveta(=Peruvian anchovy)", "Japanese anchovy", "Southern African anchovy", "Capelin", 
                     "	Blue whiting(=Poutassou)", "European sprat")
capture_non_food[which(capture_non_food$`Species (ASFIS species)` %in% foodgrade_forage),"human_cons_perc"] <- 0.1

# Correct also for the trash fish species for China that we know go all to non-food (from Cao et al. 2015)
cao_table_s2 = read.csv("data/Cao_et_al_table_S2.csv")
capture_non_food[which(capture_non_food$`Species (ASFIS species)` %in% cao_table_s2$Fish.species & 
                         capture_non_food$`Area Code`==351),"human_cons_perc"] <- 0

# Multiply the landings with the % going to human_cons/non-food
capture_non_food$non_food = capture_non_food$Capture*(1- capture_non_food$human_cons_perc)
capture_proc = capture_non_food
capture_proc$human_cons = capture_non_food$Capture *(capture_non_food$human_cons_perc)

# Multiply non-food use with the % of going to reduction and % of "trash fish"
reduction = read.csv("data/ReductionFromNonfood.csv", check.names = F)
reduction = gather(reduction, EconClass, reduction_perc, 2:3)
capture_non_food = merge(capture_non_food, reduction, by=c("EconClass", "Year"))

# Then assign all these amounts of non-food use going to reduction (and not used as trash fish)
ind_forage = c("Gulf menhaden", "Atlantic menhaden", "Sandeels(=Sandlances) nei")
capture_non_food[which(capture_non_food$`Species (ASFIS species)` %in% ind_forage),"reduction_perc"] <- 1
foodgrade_forage = c("Anchoveta(=Peruvian anchovy)", "Japanese anchovy", "Southern African anchovy", "Capelin", 
                     "	Blue whiting(=Poutassou)", "European sprat")
capture_non_food[which(capture_non_food$`Species (ASFIS species)` %in% foodgrade_forage),"reduction_perc"] <- 1

capture_non_food$reduction = capture_non_food$non_food*capture_non_food$reduction_perc
capture_non_food$trash_fish = capture_non_food$non_food* (1-capture_non_food$reduction_perc)


# Multiply amounts of capture fish going to human consumption with ratios of how much of those are processed and then how much of the 
# processing by-products are fit for reduction 
processing = read.csv("data/Processing.csv")
processing = gather(processing, EconClass, proc_perc, 2:3)
capture_proc = merge(capture_proc, processing, by=c("Year", "EconClass"))
capture_proc$processed = capture_proc$human_cons*capture_proc$proc_perc
capture_proc$bp_to_reduction =capture_proc$processed*0.415*0.98 *0.98 # 41.5% of fish are considered here as by-products (trimmings etc.) Stevens et al. 2018
#all other parts except blood (2%) can be used in reduction
# and 2% losses assumed at processing stage (from Cao et al. 2015)

# Multiply amounts of aquaculture fish (all go to human consumption) with ratios of how much of those are processed and 
# then how much of the processing by-products are fit for reduction 
aquacult = merge(aquacult, countries[,c(1,2,7,12,13,14)], by= "AreaName")
aquacult_proc = merge(aquacult, processing, by=c("EconClass", "Year"))
aquacult_proc$processed = aquacult_proc$Aquacult* aquacult_proc$proc_perc
aquacult_proc$bp_to_reduction = aquacult_proc$processed*0.415*0.98 *0.98 # 41.5% of fish are considered here as by-products (trimmings etc.) Stevens et al. 2018
# all other parts except blood (2%) can be used in reduction
# and 2% losses assumed at processing stage (from Cao et al. 2015)

# Aggregate the results for fmfo raw material for the selected years
capture_proc2 = aggregate(data= capture_proc[which(capture_proc$Year %in% x),], bp_to_reduction~MetaRegionName+Year, FUN=sum)
colnames(capture_proc2)[3]= "capture_proc_bp"
capture_proc2 = aggregate(data=capture_proc2, capture_proc_bp~MetaRegionName, FUN=mean)
aquacult_proc2 = aggregate(data= aquacult_proc[which(aquacult_proc$Year %in% x),], bp_to_reduction~MetaRegionName+ Year, FUN=sum)
colnames(aquacult_proc2)[3]= "aquacult_proc_bp"
aquacult_proc2 = aggregate(data=aquacult_proc2, aquacult_proc_bp~MetaRegionName, FUN=mean)
capture_forage_fish2 = aggregate(data=capture_non_food[which(capture_non_food$Year %in% x),], reduction~MetaRegionName+Year, FUN=sum)
colnames(capture_forage_fish2)[3]= "capture_forage_fish"
capture_forage_fish2 = aggregate(data=capture_forage_fish2, capture_forage_fish~MetaRegionName, FUN=mean)
capture_trash_fish2 = aggregate(data=capture_non_food[which(capture_non_food$Year %in% x),], trash_fish~MetaRegionName+ Year, FUN=sum)
colnames(capture_trash_fish2)[3]= "capture_trash_fish"
capture_trash_fish2 = aggregate(data=capture_trash_fish2, capture_trash_fish~MetaRegionName, FUN=mean)
fmfo_raw = merge(capture_proc2, aquacult_proc2, by="MetaRegionName")
fmfo_raw = merge(fmfo_raw, capture_forage_fish2, by="MetaRegionName")
fmfo_raw = merge(fmfo_raw, capture_trash_fish2, by="MetaRegionName") #unit:tonnes
fmfo_raw$MetaRegionName= as.character(fmfo_raw$MetaRegionName)
fmfo_raw[7,]= c("Total", colSums(fmfo_raw[,2:5]))
# for comparison IFFO (Jackson & Newton 2016) report: whole fish ~14mt, by-products (total with unused potential): 17.3mt
# Export trash fish data separately
capture_trash_fish3 = aggregate(data=capture_non_food[which(capture_non_food$Year %in% x),], trash_fish~`Area Code`+ AreaName+ Year, FUN=sum)
colnames(capture_trash_fish3)[4]= "capture_trash_fish"
capture_trash_fish3 = aggregate(data=capture_trash_fish3, capture_trash_fish~`Area Code`+ AreaName, FUN=mean)
#write.csv(capture_trash_fish3, file="outputs/capture_trash_fish.csv", row.names = F)


# 2) Add uncertainty to the used coefficients


cv<-0.1

##creating clusters ###
# Use the detectCores() function to find the number of cores in system
no_cores <- detectCores()
# Setup cluster
clust <- makeCluster(no_cores) #This line will take time
clusterExport(clust, "cv",envir=environment())
## define function for parallel processing ##
rand_sample<-function(x,y) {
  round(truncnorm::rtruncnorm(1, a=-Inf, b=1, x,y*x),2)}
environment(rand_sample) <- .GlobalEnv
####################################

capture_proc_agg = aggregate(data=capture_proc, bp_to_reduction~Year+`Area Code`+AreaName , FUN=sum)
capture_proc_agg = aggregate(data=capture_proc_agg[which(capture_proc_agg$Year %in% x),], bp_to_reduction~`Area Code`+AreaName , FUN=mean)
capture_proc_sens_matrix<-matrix(NA, dim(capture_proc_agg)[1],500)
### estimating total feed use for the new production years
set.seed(3)
for (i in 1:500){
 # message(i)
  # First estimating bps from capture fisheries
  capture_proc_sens = capture_proc
  # Multiply production with average ratios of humancons vs. non-food use
  # Add uncertainty to human cons %
  capture_proc_sens$human_cons_perc = parSapply(clust,capture_proc_sens$human_cons_perc,FUN=rand_sample,y=cv)
  # Correct for the species that there is information that bigger percentage goes from capture to non-food use 
  # (from Shepherd and Jackson, 2013)
  ind_forage = c("Gulf menhaden", "Atlantic menhaden", "Sandeels(=Sandlances) nei")
  capture_proc_sens[which(capture_proc_sens$`Species (ASFIS species)` %in% ind_forage),"human_cons_perc"] <- 0
  foodgrade_forage = c("Anchoveta(=Peruvian anchovy)", "Japanese anchovy", "Southern African anchovy", "Capelin", 
                       "	Blue whiting(=Poutassou)", "European sprat")
  capture_proc_sens[which(capture_proc_sens$`Species (ASFIS species)` %in% foodgrade_forage),"human_cons_perc"] <- 0.1
  
  # Correct also for the trash fish species for China that we know go all to non-food (from Cao et al. 2015)
  cao_table_s2 = read.csv("data/Cao_et_al_table_S2.csv")
  capture_proc_sens[which(capture_proc_sens$`Species (ASFIS species)` %in% cao_table_s2$Fish.species & 
                            capture_proc_sens$`Area Code`==351),"human_cons_perc"] <- 0
  # Multiply to get the human consumption and non-food numbers
  capture_proc_sens$non_food = capture_proc_sens$Capture*(1-capture_proc_sens$human_cons_perc)
  capture_proc_sens$human_cons = capture_proc_sens$Capture*capture_proc_sens$human_cons_perc
  # Add uncertainty to production %
  capture_proc_sens$proc_perc = parSapply(clust,capture_proc_sens$proc_perc,FUN=rand_sample,y=cv)
  capture_proc_sens$processed = capture_proc_sens$human_cons*capture_proc_sens$proc_perc
  # 41.5% of fish are considered here as by-products (trimmings etc.), add uncertainty here too
  capture_proc_sens$bp_to_reduction =capture_proc_sens$processed*0.98 *0.98*parSapply(clust,0.415,FUN=rand_sample,y=cv)
  #all other parts except blood (2%) can be used in reduction
  # and 2% losses assumed at processing stage (from Cao et al. 2015)
  
  capture_proc_sens$`Area Code`[capture_proc_sens$`Area Code`%in% c(96, 128)]<- "351" # Change China regions to China
  # Aggregate bps from different species so that there is 1 row for each country every year
  capture_proc_sens = aggregate(data=capture_proc_sens, bp_to_reduction~Year+`Area Code`+AreaName , FUN=sum)
  # Aggregate bps from to take the mean of the selected years
  capture_proc_sens = aggregate(data=capture_proc_sens[which(capture_proc_sens$Year %in% x),], bp_to_reduction~`Area Code`+AreaName , FUN=mean)
  
  capture_proc_sens_matrix[,i] = capture_proc_sens[,3]
}


aquacult_proc_agg = aggregate(data=aquacult_proc, bp_to_reduction~Year+`Area Code`+AreaName , FUN=sum)
aquacult_proc_agg = aggregate(data=aquacult_proc_agg[which(aquacult_proc_agg$Year %in% x),], bp_to_reduction~`Area Code`+AreaName , FUN=mean)
aquacult_proc_sens_matrix<-matrix(NA, dim(aquacult_proc_agg)[1],500)
### estimating total feed use for the new production years
set.seed(3)
for (i in 1:500){
 # message(i)
# Then for aquaculture
   aquacult_proc_sens = aquacult_proc
   # Add uncertainty to proc %
   aquacult_proc_sens$proc_perc = parSapply(clust,aquacult_proc_sens$proc_perc,FUN=rand_sample,y=cv)
   aquacult_proc_sens$processed = aquacult_proc_sens$Aquacult* aquacult_proc_sens$proc_perc
   # 41.5% of fish are considered here as by-products (trimmings etc.), add uncertainty
   aquacult_proc_sens$bp_to_reduction = aquacult_proc_sens$processed*0.98 *0.98*parSapply(clust,0.415,FUN=rand_sample,y=cv) 
   # all other parts except blood (2%) can be used in reduction
   # and 2% losses assumed at processing stage (from Cao et al. 2015)
   
   aquacult_proc_sens$`Area Code`[aquacult_proc_sens$`Area Code`%in% c(96, 128)]<- "351" # Change China regions to China
   # Aggregate bps from different species so that there is 1 row for each country
   aquacult_proc_sens = aggregate(data=aquacult_proc_sens, bp_to_reduction~Year+`Area Code`+AreaName , FUN=sum)
   # Aggregate bps from to take the mean of the selected years
   aquacult_proc_sens = aggregate(data=aquacult_proc_sens[which(aquacult_proc_sens$Year %in% x),], bp_to_reduction~`Area Code`+AreaName , FUN=mean)
   
   
   aquacult_proc_sens_matrix[,i] = aquacult_proc_sens[,3]

}

aquacult_bp = cbind(aquacult_proc_sens[,c(1,2)], aquacult_proc_sens_matrix)
capture_bp = cbind(capture_proc_sens[,c(1,2)], capture_proc_sens_matrix)

# Merge the bps from aquaculture and capture production
fish_bp = as.data.frame(unique(rbind(aquacult_bp[,c(1,2)], capture_bp[,c(1,2)])))
for(i in 3:502){
  for(j in 1:nrow(fish_bp)){
  fish_bp[j,i]= sum(aquacult_bp[which(aquacult_bp$`Area Code`==fish_bp[j,1]),i],capture_bp[which(capture_bp$`Area Code`==fish_bp[j,1]),i], na.rm=T)
}
}

# Export data about fm/fo production made from byproducts ACTUAL SITUATION and POTENTIAL 
# Yields for fm and fo from byproducts from Basto Silva et al. 2018
fish_bp_fm = fish_bp
for (i in 3:502){
  fish_bp_fm[,i] = fish_bp[,i]*0.2
}
fish_bp_fo = fish_bp
for (i in 3:502){
  fish_bp_fo[,i] = fish_bp[,i]*0.04
}

# Convert to dry matter
dm = read.csv("data/feed_nutritional_tables_avg.csv", check.names = F)
fish_bp_fm$feed_material = "fishmeal"
fish_bp_fm= merge(x=fish_bp_fm, y=dm[,c(1,9)], by="feed_material", all.x=T)
for(i in 4:503){
  fish_bp_fm[,i]=fish_bp_fm[,i]* fish_bp_fm[,"dry_matter"]
}
fish_bp_fo$feed_material = "fish_oil"
fish_bp_fo= merge(x=fish_bp_fo, y=dm[,c(1,9)], by="feed_material", all.x=T)
for(i in 4:503){
  fish_bp_fo[,i]=fish_bp_fo[,i]* fish_bp_fo[,"dry_matter"]
}

end.time <- Sys.time()
time.taken <- end.time - start.time
show(time.taken)
stopCluster(clust)
return(fish_bp_fm)

}

#write.csv(fish_bp_fm[,-c(1,504)], file="outputs/fish_bp_fm_2016_2018.csv", row.names = F)
#write.csv(fish_bp_fo[,-c(1,504)], file="outputs/fish_bp_fo_2016_2018.csv", row.names = F)
#write.csv(fish_bp, file="outputs/fish_bp_2016_2018.csv", row.names = F)

