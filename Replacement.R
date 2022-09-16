# Gather data together to analyze the replacement potential at regional level

# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(janitor)

years= c(2016,2017,2018)
# Read in feed use
source("fish_feed.R") # unit: tonnes
ffeed = fish_feed(years)
source("livestock_feed.R") # Unit: tonnes
lfeed = livestock_feed(years)
source("Byproducts.R")
# You can either run the functions for the amounts of byproducts produced, or
# read in the files saved running the byproducts.R code previously for 2016-2018

# Merge and correct with the FAO harmonization factors
# Import feed item codes and merge:
feed_itemcodes = read.csv("data/feed_itemcodes.csv", check.names = F)
lfeed = merge(x=lfeed, feed_itemcodes[,c(1,8)], by="feed_material", all.x=T)
hf = read.csv("outputs/hf_feed.csv")
lfeed = merge(x=lfeed, hf, by="subgroup2", all.x=T)
lfeed <- lfeed %>%
  mutate(hf = if_else(is.na(hf), 1, hf))
for (i in 6:505){
  lfeed[,i]=lfeed[,i]*lfeed[,"hf"]
}


ffeed = merge(x=ffeed, feed_itemcodes[,c(1,8)], by="feed_material", all.x=T)
ffeed = merge(x=ffeed, hf, by="subgroup2", all.x=T)
ffeed <- ffeed %>%
  mutate(hf = if_else(is.na(hf), 1, hf))
for (i in 5:504){
  ffeed[,i]=ffeed[,i]*ffeed[,"hf"]
}
ffeed$Item = "fish feed"
Total_feed = rbind(x=ffeed, y=lfeed)


Total_feed = merge(Total_feed, feed_itemcodes[,c(1,2,3,5,7)], by="feed_material")
regions = read.csv("data/FAOSTAT_countryRegionMapping.csv", check.names = F)
Total_feed = merge(Total_feed, regions[,c(4,2)], by="Area Code")

Total_feed[Total_feed$feed_material=="distillers_grains","subgroup2"]<-"distillers_grains"

Total_feed_materials <- Total_feed %>%
  group_by(subgroup2) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  select(-c(`Area Code`, hf, cf))

Total_feed_materials<-Total_feed_materials%>%
  mutate(feed_use_median =rowMedians(as.matrix(Total_feed_materials[,2:501]),na.rm=TRUE))%>%
  mutate(feed_use_5th =apply(as.matrix(Total_feed_materials[,c(2:501)]),MARGIN=1, function(x)quantile(x,0.05,na.rm=TRUE)))%>%
  mutate(feed_use_95th =apply(as.matrix(Total_feed_materials[,c(2:501)]),MARGIN=1, function(x)quantile(x,0.95,na.rm=TRUE)))
           

Total_feed_items <- Total_feed %>%
  group_by(Item, RegionName) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  select(-c(`Area Code`, hf, cf))


###### 1a) CEREALS #####

# Read in the feed use and production of the alternative feedstufff and calculate the availability
# = production - feed use at the global level, then rescale the "unused" feedstuff to regions 
# based on their share of global production

# Select only food crop cereals
Total_feed_cereals = Total_feed[Total_feed$subgroup2 == "cereals",]
for (i in 5:504){
  Total_feed_cereals[,i]=Total_feed_cereals[,i]*Total_feed_cereals$cf
}
Total_feed_cereals <- Total_feed_cereals %>%
  group_by(RegionName) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  select(-c(`Area Code`, hf, cf))
Total_feed_cereals$feed_use_median<-rowMedians(as.matrix(Total_feed_cereals[,c(2:501)]),na.rm=TRUE)
Total_feed_cereals$feed_use_5th<-apply(as.matrix(Total_feed_cereals[,c(2:501)]),MARGIN=1, function(x)quantile(x,0.05,na.rm=TRUE))
Total_feed_cereals$feed_use_95th<-apply(as.matrix(Total_feed_cereals[,c(2:501)]),MARGIN=1, function(x)quantile(x,0.95,na.rm=TRUE))

# Combine production of byprods
crop_res = crop_res(years)
crop_res$feed_material = "crop_res"
crop_byprods= crop_byprod(years)
anim_bps = anim_bp(years)
colnames(anim_bps)[1]="feed_material"

byprods = rbind(crop_res, anim_bps[,-504])
regions = read.csv("data/FAOSTAT_countryRegionMapping.csv", check.names = F)
byprods = merge(byprods, regions[,c(2,4)], by="Area Code")
crop_byprods = merge(crop_byprods, regions[,c(2,4)], by="Area Code")

# Crop residues availability
crop_res_glob = colSums(byprods[byprods$feed_material=="crop_res",c(3:502)])
# Their use as feed
crop_res_feed_use = Total_feed %>%
  group_by(subgroup2) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  filter(subgroup2 == "crop_residues")%>%
  select(-c(`Area Code`, hf, cf))
# The amounts not used as feed
crop_res$subgroup2 = "crop_residues"
crop_res_prod_reg = byprods %>%
  filter(feed_material == "crop_res")%>%
  group_by(RegionName, feed_material) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  select(-c(`Area Code`))
crop_res = crop_res_prod_reg[,c(1,2)]
for (i in 1:500){
  for(j in 1:nrow(crop_res)){
  crop_res[j,i+2]= (crop_res_glob[(i)]-crop_res_feed_use[,i+1])*(crop_res_prod_reg[j,i+2]/sum(crop_res_prod_reg[,i+2]))
  }
}

# bran by-products availability

bran_glob = sum(crop_byprods[crop_byprods$feed_material=="bran","byprod_value"])
# Their use as feed
bran_feed_use = Total_feed %>%
  group_by(subgroup2) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  filter(subgroup2 == "bran")%>%
  select(-c(`Area Code`, hf, cf))
# The amounts not used as feed
bran_prod_reg = crop_byprods %>%
  filter(feed_material == "bran")%>%
  group_by(RegionName, feed_material) %>%
  summarise(byprod_value = sum(byprod_value))
bran = bran_prod_reg[,c(1,2)]
for (i in 1:500){
  for(j in 1:nrow(bran)){
    bran[j,i+2]= (bran_glob-bran_feed_use[,i+1])*(bran_prod_reg[j,"byprod_value"]/sum(bran_prod_reg[,"byprod_value"]))
  }
}

# Sugarbeet/sugarcane molasses availability

molasses_glob = sum(crop_byprods[crop_byprods$feed_material=="molasses","byprod_value"])
# Their use as feed
molasses_feed_use = Total_feed %>%
  group_by(subgroup2) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  filter(subgroup2 == "molasses")%>%
  select(-c(`Area Code`, hf, cf))
# The amounts not used as feed
molasses_prod_reg = crop_byprods %>%
  filter(feed_material == "molasses")%>%
  group_by(RegionName, feed_material) %>%
  summarise(byprod_value = sum(byprod_value))
molasses = molasses_prod_reg[,c(1,2)]
for (i in 1:500){
  for(j in 1:nrow(molasses)){
    molasses[j,i+2]= (molasses_glob-molasses_feed_use[,i+1])*(molasses_prod_reg[j,"byprod_value"]/sum(molasses_prod_reg[,"byprod_value"]))
  }
}

# Sugarbeet pulp availability

pulp_glob = sum(crop_byprods[crop_byprods$feed_material=="pulp","byprod_value"])
# Their use as feed
pulp_feed_use = Total_feed %>%
  group_by(subgroup2) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  filter(subgroup2 == "pulp")%>%
  select(-c(`Area Code`, hf, cf))
# The amounts not used as feed
crop_byprods <- crop_byprods %>% # add manually the regions that don't produce sugar by-products
  rbind(list(NA, "pulp", NA, 0, "Caribbean"))%>%
  rbind(list(NA, "pulp", NA, 0, "Central America"))%>%
  rbind(list(NA, "pulp", NA, 0, "Eastern Africa"))%>%
  rbind(list(NA, "pulp", NA, 0, "Middle Africa"))%>%
  rbind(list(NA, "pulp", NA, 0, "Oceania"))%>%
  rbind(list(NA, "pulp", NA, 0, "South-Eastern Asia"))%>%
  rbind(list(NA, "pulp", NA, 0, "Southern Africa"))%>%
  rbind(list(NA, "pulp", NA, 0, "Western Africa"))
pulp_prod_reg = crop_byprods %>%
  filter(feed_material == "pulp")%>%
  group_by(RegionName, feed_material) %>%
  summarise(byprod_value = sum(byprod_value))
pulp = pulp_prod_reg[,c(1,2)]
for (i in 1:500){
  for(j in 1:nrow(pulp)){
    pulp[j,i+2]= (pulp_glob-pulp_feed_use[,i+1])*(pulp_prod_reg[j,"byprod_value"]/sum(pulp_prod_reg[,"byprod_value"]))
  }
}


# Citrus pulp availability

citrusp_glob = sum(crop_byprods[crop_byprods$feed_material=="citrusp","byprod_value"])
# Their use as feed
citrusp_feed_use<-matrix(NA, 1,500)
### estimating total feed use for the new production years
# Add uncertainty to the feed use estimate
set.seed(3)
for (i in 1:500){
  citrusp_feed_use[,i]<-citrusp_glob*sapply(citrusp_glob,function(x) round(rnorm(1,0.075,0.1*0.075),2))
}
# The amounts not used as feed
citrusp_prod_reg = crop_byprods %>%
  filter(feed_material == "citrusp")%>%
  group_by(RegionName, feed_material) %>%
  summarise(byprod_value = sum(byprod_value))
citrusp = citrusp_prod_reg[,c(1,2)]
for (i in 1:500){
  for(j in 1:nrow(citrusp)){
    citrusp[j,i+2]= (citrusp_glob-citrusp_feed_use[,i])*(as.numeric(citrusp_prod_reg[j,"byprod_value"])/sum(citrusp_prod_reg$byprod_value))
  }
}


# distilleries grains

distillers_grains_glob = sum(crop_byprods[crop_byprods$feed_material=="distillers_grains","byprod_value"])
# Their use as feed
distillers_grains_feed_use = Total_feed %>%
  group_by(subgroup2) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  filter(subgroup2 == "distillers_grains")%>%
  select(-c(`Area Code`, hf, cf))
# The amounts not used as feed
distillers_grains_prod_reg = crop_byprods %>%
  filter(feed_material == "distillers_grains")%>%
  group_by(RegionName, feed_material) %>%
  summarise(byprod_value = sum(byprod_value))
distillers_grains = distillers_grains_prod_reg[,c(1,2)]
for (i in 1:500){
  for(j in 1:nrow(distillers_grains)){
    distillers_grains[j,i+2]= (distillers_grains_glob-distillers_grains_feed_use[,i+1])*(distillers_grains_prod_reg[j,"byprod_value"]/sum(distillers_grains_prod_reg[,"byprod_value"]))
  }
}


# Cereals Feed use
# Select only food crop cereals
#Total_feed_cereals

### Select separately cereals used in livestock feed and in fish feed

lfeed_cereals = lfeed[lfeed$subgroup2 == "cereals",]

# Aggregate to regional level
lfeed_cereals = merge(lfeed_cereals, regions[,c(2,4)], by="Area Code")
lfeed_cereals = lfeed_cereals %>%
  group_by(RegionName, subgroup2, Item) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  select(-c(`Area Code`, hf))

# Add cereals used in fishfeed
ffeed_cereals = ffeed[ffeed$subgroup2 == "cereals",]
ffeed_cereals = merge(ffeed_cereals, regions[,c(2,4)], by="Area Code")
ffeed_cereals = ffeed_cereals %>%
  group_by(RegionName, subgroup2) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  select(-c(`Area Code`, hf))
ffeed_cereals$Item = "fish feed"

feed_cereals = rbind(lfeed_cereals, ffeed_cereals)

# Read in the replacement constraint table (nutritional and legislative constraints)
constr_cereals = read.csv("data/Repl_constraint_cereals2.csv")

# Calculate first without taking into account the crop residues that reduce productivity
constr_cereals[, c("repl_cropres_max", "repl_cropres_min")] <- 0

# Merge cereals feed use with replacement constraints for different feeds
rep_cer_feed = merge(feed_cereals, constr_cereals, by="Item")

# First calculate for each alternative feedstuff max and min feed use that can be replaced
# with that alternative feedstuff
# Run 500 monte carlo samples with the min and max from the replacement constraints and uniform distribution


#1 cerbp
rep_cer1 = rep_cer_feed
set.seed(3)
for(i in 4:503){
  rep_cer1[,i]= rep_cer1[,i]*with(rep_cer1, runif(nrow(rep_cer1), min=repl_cerbp_min, max=repl_cerbp_max ))
}
#2 mol
rep_cer2 = rep_cer_feed
set.seed(3)
for(i in 4:503){
  rep_cer2[,i]= rep_cer2[,i]*with(rep_cer2, runif(nrow(rep_cer2), min=repl_mol_min, max=repl_mol_max ))
}
#3 pulp
rep_cer3 = rep_cer_feed
set.seed(3)
for(i in 4:503){
  rep_cer3[,i]= rep_cer3[,i]*with(rep_cer3, runif(nrow(rep_cer3), min=repl_pulp_min, max=repl_pulp_max ))
}
#4 citrusp
rep_cer4 = rep_cer_feed
set.seed(3)
for(i in 4:503){
  rep_cer4[,i]= rep_cer4[,i]*with(rep_cer4, runif(nrow(rep_cer4), min=repl_citrusp_min, max=repl_citrusp_max ))
}
#5 distillers grains
rep_cer5 = rep_cer_feed
for(i in 4:503){
  rep_cer5[,i]= rep_cer5[,i]*with(rep_cer5, runif(nrow(rep_cer5), min=repl_dg_min, max=repl_dg_max ))
}
#6 crop res 
# not applied in the first cereal scenario

# Second, correct with the availability

alt_feed = c( "bran", "molasses", "pulp", "citrusp", "distillers_grains","crop_res")

# Loop through the data frames of different alt feedstuff replacement potential
# If the sum of replacement potentials of the different items (=animal production groups) is higher than the regional availability
# then normalise (= divide the regional availability with the sum of replacement potential of different items and multiply this
# with the replacement potential)
# if not then leave the replacement potential as it is
rep_cer_cor1= rep_cer1
rep_cer_cor2= rep_cer2
rep_cer_cor3= rep_cer3
rep_cer_cor4= rep_cer4
rep_cer_cor5= rep_cer5


for(k in 1:5){# loop through the number of replacement materials
  for(i in 4:503){ # loop through the 500 monte carlo simulation columns
    for(j in 1:nrow(get(paste("rep_cer", k, sep="")))){ # loop through all rows
      assign(paste0("rep_cer", k), `[<-` (get(paste0("rep_cer", k)), j, i, ifelse(sum(get(paste("rep_cer_cor", k, sep=""))[which(get(paste("rep_cer_cor", k, sep=""))$RegionName== get(paste("rep_cer_cor", k, sep=""))[j,"RegionName"]),i])>
                                                                                    get(alt_feed[k])[which(get(alt_feed[k])$RegionName==get(paste("rep_cer_cor", k, sep=""))[j,"RegionName"]),i-1],
                                                                                  (get(alt_feed[k])[which(get(alt_feed[k])$RegionName==get(paste("rep_cer_cor", k, sep=""))[j,"RegionName"]),i-1]/
                                                                                     sum(get(paste("rep_cer_cor", k, sep=""))[which(get(paste("rep_cer_cor", k, sep=""))$RegionName== get(paste("rep_cer_cor", k, sep=""))[j,"RegionName"]),i]))*
                                                                                    get(paste("rep_cer_cor", k, sep=""))[j,i],
                                                                                  get(paste("rep_cer_cor", k, sep=""))[j,i])))
    }
  }
}


# Third, correct so that the replacement potential won't exceed the feed use of an animal production group

# For this, first combine all the separate dataframes into one

rep_cer1$alt_feed = "cerbp"
rep_cer2$alt_feed = "mol"
rep_cer3$alt_feed = "pulp"
rep_cer4$alt_feed = "citrusp"
rep_cer5$alt_feed = "dg"

rep_cer = rbind(rep_cer1[,c(1:3, 516,4:503)], rep_cer2[,c(1:3, 516,4:503)])
rep_cer = rbind(rep_cer,rep_cer3[,c(1:3, 516,4:503)])
rep_cer = rbind(rep_cer,rep_cer4[,c(1:3, 516,4:503)])
rep_cer = rbind(rep_cer,rep_cer5[,c(1:3, 516,4:503)])

# If the combined replacement potential of the different replacement materials exceeds the cereal feed use of the animal group
# then normalise (= divide the cereal feed use of the animal production group with the sum of replacement potentials of different feedstuff and multiply this
# with the replacement potential of the specific feedstuff in question)

items = unique(rep_cer$Item)

for(l in 1:length(items)){
  for(i in 5:504){
    if (sum(rep_cer[rep_cer$Item==items[l], i])> sum(Total_feed[Total_feed$Item== items[l]& Total_feed$subgroup2== "cereals",i], na.rm=T)){
      rep_cer[rep_cer$Item == items[l], i]<- rep_cer[rep_cer$Item == items[l], i]*sum(Total_feed[Total_feed$Item== items[l]& Total_feed$subgroup2== "cereals",i])/sum(rep_cer[rep_cer$Item==items[l], i]) 
    }
  }
}
  
# Calculate the sum of total potential

rep_cer_tot <- rep_cer %>%
  group_by(RegionName, subgroup2) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))
rep_cer_tot$repl_median<-rowMedians(as.matrix(rep_cer_tot[,c(3:502)]),na.rm=TRUE)
rep_cer_tot$repl_5th<-apply(as.matrix(rep_cer_tot[,c(3:502)]),MARGIN=1, function(x)quantile(x,0.05,na.rm=TRUE))
rep_cer_tot$repl_95th<-apply(as.matrix(rep_cer_tot[,c(3:502)]),MARGIN=1, function(x)quantile(x,0.95,na.rm=TRUE))


## Calculate the increased food supply

macronutrients = read.csv("data/feed_nutritional_tables_avg.csv", check.names=F)
colnames(rep_cer_tot)[2]="feed_material"
rep_cer_tot = merge(rep_cer_tot, macronutrients[,c(1,6,7,8)], by="feed_material")
rep_cer_tot$increased_kcal_median = rep_cer_tot$repl_median*rep_cer_tot$kcal_t
rep_cer_tot$increased_kcal_5th = rep_cer_tot$repl_5th*rep_cer_tot$kcal_t
rep_cer_tot$increased_kcal_95th = rep_cer_tot$repl_95th*rep_cer_tot$kcal_t
rep_cer_tot$increased_prot_median = rep_cer_tot$repl_median*rep_cer_tot$prot_g_t
rep_cer_tot$increased_prot_5th = rep_cer_tot$repl_5th*rep_cer_tot$prot_g_t
rep_cer_tot$increased_prot_95th = rep_cer_tot$repl_95th*rep_cer_tot$prot_g_t
rep_cer_tot$increased_fat_median = rep_cer_tot$repl_median*rep_cer_tot$fat_g_t
rep_cer_tot$increased_fat_5th = rep_cer_tot$repl_5th*rep_cer_tot$fat_g_t
rep_cer_tot$increased_fat_95th = rep_cer_tot$repl_95th*rep_cer_tot$fat_g_t

# Check how much alt feed from the total rep
rep_alt_feed = rep_cer %>%
  group_by(alt_feed)%>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  mutate(rep_alt_feed_median =rowMedians(as.matrix(rep_alt_feed[,c(2:501)]),na.rm=TRUE))%>%
  select(!c(2:501))
rep_alt_feed$share_rep = rep_alt_feed$repl_median/sum(rowMedians(as.matrix(feed_cereals[,c(4:503)]),na.rm=TRUE))
sum(rep_alt_feed$share_rep) # ~10% can be replaced

##### 1b) CEREALS with crop residues ######

# Add to the replacement potential now also the crop residues for cattle meat and dairy 
# and take into account their reducing impact on productivity

# Combine the replacement potential with the alternative feeds from the first step
# and then add crop residues as much as possible

# Calculate the sum of feed cereals not replaced from the step 1)

rep_cer_b <- rep_cer %>%
  group_by(RegionName, subgroup2,Item) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))

# Order feed cereals data frame so that the rows are in same order and they can be subtracted

feed_cereals<- feed_cereals %>%
  group_by(RegionName, subgroup2,Item) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))

# How much of feed still not replaced, that can still be replaced with crop res

for(i in 4:503){
  rep_cer_b[,i]=feed_cereals[,i]-rep_cer_b[,i]
} 

# Read in the replacement constraint table (nutritional and legislative constraints)
constr_cereals = read.csv("data/Repl_constraint_cereals2.csv")
# Now also with crop residues replacement potential for cattle meat and dairy

# Merge cereals feed use with replacement constraints for different feeds
rep_cer_feed_b = merge(rep_cer_b, constr_cereals, by="Item")

#6 crop res
rep_cer6 = rep_cer_feed_b
set.seed(3)
for(i in 4:503){
  rep_cer6[,i]= rep_cer6[,i]*with(rep_cer6, runif(nrow(rep_cer6), min=repl_cropres_min, max=repl_cropres_max ))
}

# Second, correct with the availability
# only crop res

# Loop through the data frames of different alt feedstuff replacement potential
# If the sum of replacement potentials of the different items (=animal production groups) is higher than the regional availability
# then normalise (= divide the regional availability with the sum of replacement potential of different items and multiply this
# with the replacement potential)
# if not then leave the replacement potential as it is

rep_cer6$alt_feed = "cropres"
rep_cer_cor6 = rep_cer6

for(k in 6){# loop through the number of replacement materials (only crop res here)
  for(i in 4:503){ # loop through the 500 monte carlo simulation columns
    for(j in 1:nrow(get(paste("rep_cer", k, sep="")))){ # loop through all rows
      assign(paste0("rep_cer", k), `[<-` (get(paste0("rep_cer", k)), j, i, ifelse(sum(get(paste("rep_cer_cor", k, sep=""))[which(get(paste("rep_cer_cor", k, sep=""))$RegionName== get(paste("rep_cer_cor", k, sep=""))[j,"RegionName"]),i])>
                                                                                    get(alt_feed[k])[which(get(alt_feed[k])$RegionName==get(paste("rep_cer_cor", k, sep=""))[j,"RegionName"]),i-1],
                                                                                  (get(alt_feed[k])[which(get(alt_feed[k])$RegionName==get(paste("rep_cer_cor", k, sep=""))[j,"RegionName"]),i-1]/
                                                                                     sum(get(paste("rep_cer_cor", k, sep=""))[which(get(paste("rep_cer_cor", k, sep=""))$RegionName== get(paste("rep_cer_cor", k, sep=""))[j,"RegionName"]),i]))*
                                                                                    get(paste("rep_cer_cor", k, sep=""))[j,i],
                                                                                  get(paste("rep_cer_cor", k, sep=""))[j,i])))
    }
  }
}



# combine all the separate data frames into one

rep_cerb = rbind(rep_cer1[,c(1:3, 516,4:503)], rep_cer2[,c(1:3, 516,4:503)])
rep_cerb = rbind(rep_cerb,rep_cer3[,c(1:3, 516,4:503)])
rep_cerb = rbind(rep_cerb,rep_cer4[,c(1:3, 516,4:503)])
rep_cerb = rbind(rep_cerb,rep_cer5[,c(1:3, 516,4:503)])
rep_cerb = rbind(rep_cerb,rep_cer6[,c(1:3, 516,4:503)])

#check that the combined feed use of alt feeds isn't higher than the original feed use
for(l in 1:length(items)){
print(sum(rep_cerb[rep_cerb$Item==items[l], 8])> sum(Total_feed[Total_feed$Item== items[l]& Total_feed$subgroup2== "cereals",8], na.rm=T))
}
# Calculate the sum of total potential

rep_cer_tot2 <- rep_cerb %>%
  group_by(RegionName, subgroup2) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))
rep_cer_tot2$repl_median<-rowMedians(as.matrix(rep_cer_tot2[,c(3:502)]),na.rm=TRUE)
rep_cer_tot2$repl_5th<-apply(as.matrix(rep_cer_tot2[,c(3:502)]),MARGIN=1, function(x)quantile(x,0.05,na.rm=TRUE))
rep_cer_tot2$repl_95th<-apply(as.matrix(rep_cer_tot2[,c(3:502)]),MARGIN=1, function(x)quantile(x,0.95,na.rm=TRUE))


# Calculate the reduced productivity for the amounts of cattle production for which the replacement with crop residues would occur

# Calculate the share of replacement with crop residues from the total cereal feed use for cattle meat and dairy
cattle_meat_cropres = matrix(NA, 500,2)
for(i in 5:504){
  cattle_meat_cropres[i-4,1] = sum(rep_cer6[which(rep_cer6$Item == "Meat, cattle"),i])/
    sum(feed_cereals[which(feed_cereals$Item == "Meat, cattle"),i-1])
}

for(i in 6:505){
  cattle_meat_cropres[i-5,2] = sum(feed_cereals[which(feed_cereals$Item == "Meat, cattle"),i-2])/
    sum(lfeed[which(lfeed$Item=="Meat, cattle"),i])
}
cattle_meat_cropres =as.data.frame(cattle_meat_cropres)

cattle_meat_cropres$crop_res_of_tot_feed = cattle_meat_cropres$V1*cattle_meat_cropres$V2 

cattle_meat_cropres_share=mean(cattle_meat_cropres$crop_res_of_tot_feed,na.rm=T) # This is the % of total cattle meat feed replaced with crop res
cattle_meat_cropres_share_95th =as.numeric(quantile(cattle_meat_cropres$crop_res_of_tot_feed, probs = c(.95)))
cattle_meat_cropres_share_5th =as.numeric(quantile(cattle_meat_cropres$crop_res_of_tot_feed, probs = c(.05)))


cattle_dairy_cropres = matrix(NA, 500,2)
for(i in 5:504){
  cattle_dairy_cropres[i-4,1] = sum(rep_cer6[which(rep_cer6$Item == "Milk, whole fresh cow"),i])/
    sum(feed_cereals[which(feed_cereals$Item == "Milk, whole fresh cow"),i-1])
}
for(i in 6:505){
  cattle_dairy_cropres[i-5,2] = sum(feed_cereals[which(feed_cereals$Item == "Milk, whole fresh cow"),i-2])/
    sum(lfeed[which(lfeed$Item=="Milk, whole fresh cow"),i])
}
cattle_dairy_cropres =as.data.frame(cattle_dairy_cropres)

cattle_dairy_cropres$crop_res_of_tot_feed = cattle_dairy_cropres$V1*cattle_dairy_cropres$V2 

dairy_cropres_share= mean(cattle_dairy_cropres$crop_res_of_tot_feed,na.rm=T)
dairy_cropres_share_95th =quantile(cattle_dairy_cropres$crop_res_of_tot_feed, probs = c(.95))
dairy_cropres_share_5th =quantile(cattle_dairy_cropres$crop_res_of_tot_feed, probs = c(.05))

# For these % of the cattle meat and dairy production there would be 40-80% production decrease

# Read in data on different livestock production quantities 
# import data on the crop production and the residue production rates
#FAO_animprod = read.csv("Production_LivestockPrimary_E_All_Data_NOFLAG.csv", header=T, sep=",", check.names = F)
#save(FAO_animprod, file="FAO_animprod.Rdata")

# load prod data (previously read from csv files and saved as Rdata)
load("data/FAO_animprod.Rdata")

FAO_animprod = gather(FAO_animprod, Year, Value, 8:65) # from wide to long
FAO_animprod[,"Year"] <- sub("Y", "", FAO_animprod[,"Year"]) # Remove "Y" from years
# Here the years 2016-2018 are selected
FAO_animprod = FAO_animprod[which(FAO_animprod$`Element Code`=="5510" & FAO_animprod$Year %in% c(2016,2017,2018)),] # choose only production and years selected
FAO_animprod = FAO_animprod[!is.na(FAO_animprod$Value),] # Remove rows with NA
FAO_animprod= spread(FAO_animprod, Year, Value)
FAO_animprod$mean = rowSums(FAO_animprod[,c(8,9,10)])/3
countries = read.csv("data/countries.csv", check.names = F)
FAO_animprod = merge(x=FAO_animprod, y=countries[,c(2,12,13,14,15)], by="Area Code")

FAO_animprod = aggregate(data=FAO_animprod, mean~ Item+ RegionName, FUN=sum)
FAO_animprod = FAO_animprod[FAO_animprod$Item %in% c("Milk, whole fresh cow", "Meat, cattle"),]

# Calculate for the share of total feed how much crop res is replacing cereals
# and assume that for this share of the cattle meat and dairy production there would be 40-80% production decrease
FAO_animprod$redu_prod_min = ifelse(FAO_animprod$Item == "Milk, whole fresh cow", dairy_cropres_share_5th*FAO_animprod$mean*0.40,cattle_meat_cropres_share_5th*FAO_animprod$mean*0.40)
FAO_animprod$redu_prod_max = ifelse(FAO_animprod$Item == "Milk, whole fresh cow", dairy_cropres_share_95th*FAO_animprod$mean*0.80,cattle_meat_cropres_share_95th*FAO_animprod$mean*0.80)

# Convert to macronutrients
FAO_animprod$feed_material = "cereals"
FAO_animprod = merge(FAO_animprod, macronutrients[,c(1,6,7,8)], by="feed_material")
FAO_animprod$reduced_kcal_min = FAO_animprod$redu_prod_min*FAO_animprod$kcal_t
FAO_animprod$reduced_kcal_max = FAO_animprod$redu_prod_max+FAO_animprod$redu_prod_max*FAO_animprod$kcal_t
FAO_animprod$reduced_prot_min = FAO_animprod$redu_prod_min*FAO_animprod$prot_g_t
FAO_animprod$reduced_prot_max = FAO_animprod$redu_prod_max*FAO_animprod$prot_g_t
FAO_animprod$reduced_fat_min = FAO_animprod$redu_prod_min*FAO_animprod$fat_g_t
FAO_animprod$reduced_fat_max = FAO_animprod$redu_prod_max*FAO_animprod$fat_g_t

# sum of cattle meat and dairy
Redu_prod <- FAO_animprod %>%
  group_by(RegionName)%>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  select(-c(kcal_t, prot_g_t, fat_g_t))


#sum
FAO_animprod <- FAO_animprod %>%
  adorn_totals("row")


## Calculate the increased food supply without taking into account the reduction in productivity

macronutrients = read.csv("data/feed_nutritional_tables_avg.csv", check.names=F)
colnames(rep_cer_tot2)[2]="feed_material"
rep_cer_tot2 = merge(rep_cer_tot2, macronutrients[,c(1,6,7,8)], by="feed_material")
rep_cer_tot2$increased_kcal_median = rep_cer_tot2$repl_median*rep_cer_tot2$kcal_t
rep_cer_tot2$increased_kcal_5th = rep_cer_tot2$repl_5th*rep_cer_tot2$kcal_t
rep_cer_tot2$increased_kcal_95th = rep_cer_tot2$repl_95th*rep_cer_tot2$kcal_t
rep_cer_tot2$increased_prot_median = rep_cer_tot2$repl_median*rep_cer_tot2$prot_g_t
rep_cer_tot2$increased_prot_5th = rep_cer_tot2$repl_5th*rep_cer_tot2$prot_g_t
rep_cer_tot2$increased_prot_95th = rep_cer_tot2$repl_95th*rep_cer_tot2$prot_g_t
rep_cer_tot2$increased_fat_median = rep_cer_tot2$repl_median*rep_cer_tot2$fat_g_t
rep_cer_tot2$increased_fat_5th = rep_cer_tot2$repl_5th*rep_cer_tot2$fat_g_t
rep_cer_tot2$increased_fat_95th = rep_cer_tot2$repl_95th*rep_cer_tot2$fat_g_t

rep_alt_feedb = rep_cerb %>%
  group_by(alt_feed)%>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))
rep_alt_feedb <- rep_alt_feedb%>%
  mutate(rep_alt_feed_median =rowMedians(as.matrix(rep_alt_feedb[,c(2:501)]),na.rm=TRUE))%>%
  select(!c(2:501))
rep_alt_feedb$share_rep = rep_alt_feedb$rep_alt_feed_median/sum(rowMedians(as.matrix(feed_cereals[,c(4:503)]),na.rm=TRUE))
sum(rep_alt_feedb$share_rep) # ~26% can be replaced
                       

### 2) FISHMEAL #####

###### 1) Calculate the fm use in aquaculture and livestock feed

fish_feed_fm= ffeed[ffeed$feed_material %in% c("fishmeal"),]
# aggregate the total use of different ingredients per country and region
fish_feed_fm = merge(fish_feed_fm, regions[,c(4,2)], by="Area Code")
ff_fm <- fish_feed_fm %>%
  group_by(feed_material, RegionName) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  select(-c(hf))
ff_fm$use <- "use in fish feed"
ff_fm$Item <- "fish feed"

# Aggregate the fm use in livestock feed per region
an_fm= lfeed[lfeed$feed_material %in% c("fishmeal"),]
an_fm = merge(an_fm, regions[,c(4,2)], by="Area Code")
an_fm <- an_fm %>%
  group_by(feed_material, Item, RegionName) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  select(-c(hf))
an_fm$use <- "use in livestock feed"

# Combine
fm_use = rbind (an_fm, ff_fm)
# Reduce from the total fishmeal use the amounts of fishmeal made from by-products
# FAO SOFIA report (2020): by-products are used to produce up to 25-35 percent (average 30) of the total volume of fishmeal and fish oil
fm_use[,c(5:504)] <- fm_use[,c(5:504)]*0.70
fm_use_agg <- fm_use %>%
  group_by(feed_material,RegionName) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))
fm_use_agg$feed_use_median<-rowMedians(as.matrix(fm_use_agg[,c(4:503)]),na.rm=TRUE)
fm_use_agg$feed_use_5th<-apply(as.matrix(fm_use_agg[,c(4:503)]),MARGIN=1, function(x)quantile(x,0.05,na.rm=TRUE))
fm_use_agg$feed_use_95th<-apply(as.matrix(fm_use_agg[,c(4:503)]),MARGIN=1, function(x)quantile(x,0.95,na.rm=TRUE))

fm_use_items = fm_use

# Availability of replacement materials

# 1) Anim bps
# Calculate first how much animal bps are available and reduce the amount currently used as feed

# Read in the potential replacements for fm
# Separate different animal byproducts to their own groups

# blood meal poultry
blood_meal_poultry_glob = colSums(byprods[byprods$feed_material == "blood_meal_poultry",c(3:502)], na.rm=T)
# Their use as feed
blood_meal_poultry_feed_use = Total_feed %>%
  filter(feed_material == "blood_meal")%>%
  group_by(feed_material) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  select(-c(`Area Code`, hf, cf))
# Assign 50% of the blood meal feed use to poultry and 50% to pig
blood_meal_poultry_feed_use <- cbind(blood_meal_poultry_feed_use[,1], blood_meal_poultry_feed_use[,2:501]*0.5)
# The amounts not used as feed
blood_meal_poultry_prod_reg = byprods %>%
  filter(feed_material =="blood_meal_poultry")%>%
  group_by(RegionName, feed_material) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  select(-c(`Area Code`))
blood_meal_poultry = blood_meal_poultry_prod_reg[,c(1,2)]
for (i in 1:500){
  for(j in 1:nrow(blood_meal_poultry)){
    blood_meal_poultry[j,i+2]= (blood_meal_poultry_glob[(i)]-blood_meal_poultry_feed_use[,i+1])*(blood_meal_poultry_prod_reg[j,i+2]/sum(blood_meal_poultry_prod_reg[,i+2]))
  }
}

# blood meal pig
blood_meal_pig_glob = colSums(byprods[byprods$feed_material == "blood_meal_pig",c(3:502)], na.rm=T)
# Their use as feed
blood_meal_pig_feed_use = Total_feed %>%
  filter(feed_material == "blood_meal")%>%
  group_by(feed_material) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  select(-c(`Area Code`, hf, cf))
# Assign 50% of the blood meal feed use to poultry and 50% to pig
blood_meal_pig_feed_use <- cbind(blood_meal_pig_feed_use[,1], blood_meal_pig_feed_use[,2:501]*0.5)
# The amounts not used as feed
blood_meal_pig_prod_reg = byprods %>%
  filter(feed_material =="blood_meal_pig")%>%
  group_by(RegionName, feed_material) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  select(-c(`Area Code`))
blood_meal_pig = blood_meal_pig_prod_reg[,c(1,2)]
for (i in 1:500){
  for(j in 1:nrow(blood_meal_pig)){
    blood_meal_pig[j,i+2]= (blood_meal_pig_glob[(i)]-blood_meal_pig_feed_use[,i+1])*(blood_meal_pig_prod_reg[j,i+2]/sum(blood_meal_pig_prod_reg[,i+2]))
  }
}

# hydrolyzed_feather_meal
hydrolyzed_feather_meal_glob = colSums(byprods[byprods$feed_material == "hydrolyzed_feather_meal",c(3:502)], na.rm=T)
# Their use as feed
hydrolyzed_feather_meal_feed_use = Total_feed %>%
  filter(feed_material == "hydrolyzed_feather_meal")%>%
  group_by(feed_material) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  select(-c(`Area Code`, hf, cf))
# The amounts not used as feed
hydrolyzed_feather_meal_prod_reg = byprods %>%
  filter(feed_material =="hydrolyzed_feather_meal")%>%
  group_by(RegionName, feed_material) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  select(-c(`Area Code`))
hydrolyzed_feather_meal = hydrolyzed_feather_meal_prod_reg[,c(1,2)]
for (i in 1:500){
  for(j in 1:nrow(hydrolyzed_feather_meal)){
    hydrolyzed_feather_meal[j,i+2]= (hydrolyzed_feather_meal_glob[(i)]-hydrolyzed_feather_meal_feed_use[,i+1])*(hydrolyzed_feather_meal_prod_reg[j,i+2]/sum(hydrolyzed_feather_meal_prod_reg[,i+2]))
  }
}

# meat_meal
meat_meal_glob = colSums(byprods[byprods$feed_material == "meat_meal",c(3:502)], na.rm=T)
# Their use as feed
meat_meal_feed_use = Total_feed %>%
  filter(feed_material == "meat_and_bone_meal")%>%
  group_by(feed_material) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  select(-c(`Area Code`, hf, cf))
# The amounts not used as feed
meat_meal_prod_reg = byprods %>%
  filter(feed_material =="meat_meal")%>%
  group_by(RegionName, feed_material) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  select(-c(`Area Code`))
meat_meal = meat_meal_prod_reg[,c(1,2)]
for (i in 1:500){
  for(j in 1:nrow(meat_meal)){
    meat_meal[j,i+2]= (meat_meal_glob[(i)]-meat_meal_feed_use[,i+1])*(meat_meal_prod_reg[j,i+2]/sum(meat_meal_prod_reg[,i+2]))
  }
}

# poultry_byproduct_meal
poultry_byproduct_meal_glob = colSums(byprods[byprods$feed_material == "poultry_byproduct_meal",c(3:502)], na.rm=T)
# Their use as feed
poultry_byproduct_meal_feed_use = Total_feed %>%
  filter(feed_material == "poultry_byproduct_meal")%>%
  group_by(feed_material) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  select(-c(`Area Code`, hf, cf))
# The amounts not used as feed
poultry_byproduct_meal_prod_reg = byprods %>%
  filter(feed_material =="poultry_byproduct_meal")%>%
  group_by(RegionName, feed_material) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  select(-c(`Area Code`))
poultry_byproduct_meal = poultry_byproduct_meal_prod_reg[,c(1,2)]
for (i in 1:500){
  for(j in 1:nrow(poultry_byproduct_meal)){
    poultry_byproduct_meal[j,i+2]= (poultry_byproduct_meal_glob[(i)]-poultry_byproduct_meal_feed_use[,i+1])*(poultry_byproduct_meal_prod_reg[j,i+2]/sum(poultry_byproduct_meal_prod_reg[,i+2]))
  }
}


# 2) Oilseed meals
# Calculate first the availability and then reduce the amounts currently used as livestock and fish feed

omeals_glob = sum(crop_byprods[crop_byprods$feed_material=="oilseed_meals","byprod_value"], na.rm=T)
# Their use as feed
omeals_feed_use = Total_feed %>%
  group_by(subgroup2) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  filter(subgroup2 == "oilseed_meals")%>%
  select(-c(`Area Code`, hf, cf))
# The amounts not used as feed
omeals_prod_reg = crop_byprods %>%
  filter(feed_material == "oilseed_meals")%>%
  group_by(RegionName, feed_material) %>%
  summarise(byprod_value = sum(byprod_value))
omeals = omeals_prod_reg[,c(1,2)]
for (i in 1:500){
  for(j in 1:nrow(omeals)){
    omeals[j,i+2]= (omeals_glob-omeals_feed_use[,i+1])*(omeals_prod_reg[j,"byprod_value"]/sum(omeals_prod_reg[,"byprod_value"], na.rm=T))
  }
}
omeals[omeals<0]<-0 # convert negative availability to 0
omeals[is.na(omeals)]<-0 #NAs to 0

# 3) fishmeal from unused fisheries bps
#bp_fm = fisheries_bp(years) 
bp_fm_prod = read.csv("outputs/fish_bp_fm_2016_2018.csv", check.names = F) #To save time read in the previously saved output
regions = read.csv("data/FAOSTAT_countryRegionMapping.csv", check.names = F) # merge with region names
bp_fm_prod = merge(bp_fm_prod, regions[,c(2,4)], by="Area Code")
bp_fm_glob = colSums(bp_fm_prod[,c(3:502)], na.rm=T)
# Their use as feed
# This is from SOFIA report (FAO, 2018), 25-35% of fishmeal produced (~0.3*5.5 mmt= 1650000t) in 2018 made from by-products
fishbp_fm_feed_use<-matrix(NA,1,500)
for (i in 1:500){
  fishbp_fm_feed_use[,i]<-5500000*sapply(5500000,function(x) round(runif(1,min=0.25,max=0.35),2))
}
fishbp_fm_feed_use = cbind.data.frame(cat="fish_bp", fishbp_fm_feed_use)
# The amounts not used as feed
bp_fm_prod_reg = bp_fm_prod %>%
  group_by(RegionName) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  select(-c(`Area Code`))
bp_fm = bp_fm_prod_reg[,c(1,2)]
for (i in 1:500){
  for(j in 1:nrow(bp_fm)){
    bp_fm[j,i+2]= (bp_fm_glob[(i)]-fishbp_fm_feed_use[1,i+1])*(bp_fm_prod_reg[j,i+1]/sum(bp_fm_prod_reg[,i+1]))
  }
}


## Replacement

# Read in the replacement constraint table (nutritional and legislative constraints)
constr_fm = read.csv("data/Repl_constraint_fm2.csv")

# Merge fm feed use with replacement constraints for different feeds
rep_fm_feed = merge(fm_use_items, constr_fm, by="Item")

# First calculate for each alternative feedstuff max and min feed use that can be replaced
# with that alternative feedstuff
# Run 500 monte carlo samples with the min and max from the replacement constraints and uniform distribution

#1 blood meal pig
rep_fm1 = rep_fm_feed[,-4]
set.seed(3)
for(i in 4:503){
  rep_fm1[,i]= rep_fm1[,i]*with(rep_fm1, runif(nrow(rep_fm1), min=repl_blood_meal_pig_min, max=repl_blood_meal_pig_max ))
}
#2 blood meal poultry
rep_fm2 = rep_fm_feed[,-4]
set.seed(3)
for(i in 4:503){
  rep_fm2[,i]= rep_fm2[,i]*with(rep_fm2, runif(nrow(rep_fm2), min=repl_blood_meal_poultry_min, max=repl_blood_meal_poultry_max ))
}
#3 feather meal
rep_fm3 = rep_fm_feed[,-4]
set.seed(3)
for(i in 4:503){
  rep_fm3[,i]= rep_fm3[,i]*with(rep_fm3, runif(nrow(rep_fm3), min=repl_feath_min, max=repl_feath_max ))
}
#4 meat meal
rep_fm4 = rep_fm_feed[,-4]
set.seed(3)
for(i in 4:503){
  rep_fm4[,i]= rep_fm4[,i]*with(rep_fm4, runif(nrow(rep_fm4), min=repl_meatmeal_min, max=repl_meatmeal_max ))
}
#5 poultry bp
rep_fm5 = rep_fm_feed[,-4]
set.seed(3)
for(i in 4:503){
  rep_fm5[,i]= rep_fm5[,i]*with(rep_fm5, runif(nrow(rep_fm5), min=repl_poultrybp_min, max=repl_poultrybp_max ))
}
#6 fm from byproducts
rep_fm6 = rep_fm_feed[,-4]
for(i in 4:503){
  rep_fm6[,i]= rep_fm6[,i]*with(rep_fm6, runif(nrow(rep_fm6), min=repl_fmbp_min, max=repl_fmbp_max ))
}
#7 oilseed meals
rep_fm7 = rep_fm_feed[,-4]
for(i in 4:503){
  rep_fm7[,i]= rep_fm6[,i]*with(rep_fm7, runif(nrow(rep_fm7), min=repl_omeal_min, max=repl_omeal_max ))
}


# Second, correct with the availability

alt_feed = c("blood_meal_pig", "blood_meal_poultry", "hydrolyzed_feather_meal", "meat_meal", "poultry_byproduct_meal", "bp_fm", "omeals")

# Loop through the data frames of different alt feedstuff replacement potential
# If the sum of replacement potentials of the different items (=animal production groups) is higher than the regional availability
# then normalise (= divide the regional availability with the sum of replacement potential of different items and multiply this
# with the replacement potential)
# if not then leave the replacement potential as it is
rep_fm_cor1 = rep_fm1
rep_fm_cor2 = rep_fm2
rep_fm_cor3 = rep_fm3
rep_fm_cor4 = rep_fm4
rep_fm_cor5 = rep_fm5
rep_fm_cor6 = rep_fm6
rep_fm_cor7 = rep_fm7

for(k in 1:7){# loop through the number of replacement materials
  for(i in 4:503){ # loop through the 500 monte carlo simulation columns
    for(j in 1:nrow(get(paste("rep_fm", k, sep="")))){ # loop through all rows
      assign(paste0("rep_fm", k), `[<-` (get(paste0("rep_fm", k)), j, i, ifelse(sum(get(paste("rep_fm_cor", k, sep=""))[which(get(paste("rep_fm_cor", k, sep=""))$RegionName== get(paste("rep_fm_cor", k, sep=""))[j,"RegionName"]),i])>
                                                                                    get(alt_feed[k])[which(get(alt_feed[k])$RegionName==get(paste("rep_fm_cor", k, sep=""))[j,"RegionName"]),i-1],
                                                                                  (get(alt_feed[k])[which(get(alt_feed[k])$RegionName==get(paste("rep_fm_cor", k, sep=""))[j,"RegionName"]),i-1]/
                                                                                     sum(get(paste("rep_fm_cor", k, sep=""))[which(get(paste("rep_fm_cor", k, sep=""))$RegionName== get(paste("rep_fm_cor", k, sep=""))[j,"RegionName"]),i]))*
                                                                                    get(paste("rep_fm_cor", k, sep=""))[j,i],
                                                                                  get(paste("rep_fm_cor", k, sep=""))[j,i])))
    }
  }
}

# Third, correct so that the replacement potential won't exceed the feed use of an animal production group

# For this, first combine all the separate dataframes into one
rep_fm1$alt_feed = "blood_meal_pig"
rep_fm2$alt_feed = "blood_meal_poultry"
rep_fm3$alt_feed = "hydrolyzed_feather_meal"
rep_fm4$alt_feed = "meat_meal"
rep_fm5$alt_feed = "poultry_byproduct_meal"
rep_fm6$alt_feed = "bp_fm"
rep_fm7$alt_feed = "omeals"

rep_fm = rbind(rep_fm1[,c(1:3, 519,4:503)], rep_fm2[,c(1:3, 519,4:503)])
rep_fm = rbind(rep_fm,rep_fm3[,c(1:3, 519,4:503)])
rep_fm = rbind(rep_fm,rep_fm4[,c(1:3, 519,4:503)])
rep_fm = rbind(rep_fm,rep_fm5[,c(1:3, 519,4:503)])
rep_fm = rbind(rep_fm,rep_fm6[,c(1:3, 519,4:503)])
rep_fm = rbind(rep_fm,rep_fm7[,c(1:3, 519,4:503)])

# If the combined replacement potential of the different replacement materials exceeds the fm feed use of the animal group
# then normalise (= divide the fm feed use of the animal production group with the sum of replacement potentials of different feedstuff 
# and multiply this with the replacement potential of the specific feedstuff in question)

items = unique(rep_fm$Item)
rep_fm[is.na(rep_fm)]<-0

for(l in 1:4){
for(i in 5:504){
  if (sum(rep_fm[rep_fm$Item==items[l], i])> sum(fm_use_items[fm_use_items$Item== items[l], i])){
  rep_fm[rep_fm$Item == items[l], i]<- rep_fm[rep_fm$Item == items[l], i]*sum(fm_use_items[fm_use_items$Item== items[l], i])/sum(rep_fm[rep_fm$Item==items[l], i]) 
}
}
}

# Calculate the sum of total potential

rep_fm_tot <- rep_fm %>%
  group_by(RegionName, feed_material) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))
rep_fm_tot$repl_median<-rowMedians(as.matrix(rep_fm_tot[,c(3:502)]),na.rm=TRUE)
rep_fm_tot$repl_5th<-apply(as.matrix(rep_fm_tot[,c(3:502)]),MARGIN=1, function(x)quantile(x,0.05,na.rm=TRUE))
rep_fm_tot$repl_95th<-apply(as.matrix(rep_fm_tot[,c(3:502)]),MARGIN=1, function(x)quantile(x,0.95,na.rm=TRUE))


## Calculate the increased food supply
# Here convert the fishmeal saved to whole fish

rep_fm_tot<- rep_fm_tot %>%
  mutate(repl_median_as_whole_fish= repl_median/0.2) %>% #convert to whole fish
  mutate(repl_5th_as_whole_fish= repl_5th/0.2) %>% #convert to whole fish
  mutate(repl_95th_as_whole_fish= repl_95th/0.2) #convert to whole fish

macronutrients = read.csv("data/feed_nutritional_tables_avg.csv", check.names=F)
rep_fm_tot$feed_material= "fish"
rep_fm_tot = merge(rep_fm_tot, macronutrients[,c(1,6,7,8)], by="feed_material")
rep_fm_tot$increased_kcal_median = rep_fm_tot$repl_median_as_whole_fish*rep_fm_tot$kcal_t
rep_fm_tot$increased_kcal_5th = rep_fm_tot$repl_5th_as_whole_fish*rep_fm_tot$kcal_t
rep_fm_tot$increased_kcal_95th = rep_fm_tot$repl_95th_as_whole_fish*rep_fm_tot$kcal_t
rep_fm_tot$increased_prot_median = rep_fm_tot$repl_median_as_whole_fish*rep_fm_tot$prot_g_t
rep_fm_tot$increased_prot_5th = rep_fm_tot$repl_5th_as_whole_fish*rep_fm_tot$prot_g_t
rep_fm_tot$increased_prot_95th = rep_fm_tot$repl_95th_as_whole_fish*rep_fm_tot$prot_g_t
rep_fm_tot$increased_fat_median = rep_fm_tot$repl_median_as_whole_fish*rep_fm_tot$fat_g_t
rep_fm_tot$increased_fat_5th = rep_fm_tot$repl_5th_as_whole_fish*rep_fm_tot$fat_g_t
rep_fm_tot$increased_fat_95th = rep_fm_tot$repl_95th_as_whole_fish*rep_fm_tot$fat_g_t

##



#### 3) FISH OIL #####

# fish feed use (no fish oil used in livestock feed)
fish_feed_fo= ffeed[ffeed$feed_material %in% c("fish_oil"),]
fish_feed_fo = merge(fish_feed_fo, regions[, c(4,2)], by="Area Code")
ff_fo  <- fish_feed_fo%>%
  group_by(feed_material, RegionName) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  select(-c(`Area Code`))
ff_fo$use <- "use in fish feed"
ff_fo$Item <- "fish feed"

# Reduce from the total fishmeal use the amounts of fishmeal made from by-products
# FAO SOFIA report (2020): by-products are used to produce up to 25-35 percent of the total volume of fishmeal and fish oil
for(i in 3:502){
  ff_fo[,i] <- ff_fo[,i]*0.70
}
ff_fo$feed_use_median<-rowMedians(as.matrix(ff_fo[,c(3:502)]),na.rm=TRUE)
ff_fo$feed_use_5th<-apply(as.matrix(ff_fo[,c(3:502)]),MARGIN=1, function(x)quantile(x,0.05,na.rm=TRUE))
ff_fo$feed_use_95th<-apply(as.matrix(ff_fo[,c(3:502)]),MARGIN=1, function(x)quantile(x,0.95,na.rm=TRUE))

# Replacement potential
# fo with poultry oil 
# fo with fo from fisheries bps

# Availability of replacement materials

# 1) Poultry oil
# poultry_oil
poultry_oil_glob = colSums(byprods[byprods$feed_material == "poultry_oil",c(3:502)], na.rm=T)
# Their use as feed
poultry_oil_feed_use = Total_feed %>%
  filter(feed_material == "poultry_oil")%>%
  group_by(feed_material) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  select(-c(`Area Code`, hf, cf))
# The amounts not used as feed
poultry_oil_prod_reg = byprods %>%
  filter(feed_material =="poultry_oil")%>%
  group_by(RegionName, feed_material) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  select(-c(`Area Code`))
poultry_oil = poultry_oil_prod_reg[,c(1,2)]
for (i in 1:500){
  for(j in 1:nrow(poultry_oil)){
    poultry_oil[j,i+2]= (poultry_oil_glob[(i)]-poultry_oil_feed_use[,i+1])*(poultry_oil_prod_reg[j,i+2]/sum(poultry_oil_prod_reg[,i+2]))
  }
}

# 2) fish oil from unused fisheries bps
#bp_fo = fisheries_bp(years)
#Production
bp_fo_prod = read.csv("outputs/fish_bp_fo_2016_2018.csv", check.names = F) # To save time red in the previously saved output
bp_fo_prod = merge(bp_fo_prod, regions[,c(2,4)], by="Area Code")
bp_fo_glob = colSums(bp_fo_prod[,c(3:502)], na.rm=T)
# Their use as feed
bp_fo_feed_use = 340000 # This is from SOFIA report, 40% of fish oil produced (~0.85 mmt) made from by-products
# The amounts not used as feed
bp_fo_prod_reg = bp_fo_prod %>%
  group_by(RegionName) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  select(-c(`Area Code`))
bp_fo = bp_fo_prod_reg[,c(1,2)]
for (i in 1:500){
  for(j in 1:nrow(bp_fo)){
    bp_fo[j,i+2]= (bp_fo_glob[(i)]-bp_fo_feed_use)*(bp_fo_prod_reg[j,i+1]/sum(bp_fo_prod_reg[,i+1]))
  }
}


## Replacement

# Read in the replacement constraint table (nutritional and legislative constraints)
constr_fo = read.csv("data/Repl_constraint_fo2.csv")

# Merge fish oil feed use with replacement constraints for different feeds
rep_fo_feed = merge(ff_fo, constr_fo, by="Item")

# First calculate for each alternative feedstuff max and min feed use that can be replaced
# with that alternative feedstuff
# Run 500 monte carlo samples with the min and max from the replacement constraints and uniform distribution

#1 poultry oil
rep_fo1 = rep_fo_feed
set.seed(3)
for(i in 4:503){
  rep_fo1[,i]= rep_fo1[,i]*with(rep_fo1, runif(nrow(rep_fo1), min=repl_poultryoil_min, max=repl_poultryoil_max ))
}
#2 fo from byprods
rep_fo2 = rep_fo_feed
set.seed(3)
for(i in 4:503){
  rep_fo2[,i]= rep_fo2[,i]*with(rep_fo2, runif(nrow(rep_fo2), min=repl_fobp_min, max=repl_fobp_max ))
}

# Second, correct with the availability

alt_feed = c("poultry_oil", "bp_fo")
rep_fo_cor1 = rep_fo1
rep_fo_cor2 = rep_fo2


# Loop through the data frames of different alt feedstuff replacement potential
# If the sum of replacement potentials of the different items (=animal production groups) is higher than the regional availability
# then normalise (= divide the regional availability with the sum of replacement potential of different items and multiply this
# with the replacement potential)
# if not then leave the replacement potential as it is

for(k in 1:2){# loop through the number of replacement materials
  for(i in 4:503){ # loop through the 500 monte carlo simulation columns
    for(j in 1:nrow(get(paste("rep_fo", k, sep="")))){ # loop through all rows
      assign(paste0("rep_fo", k), `[<-` (get(paste0("rep_fo", k)), j, i, ifelse(sum(get(paste("rep_fo_cor", k, sep=""))[which(get(paste("rep_fo_cor", k, sep=""))$RegionName== get(paste("rep_fo_cor", k, sep=""))[j,"RegionName"]),i])>
                                                                                  get(alt_feed[k])[which(get(alt_feed[k])$RegionName==get(paste("rep_fo_cor", k, sep=""))[j,"RegionName"]),i-1],
                                                                                (get(alt_feed[k])[which(get(alt_feed[k])$RegionName==get(paste("rep_fo_cor", k, sep=""))[j,"RegionName"]),i-1]/
                                                                                   sum(get(paste("rep_fo_cor", k, sep=""))[which(get(paste("rep_fo_cor", k, sep=""))$RegionName== get(paste("rep_fo_cor", k, sep=""))[j,"RegionName"]),i]))*
                                                                                  get(paste("rep_fo_cor", k, sep=""))[j,i],
                                                                                get(paste("rep_fo_cor", k, sep=""))[j,i])))
    }
  }
}

# Third, correct so that the replacement potential won't exceed the feed use of an animal production group

# For this, first combine all the separate dataframes into one
rep_fo1$alt_feed = "poultry_oil"
rep_fo2$alt_feed = "fo_bp"


rep_fo = rbind(rep_fo1[,c(1:3, 513,4:503)], rep_fo2[,c(1:3, 513,4:503)])

# If the combined replacement potential of the different replacement materials exceeds the feed use of the animal group
# then normalise (= divide the feed use of the animal production group with the sum of replacement potentials of different feedstuff and multiply this
# with the replacement potential of the specific feedstuff in question)

items = unique(rep_fo$Item)


for(l in 1:length(items)){
  for(i in 5:504){
    if (sum(rep_fo[rep_fo$Item==items[l], i])> sum(ff_fo[ff_fo$Item== items[l]& ff_fo$feed_material== "fish_oil",i-2], na.rm=T)){
      rep_fo[rep_fo$Item == items[l], i]<- rep_fo[rep_fo$Item == items[l], i]*(sum(ff_fo[ff_fo$Item== items[l]& ff_fo$feed_material== "fish_oil",i-2], na.rm=T)/sum(rep_fo[rep_fo$Item==items[l], i]) )
    }
  }
}


# Calculate the sum of total potential

rep_fo_tot <- rep_fo %>%
  group_by(RegionName, feed_material) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))
rep_fo_tot$repl_median<-rowMedians(as.matrix(rep_fo_tot[,c(3:502)]),na.rm=TRUE)
rep_fo_tot$repl_5th<-apply(as.matrix(rep_fo_tot[,c(3:502)]),MARGIN=1, function(x)quantile(x,0.05,na.rm=TRUE))
rep_fo_tot$repl_95th<-apply(as.matrix(rep_fo_tot[,c(3:502)]),MARGIN=1, function(x)quantile(x,0.95,na.rm=TRUE))


## Calculate the increased food supply

## Calculate the increased food supply
# Here convert the fish oil saved to whole fish

rep_fo_tot<- rep_fo_tot %>%
  mutate(repl_median_as_whole_fish= repl_median/0.04) %>% #convert to whole fish
  mutate(repl_5th_as_whole_fish= repl_5th/0.04) %>% #convert to whole fish
  mutate(repl_95th_as_whole_fish= repl_95th/0.04) #convert to whole fish


macronutrients = read.csv("data/feed_nutritional_tables_avg.csv", check.names=F)
rep_fo_tot$feed_material= "fish"
rep_fo_tot = merge(rep_fo_tot, macronutrients[,c(1,6,7,8)], by="feed_material")
rep_fo_tot$increased_kcal_median = rep_fo_tot$repl_median_as_whole_fish*rep_fo_tot$kcal_t
rep_fo_tot$increased_kcal_5th = rep_fo_tot$repl_5th_as_whole_fish*rep_fo_tot$kcal_t
rep_fo_tot$increased_kcal_95th = rep_fo_tot$repl_95th_as_whole_fish*rep_fo_tot$kcal_t
rep_fo_tot$increased_prot_median = rep_fo_tot$repl_median_as_whole_fish*rep_fo_tot$prot_g_t
rep_fo_tot$increased_prot_5th = rep_fo_tot$repl_5th_as_whole_fish*rep_fo_tot$prot_g_t
rep_fo_tot$increased_prot_95th = rep_fo_tot$repl_95th_as_whole_fish*rep_fo_tot$prot_g_t
rep_fo_tot$increased_fat_median = rep_fo_tot$repl_median_as_whole_fish*rep_fo_tot$fat_g_t
rep_fo_tot$increased_fat_5th = rep_fo_tot$repl_5th_as_whole_fish*rep_fo_tot$fat_g_t
rep_fo_tot$increased_fat_95th = rep_fo_tot$repl_95th_as_whole_fish*rep_fo_tot$fat_g_t



#### 4) OILSEED OILS ######

# oilseed oils use in livestock and aquafeeds

lfeed_oil = lfeed[lfeed$subgroup2 == "oilseed_oils",]

# Aggregate to regional level
lfeed_oil = merge(lfeed_oil, regions[,c(2,4)], by="Area Code")
lfeed_oil <- lfeed_oil %>%
  group_by(subgroup2, Item, RegionName) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))

# Oilseedoils used in fishfeed
ffeed_oils = ffeed[ffeed$subgroup2 == "oilseed_oils",]
ffeed_oils = merge(ffeed_oils, regions[,c(2,4)], by="Area Code")
ffeed_oils <- ffeed_oils %>%
  group_by(subgroup2, RegionName) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))
ffeed_oils$Item = "fish feed"
feed_oils = rbind(lfeed_oil, ffeed_oils)
feed_oils_reg <- feed_oils %>%
  group_by( RegionName) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))
feed_oils_reg$feed_use_median<-rowMedians(as.matrix(feed_oils_reg[,c(3:502)]),na.rm=TRUE)
feed_oils_reg$feed_use_5th<-apply(as.matrix(feed_oils_reg[,c(3:502)]),MARGIN=1, function(x)quantile(x,0.05,na.rm=TRUE))
feed_oils_reg$feed_use_95th<-apply(as.matrix(feed_oils_reg[,c(3:502)]),MARGIN=1, function(x)quantile(x,0.95,na.rm=TRUE))

# Replacement potential
# oilseed oils with poultry oil 
# oilseed oils with fo from fisheries bps

# Availability of replacement materials

# 1) Poultry oil
# Availability calculated already in the replacement with fo
## Reduce the amounts already used in the replacement of fo with poultry oil
for(i in 3:502){
  poultry_oil[,i] = poultry_oil[,i]-rep_fo[rep_fo$alt_feed=="poultry_oil",i+2]
}

# 2) fish oil from unused fisheries bps
# Availability calculated already in the replacement with fo
## Reduce the amounts already used in the replacement of fo with poultry oil
for(i in 3:502){
  bp_fo[,i] = bp_fo[,i]-rep_fo[rep_fo$alt_feed=="fo_bp",i+2]
}

## Replacement

# Read in the replacement constraint table (nutritional and legislative constraints)
constr_oils = read.csv("data/Repl_constraint_oils2.csv")

# Merge fish oil feed use with replacement constraints for different feeds
rep_oils_feed = merge(feed_oils, constr_oils, by="Item")

# First calculate for each alternative feedstuff max and min feed use that can be replaced
# with that alternative feedstuff
# Run 500 monte carlo samples with the min and max from the replacement constraints and uniform distribution

#1 poultry oil
rep_oils1 = rep_oils_feed
set.seed(3)
for(i in 4:503){
  rep_oils1[,i]= rep_oils1[,i]*with(rep_oils1, runif(nrow(rep_oils1), min=repl_poultryoil_min, max=repl_poultryoil_max ))
}
#2 fo from byprods
rep_oils2 = rep_oils_feed
set.seed(3)
for(i in 4:503){
  rep_oils2[,i]= rep_oils2[,i]*with(rep_oils2, runif(nrow(rep_oils2), min=repl_fobp_min, max=repl_fobp_max ))
}

# Second, correct with the availability

alt_feed = c("poultry_oil", "bp_fo")
rep_oils_cor1 = rep_oils1
rep_oils_cor2 = rep_oils2

# Loop through the data frames of different alt feedstuff replacement potential
# If the sum of replacement potentials of the different items (=animal production groups) is higher than the regional availability
# then normalise (= divide the regional availability with the sum of replacement potential of different items and multiply this
# with the replacement potential)
# if not then leave the replacement potential as it is

for(k in 1:2){# loop through the number of replacement materials
  for(i in 5:504){ # loop through the 500 monte carlo simulation columns
    for(j in 1:nrow(get(paste("rep_oils", k, sep="")))){ # loop through all rows
      assign(paste0("rep_oils", k), `[<-` (get(paste0("rep_oils", k)), j, i, ifelse(sum(get(paste("rep_oils_cor", k, sep=""))[which(get(paste("rep_oils_cor", k, sep=""))$RegionName== get(paste("rep_oils_cor", k, sep=""))[j,"RegionName"]),i])>
                                                                                  get(alt_feed[k])[which(get(alt_feed[k])$RegionName==get(paste("rep_oils_cor", k, sep=""))[j,"RegionName"]),i-2],
                                                                                (get(alt_feed[k])[which(get(alt_feed[k])$RegionName==get(paste("rep_oils_cor", k, sep=""))[j,"RegionName"]),i-2]/
                                                                                   sum(get(paste("rep_oils_cor", k, sep=""))[which(get(paste("rep_oils_cor", k, sep=""))$RegionName== get(paste("rep_oils_cor", k, sep=""))[j,"RegionName"]),i]))*
                                                                                  get(paste("rep_oils_cor", k, sep=""))[j,i],
                                                                                get(paste("rep_oils_cor", k, sep=""))[j,i])))
    }
  }
}

# Third, correct so that the replacement potential won't exceed the feed use of an animal production group

# For this, first combine all the separate dataframes into one
rep_oils1$alt_feed = "poultry_oil"
rep_oils2$alt_feed = "fo_bp"


rep_oils = rbind(rep_oils1[,c(1:3, 510,5:504)], rep_oils2[,c(1:3, 510,5:504)])

# If the combined replacement potential of the different replacement materials exceeds the cereal feed use of the animal group
# then normalise (= divide the cereal feed use of the animal production group with the sum of replacement potentials of different feedstuff and multiply this
# with the replacement potential of the specific feedstuff in question)

items = unique(rep_oils$Item)

for(l in 1:length(items)){
  for(i in 5:504){
    if (sum(rep_oils[rep_oils$Item==items[l], i])> sum(feed_oils[feed_oils$Item== items[l]& feed_oils$subgroup2== "oilseed_oils",i], na.rm=T)){
      rep_oils[rep_oils$Item == items[l], i]<- rep_oils[rep_oils$Item == items[l], i]*(sum(feed_oils[feed_oils$Item== items[l]& feed_oils$subgroup2== "oilseed_oils",i], na.rm=T)/sum(rep_oils[rep_oils$Item==items[l], i]) )
    }
  }
}

# Calculate the sum of total potential

rep_oils_tot <- rep_oils %>%
  group_by(RegionName, alt_feed) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))
rep_oils_tot$repl_median<-rowMedians(as.matrix(rep_oils_tot[,c(3:502)]),na.rm=TRUE)
rep_oils_tot$repl_5th<-apply(as.matrix(rep_oils_tot[,c(3:502)]),MARGIN=1, function(x)quantile(x,0.05,na.rm=TRUE))
rep_oils_tot$repl_95th<-apply(as.matrix(rep_oils_tot[,c(3:502)]),MARGIN=1, function(x)quantile(x,0.95,na.rm=TRUE))


## Calculate the increased food supply

macronutrients = read.csv("data/feed_nutritional_tables_avg.csv", check.names=F)
rep_oils_tot$feed_material = "oilseed_oils"
rep_oils_tot = merge(rep_oils_tot, macronutrients[,c(1,6,7,8)], by="feed_material")
rep_oils_tot$increased_kcal_median = rep_oils_tot$repl_median*rep_oils_tot$kcal_t
rep_oils_tot$increased_kcal_5th = rep_oils_tot$repl_5th*rep_oils_tot$kcal_t
rep_oils_tot$increased_kcal_95th = rep_oils_tot$repl_95th*rep_oils_tot$kcal_t
rep_oils_tot$increased_prot_median = rep_oils_tot$repl_median*rep_oils_tot$prot_g_t
rep_oils_tot$increased_prot_5th = rep_oils_tot$repl_5th*rep_oils_tot$prot_g_t
rep_oils_tot$increased_prot_95th = rep_oils_tot$repl_95th*rep_oils_tot$prot_g_t
rep_oils_tot$increased_fat_median = rep_oils_tot$repl_median*rep_oils_tot$fat_g_t
rep_oils_tot$increased_fat_5th = rep_oils_tot$repl_5th*rep_oils_tot$fat_g_t
rep_oils_tot$increased_fat_95th = rep_oils_tot$repl_95th*rep_oils_tot$fat_g_t
# Aggregate to region level
rep_oils_tot <- rep_oils_tot %>%
  group_by(RegionName) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))


#### 5) PULSES #####


# Pulses used in livestock and aquafeeds

lfeed_pulses = lfeed[lfeed$subgroup2 == "pulses",]

# Aggregate to regional level
lfeed_pulses = merge(lfeed_pulses, regions[,c(2,4)], by="Area Code")
lfeed_pulses <- lfeed_pulses %>%
  group_by(subgroup2, Item, RegionName) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  select(-c(hf, `Area Code`))

# Add pulses used in fishfeed
ffeed_pulses = ffeed[ffeed$subgroup2 == "pulses",]
ffeed_pulses = merge(ffeed_pulses, regions[,c(2,4)], by="Area Code")
ffeed_pulses <- ffeed_pulses %>%
  group_by(subgroup2, RegionName) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))%>%
  select(-c(hf, `Area Code`))
ffeed_pulses$Item = "fish feed"

feed_pulses = rbind(lfeed_pulses, ffeed_pulses)
feed_pulses_reg <- feed_pulses %>%
  group_by(RegionName) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))
feed_pulses_reg$feed_use_median<-rowMedians(as.matrix(feed_pulses_reg[,c(2:501)]),na.rm=TRUE)
feed_pulses_reg$feed_use_5th<-apply(as.matrix(feed_pulses_reg[,c(2:501)]),MARGIN=1, function(x)quantile(x,0.05,na.rm=TRUE))
feed_pulses_reg$feed_use_95th<-apply(as.matrix(feed_pulses_reg[,c(2:501)]),MARGIN=1, function(x)quantile(x,0.95,na.rm=TRUE))

# Replacement potential
# pulses with oilseed meals
# pulses with livestock by-products
# pulses with fm from bps

# Availability of replacement materials
# Read in the potential replacements for pulses

# 1) Anim bps
# Calculate first how much animal bps are available and reduce the amount currently used as feed
# Calculated earlier in replacement with fm

# Reduce the amounts already used in the replacement of fm
# First aggregate fm replacement potentials
rep_fm_alt_feed <- rep_fm %>%
  group_by(RegionName, alt_feed) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))
for(i in 3:502){
  blood_meal_pig[,i] = blood_meal_pig[,i]-rep_fm_alt_feed[rep_fm_alt_feed$alt_feed=="blood_meal_pig",i]
}
for(i in 3:502){
  blood_meal_poultry[,i] = blood_meal_poultry[,i]-rep_fm_alt_feed[rep_fm_alt_feed$alt_feed=="blood_meal_poultry",i]
}
for(i in 3:502){
  meat_meal[,i] = meat_meal[,i]-rep_fm_alt_feed[rep_fm_alt_feed$alt_feed=="meat_meal",i]
}
for(i in 3:502){
  poultry_byproduct_meal[,i] = poultry_byproduct_meal[,i]-rep_fm_alt_feed[rep_fm_alt_feed$alt_feed=="poultry_byproduct_meal",i]
}


# 2) Oilseed meals
# Calculate first the availability and then reduce the amounts currently used as livestock and fish feed
# Calculated earlier in replacement with fm
# Remove the amounts already used in the replacement of fm
for(i in 3:502){
  omeals[,i] = omeals[,i]-rep_fm_alt_feed[rep_fm_alt_feed$alt_feed=="omeals",i]
}


# 3) fishmeal from unused fisheries bps
# Remove already the amounts used in the replacement of fm
for(i in 3:502){
  bp_fm[,i] = bp_fm[,i]-rep_fm_alt_feed[rep_fm_alt_feed$alt_feed=="bp_fm",i]
}

## Replacement potential

# Read in the replacement constraint table (nutritional and legislative constraints)
constr_pulses = read.csv("data/Repl_constraint_pulses2.csv")

# Merge cereals feed use with replacement constraints for different feeds
rep_pulses_feed = merge(feed_pulses, constr_pulses, by="Item")

# First calculate for each alternative feedstuff max and min feed use that can be replaced
# with that alternative feedstuff
# Run 500 monte carlo samples with the min and max from the replacement constraints and uniform distribution

#1 blood meal pig
rep_pulses1 = rep_pulses_feed
set.seed(3)
for(i in 4:503){
  rep_pulses1[,i]= rep_pulses1[,i]*with(rep_pulses1, runif(nrow(rep_pulses1), min=repl_blood_meal_pig_min, max=repl_blood_meal_pig_max ))
}
#2 blood meal poultry
rep_pulses2 = rep_pulses_feed
set.seed(3)
for(i in 4:503){
  rep_pulses2[,i]= rep_pulses2[,i]*with(rep_pulses2, runif(nrow(rep_pulses2), min=repl_blood_meal_poultry_min, max=repl_blood_meal_poultry_max ))
}
#3 feather meal
rep_pulses3 = rep_pulses_feed
set.seed(3)
for(i in 4:503){
  rep_pulses3[,i]= rep_pulses3[,i]*with(rep_pulses3, runif(nrow(rep_pulses3), min=repl_feath_min, max=repl_feath_max ))
}
#4 meat meal
rep_pulses4 = rep_pulses_feed
set.seed(3)
for(i in 4:503){
  rep_pulses4[,i]= rep_pulses4[,i]*with(rep_pulses4, runif(nrow(rep_pulses4), min=repl_meatmeal_min, max=repl_meatmeal_max ))
}
#5 poultry bp
rep_pulses5 = rep_pulses_feed
set.seed(3)
for(i in 4:503){
  rep_pulses5[,i]= rep_pulses5[,i]*with(rep_pulses5, runif(nrow(rep_pulses5), min=repl_poultrybp_min, max=repl_poultrybp_max ))
}
#6 fm from byproducts
rep_pulses6 = rep_pulses_feed
for(i in 4:503){
  rep_pulses6[,i]= rep_pulses6[,i]*with(rep_pulses6, runif(nrow(rep_pulses6), min=repl_fmbp_min, max=repl_fmbp_max ))
}
#7 oilseed meals
rep_pulses7 = rep_pulses_feed
for(i in 4:503){
  rep_pulses7[,i]= rep_pulses6[,i]*with(rep_pulses7, runif(nrow(rep_pulses7), min=repl_omeal_min, max=repl_omeal_max ))
}


# Second, correct with the availability

alt_feed = c("blood_meal_pig", "blood_meal_poultry", "hydrolyzed_feather_meal", "meat_meal", "poultry_byproduct_meal", "bp_fm", "omeals")
rep_pulses_cor1= rep_pulses1
rep_pulses_cor2= rep_pulses2
rep_pulses_cor3= rep_pulses3
rep_pulses_cor4= rep_pulses4
rep_pulses_cor5= rep_pulses5
rep_pulses_cor6= rep_pulses6
rep_pulses_cor7= rep_pulses7

# Loop through the data frames of different alt feedstuff replacement potential
# If the sum of replacement potentials of the different items (=animal production groups) is higher than the regional availability
# then normalise (= divide the regional availability with the sum of replacement potential of different items and multiply this
# with the replacement potential)
# if not then leave the replacement potential as it is

for(k in 1:7){# loop through the number of replacement materials
  for(i in 4:503){ # loop through the 500 monte carlo simulation columns
    for(j in 1:nrow(get(paste("rep_pulses", k, sep="")))){ # loop through all rows
      assign(paste0("rep_pulses", k), `[<-` (get(paste0("rep_pulses", k)), j, i, ifelse(sum(get(paste("rep_pulses_cor", k, sep=""))[which(get(paste("rep_pulses_cor", k, sep=""))$RegionName== get(paste("rep_pulses_cor", k, sep=""))[j,"RegionName"]),i])>
                                                                                  get(alt_feed[k])[which(get(alt_feed[k])$RegionName==get(paste("rep_pulses_cor", k, sep=""))[j,"RegionName"]),i-1],
                                                                                (get(alt_feed[k])[which(get(alt_feed[k])$RegionName==get(paste("rep_pulses_cor", k, sep=""))[j,"RegionName"]),i-1]/
                                                                                   sum(get(paste("rep_pulses_cor", k, sep=""))[which(get(paste("rep_pulses_cor", k, sep=""))$RegionName== get(paste("rep_pulses_cor", k, sep=""))[j,"RegionName"]),i]))*
                                                                                  get(paste("rep_pulses_cor", k, sep=""))[j,i],
                                                                                get(paste("rep_pulses_cor", k, sep=""))[j,i])))
    }
  }
}

# Third, correct so that the replacement potential won't exceed the feed use of an animal production group

# For this, first combine all the separate dataframes into one
rep_pulses1$alt_feed = "blood_meal_pig"
rep_pulses2$alt_feed = "blood_meal_poultry"
rep_pulses3$alt_feed = "hydrolyzed_feather_meal"
rep_pulses4$alt_feed = "meat_meal"
rep_pulses5$alt_feed = "poultry_byproduct_meal"
rep_pulses6$alt_feed = "bp_fm"
rep_pulses7$alt_feed = "omeals"

rep_pulses = rbind(rep_pulses1[,c(1:3, 518,4:503)], rep_pulses2[,c(1:3, 518,4:503)])
rep_pulses = rbind(rep_pulses,rep_pulses3[,c(1:3, 518,4:503)])
rep_pulses = rbind(rep_pulses,rep_pulses4[,c(1:3, 518,4:503)])
rep_pulses = rbind(rep_pulses,rep_pulses5[,c(1:3, 518,4:503)])
rep_pulses = rbind(rep_pulses,rep_pulses6[,c(1:3, 518,4:503)])
rep_pulses = rbind(rep_pulses,rep_pulses7[,c(1:3, 518,4:503)])

# If the combined replacement potential of the different replacement materials exceeds the cereal feed use of the animal group
# then normalise (= divide the cereal feed use of the animal production group with the sum of replacement potentials of different feedstuff and multiply this
# with the replacement potential of the specific feedstuff in question)

items = unique(rep_pulses$Item)

for(l in 1:length(items)){
  for(i in 5:504){
    if (sum(rep_pulses[rep_pulses$Item==items[l], i])> sum(Total_feed[Total_feed$Item== items[l]& Total_feed$subgroup2== "pulses",i], na.rm=T)){
      rep_pulses[rep_pulses$Item == items[l], i]<- rep_pulses[rep_pulses$Item == items[l], i]*(sum(Total_feed[Total_feed$Item== items[l]& Total_feed$subgroup2== "pulses",i], na.rm=T)/sum(rep_pulses[rep_pulses$Item==items[l], i]) )
    }
  }
}



# Calculate the sum of total potential

rep_pulses_tot <- rep_pulses %>%
  group_by(RegionName, subgroup2) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm=T))
rep_pulses_tot$repl_median<-rowMedians(as.matrix(rep_pulses_tot[,c(3:502)]),na.rm=TRUE)
rep_pulses_tot$repl_5th<-apply(as.matrix(rep_pulses_tot[,c(3:502)]),MARGIN=1, function(x)quantile(x,0.05,na.rm=TRUE))
rep_pulses_tot$repl_95th<-apply(as.matrix(rep_pulses_tot[,c(3:502)]),MARGIN=1, function(x)quantile(x,0.95,na.rm=TRUE))


## Calculate the increased food supply

macronutrients = read.csv("data/feed_nutritional_tables_avg.csv", check.names=F)
colnames(rep_pulses_tot)[2]="feed_material"
rep_pulses_tot = merge(rep_pulses_tot, macronutrients[,c(1,6,7,8)], by="feed_material")
rep_pulses_tot$increased_kcal_median = rep_pulses_tot$repl_median*rep_pulses_tot$kcal_t
rep_pulses_tot$increased_kcal_5th = rep_pulses_tot$repl_5th*rep_pulses_tot$kcal_t
rep_pulses_tot$increased_kcal_95th = rep_pulses_tot$repl_95th*rep_pulses_tot$kcal_t
rep_pulses_tot$increased_prot_median = rep_pulses_tot$repl_median*rep_pulses_tot$prot_g_t
rep_pulses_tot$increased_prot_5th = rep_pulses_tot$repl_5th*rep_pulses_tot$prot_g_t
rep_pulses_tot$increased_prot_95th = rep_pulses_tot$repl_95th*rep_pulses_tot$prot_g_t
rep_pulses_tot$increased_fat_median = rep_pulses_tot$repl_median*rep_pulses_tot$fat_g_t
rep_pulses_tot$increased_fat_5th = rep_pulses_tot$repl_5th*rep_pulses_tot$fat_g_t
rep_pulses_tot$increased_fat_95th = rep_pulses_tot$repl_95th*rep_pulses_tot$fat_g_t

####### summary of all #######

# Combine to a summary table the replacement potentials

# Show in the summary table the feed use, replacement potential, increased food supply

# 1) cereals
sum_table_cereals = Total_feed_cereals[,c(1,502:504)]
sum_table_cereals = cbind(sum_table_cereals, rep_cer_tot[,c(503:505, 509:517)]) 

# 1b) cereals replacement with crop residues and the reduced productivity
sum_table_cereals_b = Total_feed_cereals[,c(1,502:504)]
sum_table_cereals_b = cbind(sum_table_cereals_b, rep_cer_tot2[,c(503:505, 509:517)])
sum_table_cereals_b = cbind(sum_table_cereals_b, Redu_prod[,c(5:10)])
sum_table_cereals_b <- sum_table_cereals_b %>%
  mutate(increased_kcal_median= increased_kcal_median-((reduced_kcal_min + reduced_kcal_max)/2))%>%
  mutate(increased_kcal_5th= increased_kcal_5th-reduced_kcal_min)%>%
  mutate(increased_kcal_95th= increased_kcal_95th-reduced_kcal_max)%>%
  mutate(increased_prot_median= increased_prot_median-((reduced_prot_min + reduced_prot_max)/2))%>%
  mutate(increased_prot_5th= increased_prot_5th-reduced_prot_min)%>%
  mutate(increased_prot_95th= increased_prot_95th-reduced_prot_max)%>%
  mutate(increased_fat_median= increased_fat_median-((reduced_fat_min + reduced_fat_max)/2))%>%
  mutate(increased_fat_5th= increased_fat_5th-reduced_fat_min)%>%
  mutate(increased_fat_95th= increased_fat_95th-reduced_fat_max)%>%
  select(-c(reduced_kcal_min,reduced_kcal_max, reduced_prot_min,reduced_prot_max, reduced_fat_min,reduced_fat_max))

# 2) fishmeal
sum_table_fm = fm_use_agg[,c(2,504:506)]
sum_table_fm = cbind(sum_table_fm, rep_fm_tot[,c(503:505, 512:520)])
#colnames(sum_table_fm)[5:7]= c("repl_median", "repl_5th", "repl_95th")

# 3) fish oil
sum_table_fo = ff_fo[,c(2,506:508)]
sum_table_fo = cbind(sum_table_fo, rep_fo_tot[,c(503:505, 512:520)]) 
#colnames(sum_table_fo)[5:7]= c("repl_median", "repl_5th", "repl_95th")


# 4) oilseed oils
sum_table_oils = feed_oils_reg[,c(1,504:506)]
sum_table_oils = cbind(sum_table_oils, rep_oils_tot[,c(502:504, 508:516)])

# 5) pulses
sum_table_pulses = feed_pulses_reg[,c(1,502:504)]
sum_table_pulses = cbind(sum_table_pulses, rep_pulses_tot[,c(503:505, 509:517)])


# Combine
sum_table = t(as.data.frame(colSums(sum_table_cereals[,c(2:16)])))
sum_table =rbind(sum_table, t(as.data.frame(colSums(sum_table_cereals_b[,c(2:16)]))))
sum_table =rbind(sum_table, t(as.data.frame(colSums(sum_table_fm[,c(2:16)]))))
sum_table =rbind(sum_table, t(as.data.frame(colSums(sum_table_fo[,c(2:16)]))))
sum_table =rbind(sum_table, t(as.data.frame(colSums(sum_table_oils[,c(2:16)]))))
sum_table =rbind(sum_table, t(as.data.frame(colSums(sum_table_pulses[,c(2:16)]))))
rownames(sum_table) <- c("cereals", "cereals with cropres", "fishmeal", "fish oil", "oilseed oils", "pulses")
sum_table <- data.frame(feed_material = row.names(sum_table), sum_table)

sum_table$perc_repl = sum_table$repl_median/sum_table$feed_use_median
sum_table$perc_repl_5th = sum_table$repl_5th/sum_table$feed_use_5th
sum_table$perc_repl_95th = sum_table$repl_95th/sum_table$feed_use_95th

# Add columns about how much the increased food supply is from current use
# Global food supply
# Total pop (average 2016-2018) : 7365063000
total_pop =7365063000
losses = 1.084  # 8.4% from Kummu et al. (2012) for the supply chain losses without production and consumption
# Global food supply = 2918.7 kcal/cap/day
kcal_glob = (2918.7 * 365 * total_pop *losses)/10^12
# Global protein supply = 82.5 g/cap/day
prot_glob = (82.5 * 365 * total_pop *losses)/10^12
# Global fat supply (85.2 g/cap/year
fat_glob = (85.2 * 365 * total_pop *losses)/10^12

sum_table$kcal_perc_use_median = (sum_table$increased_kcal_median/10^12)/kcal_glob
sum_table$kcal_perc_use_5th = (sum_table$increased_kcal_5th/10^12)/kcal_glob
sum_table$kcal_perc_use_95th = (sum_table$increased_kcal_95th/10^12)/kcal_glob
sum_table$prot_perc_use_median = (sum_table$increased_prot_median/10^12)/prot_glob
sum_table$prot_perc_use_5th = (sum_table$increased_prot_5th/10^12)/prot_glob
sum_table$prot_perc_use_95th = (sum_table$increased_prot_95th/10^12)/prot_glob
sum_table$fat_perc_use_median = (sum_table$increased_fat_median/10^12)/fat_glob
sum_table$fat_perc_use_5th = (sum_table$increased_fat_5th/10^12)/fat_glob
sum_table$fat_perc_use_95th = (sum_table$increased_fat_95th/10^12)/fat_glob
