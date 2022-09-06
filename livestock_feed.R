# Livestock feed updated

# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# data should be its subfolder

library(tidyr)
library(dplyr)
library(matrixStats)
library(truncnorm)

# If you don't read the code as a function you need to specify which years you want to analyze
# x= c(2016,2017,2018) # These years used in the paper

livestock_feed <- function(x) {
  start.time <- Sys.time()
# First, calculate the share of production systems for each region

GLEAM= read.csv("data/GLEAM_production_systems.csv", check.names = F)

for(i in 1:nrow(GLEAM)){
  GLEAM[i,"prod_system_ratio"]= GLEAM[i,6]/(GLEAM[which(GLEAM$Commodity==GLEAM[i,4]& GLEAM$GLEAMgroup==GLEAM[i,1]& GLEAM$`Animal species`==GLEAM[i,2]& GLEAM$`Production system`== "Aggregated"),6])
}
GLEAM = GLEAM[-which(GLEAM$`Production system`== "Aggregated"),] # Remove the aggregated category to avoid double counting

# Then multiply the feed use quantities from GLEAM

# Import data on livestock production

#FAO_animprod = read.csv("data/Production_LivestockPrimary_E_All_Data_NOFLAG.csv", header=T, sep=",", check.names = F)
#save(FAO_animprod, file="data/FAO_animprod.Rdata") # These two lines you need to run only the first time

# load prod data (previously read from csv files and saved as Rdata)
load("data/FAO_animprod.Rdata")

FAO_animprod = gather(FAO_animprod, Year, Value, 8:65) # from wide to long
FAO_animprod[,"Year"] <- sub("Y", "", FAO_animprod[,"Year"]) # Remove "Y" from years
FAO_animprod = FAO_animprod[which(FAO_animprod$`Element Code`=="5510" & FAO_animprod$Year %in% x),] # choose only production and years selected
FAO_animprod = FAO_animprod[!is.na(FAO_animprod$Value),] # Remove rows with NA
FAO_animprod= spread(FAO_animprod, Year, Value)
FAO_animprod$mean = rowSums(FAO_animprod[,c(8,9,10)])/3
countries = read.csv("data/countries.csv", check.names = F) # Read in a file with country codes and regional divisions
FAO_animprod = merge(x=FAO_animprod, y=countries[,c(2,12,13,14,15)], by="Area Code")

# Merge with % of different production systems
FAO_animprod =merge(FAO_animprod, GLEAM[,c(1,3,5,7)], by=c("GLEAMgroup", "Item Code"))
# Here other than the main product categories are dropped
#Check that the world production is still ok
FAO_animprod$prod = FAO_animprod$mean*FAO_animprod$prod_system_ratio

# Remove China separate regions and leave only China (351)
FAO_animprod <- FAO_animprod %>% 
  filter(! `Area Code` %in% c(41,96,214, 128)) # Remove the separate China provinces


# Import FCRs
livestockFCR = read.csv("data/Livestock_FCR_2.csv", check.names = F)
# Livestock FCRs from Mekonnen & Hoekstra, 2012, Mottet et al., 2017 and Herrero et al., 2014
# FCRs in kg of feed (dry mass) per kg of output
FAO_animprod = merge(x=FAO_animprod, y=livestockFCR[,c(1,3,4,8,9,10,11,12)], by=c("Item Code", "Production system", "RegionName"), all.x=T)

### Monte carlos to add uncertainty in the FCRs ###
FCR<-function(a,b,c,d) {
  fcr <- round(rtruncnorm(1,a=a, b=b, mean=d, sd=d),2)
  return(fcr)
}
### estimating total feed use with different FCRs
FAO_animprod_sens <-matrix(NA, dim(FAO_animprod)[1],500)
set.seed(3)

for (i in 1:500){
  FAO_animprod_sens[,i]<-FAO_animprod$mean*(FAO_animprod$prod_system_ratio)*mapply(FCR, FAO_animprod$fcr_min, FAO_animprod$fcr_max, FAO_animprod$fcr_mean, FAO_animprod$fcr_sd)
}

# Feed used 5% and 95% percentile
FAO_animprod$FeedUse_median<-rowMedians(FAO_animprod_sens,na.rm=TRUE)
FAO_animprod$FeedUse_5th<-apply(FAO_animprod_sens,MARGIN=1, function(x)quantile(x,0.05,na.rm=TRUE))
FAO_animprod$FeedUse_95th<-apply(FAO_animprod_sens,MARGIN=1, function(x)quantile(x,0.95,na.rm=TRUE))

sum(FAO_animprod$FeedUse_median, na.rm=T) #DM feed use in tonnes

# Merge with the % of different feeds
Feed = read.csv("data/livestock_feed_use2.csv", check.names = F)
# feed use from GLEAM 2.0: with the modified fishmeal numbers (global means for pig 0.15 and poultry 0.02) (Froelich et al.2018) without weighting
# and oilseed meals in ruminant categories to individual meals, grains in cattle feed to individual grains based on global feed use rations
Feed = gather(Feed, GLEAMgroup, feed_perc, 7:16)
#FAO_animprod = merge(x=FAO_animprod, y=Feed[,c(3:8)], by=c("Item Code", "Production system", "GLEAMgroup"), all.x=T)
# mean ingr use in livestock feed

# Multiply with the 500 monte carlo samples of the total feed use
Feed_use = cbind(FAO_animprod[,c(1,2,4,5,6,7)], FAO_animprod_sens)
Feed_use = merge(y=Feed[,c(3:8)],x=Feed_use, by=c("Item Code", "Production system", "GLEAMgroup"), all.x=T)
for(i in 7:506){
   Feed_use[,i] = Feed_use[,i]*(Feed_use$"feed_perc"/100) 
  }


end.time <- Sys.time()
time.taken <- end.time - start.time
show(time.taken)
return(Feed_use[,c(508,4,5,6, 7:506)])

}

#write.csv(Feed_use[,c(508,4,5,6, 7:506)], "outputs/livestock_feed_updated.csv", row.names = F)
