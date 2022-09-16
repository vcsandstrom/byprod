# Estimating the use of different ingredients in fish feed
# Based on data from Max Troell and his colleagues (based on Tacon et al.2011 survey)

# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# data should be its subfolder

library(dplyr)
library(tidyr)
library(matrixStats)

#x=c(2016,2017,2018) # Years analyzed in this paper

fish_feed <- function(x) {
  start.time <- Sys.time()
  
  # Import data of fish production for 2010
  ### to calculate the diet per country for this year###
  
  FishProd_orig = read.csv("data/FishProductionFishStat_2010.csv", check.names = F)
  # fed species 
  speciesGroups = read.csv("Data/speciesGroups2.csv")
  
  FishProd = merge(FishProd_orig, speciesGroups, by="Name_en", all=T)
  
  ### New data on FCRs, % on feeds and %FM and %FO from Tacon et al. 2015 & Tacon et al. 2011
  New_FCR<-read.csv("data/New_FCR.csv")
  
  ###2010 TAcon et al.data###
  PercFeed = read.csv("data/PercFeed.csv")
  
  # using the updated 2010 FCRs
  New_FCR_year<-New_FCR[which(New_FCR$Year=="2010"),]
  New_FCR_year<-New_FCR_year[ order(match(New_FCR_year$Taxa, PercFeed$Taxa)), ]
  PercFeed[1:11,]<-New_FCR_year[1:11,c(1,3,4)]
  ### For freshwater crayfish same as fw crustaceans
  PercFeed[12,2:3]<-New_FCR_year[9,3:4]
  ### For mullets same as marine fish
  PercFeed[13,2:3]<-New_FCR_year[4,3:4]
  ###percent on farm made feeds and new FCRs for them and "trash fish" for marine fish, shrimps and mullets 
  PercFeed$Farm<-100-PercFeed$PercFeed
  PercFeed$farmEFCR<-PercFeed$EFCR*1.5
  PercFeed$farmEFCR[4]<-10 #marine fish trash fish 1:10 ratio
  PercFeed$farmEFCR[3]<-6 # shrimps trash fish 1:6 ratio
  PercFeed$farmEFCR[13]<-10 #mullets fish trash fush 1:10 ratio
  ### FCRs for shrimp and marine fish farm made feeds from FAO reports & F&F article
  
  FishProd = merge(x=FishProd, PercFeed, by="Taxa", all.x=T)
  
  # Choose only rows with no NA - so only the species that are fed
  FishProd=FishProd[!is.na(FishProd$EFCR),]
  
  # Feed used for commercial
  FishProd$FeedUse = FishProd$`2010`*(FishProd$PercFeed/100)*FishProd$EFCR
  # Calculate the % of world production for each species
  for (i in 1:nrow(FishProd)){
    FishProd[i,"PercProd"] = FishProd[i,"2010"]/sum(FishProd[which(FishProd$Taxa==FishProd[i,"Taxa"]),"2010"], na.rm=T)
  }
  
  ### Feed used for farm made feed
  FishProd$farm_FeedUse = FishProd$`2010`*(FishProd$Farm/100)*FishProd$farmEFCR
  trashfish<-sum(c(FishProd$farm_FeedUse[which(FishProd$Taxa=="marine fishes")],FishProd$farm_FeedUse[which(FishProd$Taxa=="shrimps")],FishProd$farm_FeedUse[which(FishProd$Taxa=="mullets")]),na.rm=T)
  all_rest_farm_feeds<-sum(c(FishProd$farm_FeedUse[which(!FishProd$Taxa=="marine fishes")],FishProd$farm_FeedUse[which(FishProd$Taxa=="shrimps")],FishProd$farm_FeedUse[which(FishProd$Taxa=="mullets")]),na.rm=T)
  
  # Assign the feed use of marine fish and shrimps fed by farm made feeds to 0 
  # because the assumption is that they are only fed with whole fish
  FishProd[which(FishProd$Taxa== "marine fishes" |FishProd$Taxa== "shrimps"|FishProd$Taxa== "mullets"),"farm_FeedUse"] <- 0
  
  # Use of different feed ingredients
  Feed = read.csv("data/Aquafeed_wide.csv", check.names = F, stringsAsFactors = FALSE)
  ### correcting for Venezuela because it is not read properly
  Feed$Country[which(Feed$Country=="Venezuela")]<-"Venezuela, Boliv Rep of"
  
  
  ### already change the freshwater fish diet composition because it is clearly wrong now ###
  #### keeping FM to average and normalize the rest ####
  fwfish<-Feed[which(Feed$Taxa=="fw fishes"&Feed$Range=="Average"),]
  FM_av<-as.numeric(fwfish[14])
  
  fwfish[c(4:13,15:39)]<-(fwfish[c(4:13,15:39)]/sum(fwfish[c(4:13,15:39)]))*(100-FM_av)
  Feed[which(Feed$Taxa=="fw fishes"&Feed$Range=="Average"),]<-fwfish
  ###############################################
  ### Estimating a 100% inclusion of all ingredients for the average composition
  ### based on amount of each ingredient with optimization algorithm
  
  #####using proportions to sum to 100 the diet
  cells<-which(Feed$Range== "Average")
  cells_min<-which(Feed$Range== "Min")
  cells_max<-which(Feed$Range== "Max")
  for (i in 1:length(cells)){
    min<-as.numeric(Feed[cells_min[i], 4:39])
    max<-as.numeric(Feed[cells_max[i], 4:39])
    ##current composition of feed as it is in the data
    C_comp<-as.numeric(Feed[cells[i], 4:39]) 
    ## if the maximum is less than 100% substitute the average with the max and move to the next
    if (sum(max)<100){
      Feed[cells[i], 4:39]<-max
      
    }else  ## if the minimum is more than 100% substitute the average with the min that is first normalized to 100 and move to the next
      if (sum(min)>100){
        Feed[cells[i], 4:39]<-min/sum(min)*100
        
      }else
        ### if there is no range but same input for all 3 options ##
        if(identical(min,max)){
          if(min>100){
            Feed[cells[i], 4:39]<-min/sum(min)*100 
          }else{
            Feed[cells[i], 4:39]<-min
          }
          ##move to the next one for normalization
          
        }else{
          Feed[cells[i],4:39]<-C_comp/sum(C_comp)*100 
        }}
  ##########################################################
  ###check that sums to 100 most of them ###
  rowSums(Feed[cells,4:39]) # It does
  
  #########data merge #########
  colnames(FishProd)[3]="Country"
  FishProd_ave = merge(x=FishProd, y=Feed[which(Feed$Range== "Average"),], by=c("Taxa", "Country"), all.x=T)
  ### adding an id to help with changing from long to wide and rearrange so it is not lost when gathering
  FishProd_ave$Id<-1:dim(FishProd_ave)[1]
  FishProd_ave<-FishProd_ave[,c(54,1:53)]
  
  ###Removing range column as it is not needed and just cause confusion ###
  FishProd_ave<-subset(FishProd_ave, select = -Range)
  
  # Add feed use to all other countries based on world average feed use for the specific taxa
  # First change from wide to long
  FishProd_ave = gather(FishProd_ave, Ingredient, PercInFeed, 18:53,factor_key=TRUE)
  # calculate the world average use of different ingredients for different taxa
  wa_agg_ave= FishProd_ave %>% 
    group_by(Taxa, Ingredient) %>% 
    summarise(wa = weighted.mean(PercInFeed, PercProd, na.rm=T))
  
  
  FishProd_ave = merge(x=FishProd_ave, y= wa_agg_ave, by=c("Ingredient", "Taxa"), all.x=T)
  
  # If the feed use ratio is given to the country by Tacon et al. 2011 use that, 
  # if not then use the production weighted world average for that taxa
  
  for(i in 1:nrow(FishProd_ave)){
    FishProd_ave[i,"Comb_FeedPerc"] = ifelse(!is.na(FishProd_ave[i,"PercInFeed"]),FishProd_ave[i,"PercInFeed"], FishProd_ave[i,"wa"])
  }
  
  ### one extra step added here: Now FM and FO use is overestimated thus, we will substitute the mean FM and FO
  ### for the countries not included in the survey data to match this years FM production based on IFFO
  ### Removing the PercInfeed & wa columns (not needed anymore) as they cause problems with spread #####
  FishProd_ave<-subset(FishProd_ave, select = -c(PercInFeed,wa))
  ### from long to wide again to make the diet change###
  FishProd_ave <- spread(FishProd_ave, Ingredient, Comb_FeedPerc)
  New_FMFO_year<-New_FCR[which(New_FCR$Year=="2010"),]
  New_FMFO_year<-New_FMFO_year[,c(1,6,7)]
  for (i in 1: dim(FishProd_ave[,18:53])[1]){
    
    countries<-unique(Feed$Country[Feed$Taxa==FishProd_ave$Taxa[i]])
    if(FishProd_ave$Country[i]%in% countries){
      i<-i+1
    }else{
      C_comp<-as.numeric(FishProd_ave[i,18:53])
      ## correcting for the taxa that new info is not available
      Taxa<-as.character(FishProd_ave$Taxa[i])
      if(Taxa=="fw crayfish"){
        Taxa<-"fw crustaceans"
      }else if (Taxa=="mullets"){
        Taxa<-"marine fishes"
      }
      ### New FM and FO inclusion ##
      FM<-New_FMFO_year[which(New_FMFO_year$Taxa==Taxa),2]
      FO<-New_FMFO_year[which(New_FMFO_year$Taxa==Taxa),3]
      ## if the new inclusion of fish meal is lower than the current then change it 
      if(FM<C_comp[11]){
        FM_dif<-C_comp[11]-FM
        C_comp[11]<-FM
        
        ### If the new inclusion is higher than the current let it as it is
      } else if (FM>=C_comp[11]){
        FM_dif<-0
      }
      ## if the new inclusion of fish oil is lower than the current then change it 
      if(FO<C_comp[10]){
        FO_dif<-C_comp[10]-FO
        C_comp[10]<-FO
        ### If the new inclusion is higher than the current let it as it is
      } else if (FO>=C_comp[10]){
        FO_dif<-0
      }
      ### Proportion of current composition without the FM and FO
      Prop<-C_comp[c(1,5,7,9,12:18,20,21,23,25,30,31,33,35)]/sum(C_comp[c(1,5,7,9,12:18,20,21,23,25,30,31,33,35)])
      ### New added extra ingredients from the removal of FM + FO
      ## first FM
      New_ingr<-Prop*(FM_dif)
      C_comp[c(1,5,7,9,12:18,20,21,23,25,30,31,33,35)]<-C_comp[c(1,5,7,9,12:18,20,21,23,25,30,31,33,35)]+New_ingr
      
      ### now the fats
      Prop_oil<-C_comp[c(22,24,27,32)]/sum(C_comp[c(22,24,27,32)])
      if(is.na(sum(Prop_oil))){
        New_oil<-rep(FO_dif/4,4)
      }else{
        New_oil<-Prop_oil*FO_dif}
      C_comp[c(22,24,27,32)]<-C_comp[c(22,24,27,32)]+New_oil
      FishProd_ave[i,18:53]<-C_comp
      
    }
  }
  
  # Export average fishfeed composition for 2010, Comb_FeerPerc indicates the % in feed
 # write.csv(FishProd_ave, file="outputs/Fishfeed_2010_allcountries.csv", row.names = F)
  ###check that sums to 100 most of them ###
  rowSums(FishProd_ave[cells,18:53])
  
  
  ### Here after we have the diet composition for all countries for 2010 we update the diet based on FM and FO reduction
  ### for the years of interest
  #### Sharing the difference in FM and FO to the ingredients that contain more than 40% protein and the rest oil sources###
  ##Updated FM and FO% for 2015 or 2020
  
  
  New_FMFO_year<-New_FCR[which(New_FCR$Year=="2015"),]
  New_FMFO_year<-New_FMFO_year[,c(1,6,7)]
  
  for (i in 1:dim(FishProd_ave[,18:53])[1]){
    # current diet composition
    
    C_comp<-as.numeric(FishProd_ave[i,18:53])
    ## correcting for the taxa that new info is not available
    Taxa<-as.character(FishProd_ave$Taxa[i])
    if(Taxa=="fw crayfish"){
      Taxa<-"fw crustaceans"
    }else if (Taxa=="mullets"){
      Taxa<-"marine fishes"
    }
    ### New FM and FO inclusion ##
    FM<-New_FMFO_year[which(New_FMFO_year$Taxa==Taxa),2]
    FO<-New_FMFO_year[which(New_FMFO_year$Taxa==Taxa),3]
    ## if the new inclusion of fish meal is lower than the current then change it 
    if(FM<C_comp[11]){
      FM_dif<-C_comp[11]-FM
      C_comp[11]<-FM
      
      ### If the new inclusion is higher than the current let it as it is
    } else if (FM>=C_comp[11]){
      FM_dif<-0
    }
    ## if the new inclusion of fish oil is lower than the current then change it 
    if(FO<C_comp[10]){
      FO_dif<-C_comp[10]-FO
      C_comp[10]<-FO
      ### If the new inclusion is higher than the current let it as it is
    } else if (FO>=C_comp[10]){
      FO_dif<-0
    }
    ### Proportion of current composition without the FM and FO
    Prop<-C_comp[c(1,5,7,9,12:18,20,21,23,25,30,31,33,35)]/sum(C_comp[c(1,5,7,9,12:18,20,21,23,25,30,31,33,35)])
    ### New added extra ingredients from the removal of FM + FO
    ## first FM
    New_ingr<-Prop*(FM_dif)
    C_comp[c(1,5,7,9,12:18,20,21,23,25,30,31,33,35)]<-C_comp[c(1,5,7,9,12:18,20,21,23,25,30,31,33,35)]+New_ingr
    
    ### now the fats
    Prop_oil<-C_comp[c(22,24,27,32)]/sum(C_comp[c(22,24,27,32)])
    if(is.na(sum(Prop_oil))){
      New_oil<-rep(FO_dif/4,4)
    }else{
      New_oil<-Prop_oil*FO_dif}
    C_comp[c(22,24,27,32)]<-C_comp[c(22,24,27,32)]+New_oil
    FishProd_ave[i,18:53]<-C_comp
  }
  
  ### Up till here diet composition has been estimated and updated  based on 2010 inclusion rates and mean %FM and mean %FO in diets
  ### for 2015. Next step is to estimate new production for reference years and do the following calculations for total ingredient use
  
  FishProd_new = read.csv("data/aquaculture_2015_2018.csv", check.names = F,stringsAsFactors = FALSE)
  FishProd_old = read.csv("data/aquaculture_2000_2017.csv", check.names = F,stringsAsFactors = FALSE)
  FishProd_new = merge(FishProd_new, FishProd_old[,1:20], by=c("Land Area", "Name_en","Ocean Area","Environment"), all=T)
  FishProd_new = FishProd_new[,c(1,2,4:ncol(FishProd_new),3)]
  
  # Select the years defined by the function call
  # years = c(2011,2012,2013) # for the harmonization
  FishProd_new = FishProd_new[,c(1,2,3,4,5,which(colnames(FishProd_new) %in% x))]
  
  ### same groups but added if the previous step not run again 
  speciesGroups = read.csv("Data/speciesGroups2.csv")
  FishProd_new = merge(FishProd_new, speciesGroups, by="Name_en", all=T)
  
  #Removing species outside the taxa we are interested & species that seems are not anymore included
  FishProd_new<-FishProd_new[!is.na(FishProd_new$Taxa),]
  FishProd_new<-FishProd_new[!is.na(FishProd_new$Environment),]
  
  
  
  ### Selecting what production values to use
  
  FishProd_new$mean<-rowSums(FishProd_new[,which(colnames(FishProd_new) %in% x)])/3
  
  ### Now we can calculate the new total feed and combine the new year production to the diet composition
  #updated FCRs and % on feeds for the years of interest (2015 now) 
  ### New data on FCRs, % on feeds and %FM and %FO from Tacon et al. 2015 & Tacon et al. 2011
  New_FCR_year<-New_FCR[which(New_FCR$Year=="2015"),]
  New_FCR_year<-New_FCR_year[ order(match(New_FCR_year$Taxa, PercFeed$Taxa)), ]
  
  
  
  PercFeed[1:11,]<-New_FCR_year[1:11,c(1,3,4)]
  ### For freshwater crayfish same as fw crustaceans
  PercFeed[12,2:3]<-New_FCR_year[9,3:4]
  ### For mullets same as marine fish
  PercFeed[13,2:3]<-New_FCR_year[4,3:4]
  ###percent on farm made feeds and new FCRs for them and "trash fish" for marine fish, shrimps and mullets 
  PercFeed$Farm<-100-PercFeed$PercFeed
  PercFeed$farmEFCR<-PercFeed$EFCR*1.5
  PercFeed$farmEFCR[4]<-10 #marine fish trash fish 1:10 ratio
  PercFeed$farmEFCR[3]<-6 # shrimps trash fish 1:6 ratio
  PercFeed$farmEFCR[13]<-10 #mullets fish trash fush 1:10 ratio
  
  
  FishProd_new<-merge(x=FishProd_new, PercFeed, by="Taxa", all.x=T)
  # Calculate the % of world production for each species
  for (i in 1:nrow(FishProd_new)){
    FishProd_new[i,"PercProd"] = FishProd_new[i,"mean"]/sum(FishProd_new[which(FishProd_new$Taxa==FishProd_new[i,"Taxa"]),"mean"], na.rm=T)
  }
  ### Monte carlos to add uncertainty in the FCRs ###
  ## with a cv 0.1 and normal distribution
  cv<-0.1
  
  Feed_use_sens<-matrix(NA, dim(FishProd_new)[1],500)
  Feed_use_sens_farm<-matrix(NA, dim(FishProd_new)[1],500)
  ### estimating total feed use for the new production years
  set.seed(3)
  for (i in 1:500){
    Feed_use_sens[,i]<-FishProd_new$mean*(FishProd_new$PercFeed/100)*sapply(FishProd_new$EFCR,function(x) round(rnorm(1,x,cv*x),2))
    
    Feed_use_sens_farm[,i]<-FishProd_new$mean*(FishProd_new$Farm/100)*sapply(FishProd_new$farmEFCR,function(x) round(rnorm(1,x,cv*x),2))
    
  }
  # Feed used for commercial and 5% and 95% percentile
  FishProd_new$FeedUse<-rowMedians(Feed_use_sens,na.rm=TRUE)
  FishProd_new$FeedUse_5th<-apply(Feed_use_sens,MARGIN=1, function(x)quantile(x,0.05,na.rm=TRUE))
  FishProd_new$FeedUse_95th<-apply(Feed_use_sens,MARGIN=1, function(x)quantile(x,0.95,na.rm=TRUE))
  
  ### Feed used for farm made feed and 5% and 95% percentile
  FishProd_new$farm_FeedUse<-rowMeans(Feed_use_sens_farm,na.rm=TRUE)
  FishProd_new$farm_FeedUse_5th<-apply(Feed_use_sens_farm,MARGIN=1, function(x)quantile(x,0.05,na.rm=TRUE))
  FishProd_new$farm_FeedUse_95th<-apply(Feed_use_sens_farm,MARGIN=1, function(x)quantile(x,0.95,na.rm=TRUE))
  
  #### trash fish estimations
  trashfish<-sum(c(FishProd_new$farm_FeedUse[which(FishProd_new$Taxa=="marine fishes")],FishProd_new$farm_FeedUse[which(FishProd_new$Taxa=="shrimps")],FishProd$farm_FeedUse[which(FishProd$Taxa=="mullets")]),na.rm=T)
  trashfish_5th<-sum(c(FishProd_new$farm_FeedUse_5th[which(FishProd_new$Taxa=="marine fishes")],FishProd_new$farm_FeedUse_5th[which(FishProd_new$Taxa=="shrimps")],FishProd$farm_FeedUse_5th[which(FishProd$Taxa=="mullets")]),na.rm=T)
  trashfish_95th<-sum(c(FishProd_new$farm_FeedUse_95th[which(FishProd_new$Taxa=="marine fishes")],FishProd_new$farm_FeedUse_95th[which(FishProd_new$Taxa=="shrimps")],FishProd$farm_FeedUse_95th[which(FishProd$Taxa=="mullets")]),na.rm=T)
  
  Feed_use_sens = cbind(FishProd_new[,c(1,3)], Feed_use_sens)
  Feed_use_sens_farm = cbind(FishProd_new[,c(1,3)], Feed_use_sens_farm)
  
  # Assign the feed use of marine fish and shrimps fed by farm made feeds to 0 
  # because the assumption is that they are only fed with whole fish
  Feed_use_sens_farm[which(Feed_use_sens_farm$Taxa== "marine fishes" |Feed_use_sens_farm$Taxa== "shrimps"|Feed_use_sens_farm$Taxa== "mullets"),3:502] <- 0
  
  ### Combine new production to diet ####
  Diet<-FishProd_ave[,c(1:4,18:53)]
  Diet[is.na(Diet)] <- 0
  # from wide to long
  Diet_long <- gather(Diet, Ingredient, Comb_FeedPerc, 5:ncol(Diet))
  # aggregate to taxa level
  Diet_long <- Diet_long %>%
    group_by(Taxa, Country, Ingredient) %>% 
    summarise(Comb_FeedPerc=mean(Comb_FeedPerc))
  
  names(Feed_use_sens)[names(Feed_use_sens) == "Land Area"] <- "Country"
  names(Feed_use_sens_farm)[names(Feed_use_sens_farm) == "Land Area"] <- "Country"
  # aggregate to taxa level
  Feed_use_sens <- Feed_use_sens %>%
    group_by(Taxa, Country) %>% 
    summarise(across(where(is.numeric), .fns = sum, na.rm=T))

  Feed_use_sens_ingr = merge(x=Feed_use_sens,y=Diet_long, by=c("Taxa", "Country"), all.x=T)
  Feed_use_sens_farm_ingr = merge(x=Feed_use_sens_farm,y=Diet_long, by=c("Taxa", "Country"), all.x=T)
  
  ### select the ones with NA, because they were not produced in 2010 and check their importance
  Nas<-unique(Feed_use_sens_ingr[which(is.na(Feed_use_sens_ingr$Comb_FeedPerc )),c("Taxa", "Country", "1")])
  sum(Nas$`1`) # 22.99023 t, insignificant amount so we can exclude these
  
  # Multiply feed use with the % of the different ingredients
  for (i in 3:502){
    Feed_use_sens_ingr[,i]= Feed_use_sens_ingr[,i]*(Feed_use_sens_ingr$Comb_FeedPerc/100)
  }
  # Aggregate so that we'll have the total feed ingredient use per country for the 500 simulations
  library(dplyr)
  Feed_use_sens_ingr <- Feed_use_sens_ingr %>%
    group_by(Country, Ingredient) %>% 
    summarise(across(where(is.numeric), .fns = sum)) %>%
    select(-Comb_FeedPerc)
  
  # Multiply farm feed use with the % of the different ingredients
  for (i in 3:502){
    Feed_use_sens_farm_ingr[,i]= Feed_use_sens_farm_ingr[,i]*(Feed_use_sens_farm_ingr$Comb_FeedPerc/100)
  }
  # Aggregate so that we'll have the total feed ingredient use per country for the 500 simulations
  Feed_use_sens_farm_ingr <- Feed_use_sens_farm_ingr %>%
    group_by(Country, Ingredient) %>% 
    summarise(across(where(is.numeric), .fns = sum)) %>%
    select(-Comb_FeedPerc)
  
  # These are now reported as dry weight

  names(Feed_use_sens_ingr)[names(Feed_use_sens_ingr) == "Ingredient"] <- "feed_material"
  names(Feed_use_sens_farm_ingr)[names(Feed_use_sens_farm_ingr) == "Ingredient"] <- "feed_material"
  
   # Check to make sure the two first columns are equal in both dataframes
  identical(Feed_use_sens_ingr[,c(1,2)], Feed_use_sens_farm_ingr[,c(1,2)])
  # This is true so we can just sum the rows
  
  # Combine both industrial and farm made feed use
  Feed_use_sens_both <- bind_rows(Feed_use_sens_ingr, Feed_use_sens_farm_ingr) %>%
    group_by(Country, feed_material) %>%
    summarise_all(funs(sum), na.rm=T)
  
    # Merge with country codes
  countries = read.csv("data/countries.csv", check.names = F)
  colnames(Feed_use_sens_both)[1]="AreaName"
  #Remove China 41 to avoid double accounting
  countries <- countries[!countries$`Area Code`==41,]
  Feed_use_sens_both = merge(Feed_use_sens_both, countries[,c(1,2)], by="AreaName")
  
  Feed_use_sens_both=Feed_use_sens_both[,c(1,503,2,3:502)]
  colnames(Feed_use_sens_both)=c("Area", "Area Code", "feed_material", 1:500) 
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  show(time.taken)
  return(Feed_use_sens_both)
  
}

