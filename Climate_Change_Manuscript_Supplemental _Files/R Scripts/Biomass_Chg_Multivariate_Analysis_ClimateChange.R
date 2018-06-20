#### Project: More widespread and severe wildfires under climate change lead to dramatic declines in 
#### high-elevation species in the dry mixed conifer forests of the inland western U.S. by 
#### Brooke A. Cassell, Portland State University, Dissertation defended February 2018

### Script for multivariate analysis on biomass change (several species) of one replicate, selected randomly, for each climate scenario. The contemporary weather scenario was selected randomly;
# the RCP 4.5 GCM CESM1-BGC (warm/wet) was selected, and the RCP 8.5 GCM MIROC-ESM-CHEM (hot/dry) was Due to data processing constraints, this analysis
# uses 1600-ha resolution of one replicate. Written by Brooke Cassell, January 2018.

### This script reads in biomass maps for years 0 and 90 for TotalBiomass and each conifer species (does not include aspen or mountain mahogany, as their biomass is very low)

#Calculates change in biomass and extent over time for the historical/climate change scenarios

#Uses PERMANOVA to link biomass change with abiotic variables (# of burns, precipitation, temperature,
#slope, elevation, and soil type).


w.dir <- "Z:/ClimateChange_Results_Oct6_2017/"


library(raster)
library(rgdal)
#library(viridis)
library(dplyr)
library(vegan)

ecomap <- raster("Z:/ClimateChange_Results_Oct6_2017/Historical/replicate1/ecoregions4ha1.img") #read in the map of ecoregions to use as a mask
#plot(ecomap) #plot to make sure it looks right
ecomap[(ecomap ==0)] <- NA #change cells with value = 0 to NA. Since all active cells have an ecoregion value, this will give a mask for inactive cells

climatemap <- raster("Z:/climate_regions.img") #map with just the 5 climate regions
plot(climatemap)
climatemap1600ha <- aggregate(climatemap, fact = 20, fun = modal, expand = F, na.rm = T)
plot(climatemap1600ha)
climate.df <- as.data.frame(climatemap1600ha)

## Read each scenario's temp & precip data for each climate region

hist.temp <- read.csv("Z:/ClimateChange_Results_Oct6_2017/Climate_Data/Historical_Mean_Summer_Temps.csv", header = T, strip.white = T)
hist.precip <- read.csv("Z:/ClimateChange_Results_Oct6_2017/Climate_Data/Historical_Mean_Summer_Precip.csv", header = T, strip.white = T)
hist.temp.df <- climate.df
hist.temp.df[]<- hist.temp$Max_Summer_Temp[match(unlist(climate.df), hist.temp$Climate)]
colnames(hist.temp.df)[1] <- "temp"
hist.precip.df <- climate.df
hist.precip.df[]<- hist.precip$Mean_Summer_Precip[match(unlist(climate.df), hist.precip$Climate)]
colnames(hist.precip.df)[1] <- "precip"


RCP_4.5.temp <- read.csv("Z:/ClimateChange_Results_Oct6_2017/Climate_Data/RCP_4.5_4_Mean_Summer_Temp.csv", header = T, strip.white = T)
RCP_4.5.precip <- read.csv("Z:/ClimateChange_Results_Oct6_2017/Climate_Data/RCP_4.5_4_Mean_Summer_Precip.csv", header = T, strip.white = T)
RCP_4.5.temp.df <- climate.df
RCP_4.5.temp.df[]<- RCP_4.5.temp$Max_Summer_Temp[match(unlist(climate.df), RCP_4.5.temp$Climate)]
colnames(RCP_4.5.temp.df)[1] <- "temp"
RCP_4.5.precip.df <- climate.df
RCP_4.5.precip.df[]<- RCP_4.5.precip$Mean_Summer_Precip[match(unlist(climate.df), RCP_4.5.precip$Climate)]
colnames(RCP_4.5.precip.df)[1] <- "precip"


RCP_8.5.temp <- read.csv("Z:/ClimateChange_Results_Oct6_2017/Climate_Data/RCP_8.5_9_Mean_Summer_Temp.csv", header = T, strip.white = T)
RCP_8.5.precip <- read.csv("Z:/ClimateChange_Results_Oct6_2017/Climate_Data/RCP_8.5_9_Mean_Summer_Precip.csv", header = T, strip.white = T)
RCP_8.5.temp.df <- climate.df
RCP_8.5.temp.df[]<- RCP_8.5.temp$Max_Summer_Temp[match(unlist(climate.df), RCP_8.5.temp$Climate)]
colnames(RCP_8.5.temp.df)[1] <- "temp"
RCP_8.5.precip.df <- climate.df
RCP_8.5.precip.df[]<- RCP_8.5.precip$Summer_Mean_Precip[match(unlist(climate.df), RCP_8.5.precip$Climate)]
colnames(RCP_8.5.precip.df)[1] <- "precip"

#rbind climate data for all 3 scenarios
precip.df <- as.data.frame(rbind(hist.precip.df, RCP_4.5.precip.df, RCP_8.5.precip.df))

temp.df <- as.data.frame(rbind(hist.temp.df, RCP_4.5.temp.df, RCP_8.5.temp.df))


#Import elevation, aspect and slope data:
dem <- raster("Z:/elev_map.img") #read in elevation map
dem1600ha <- aggregate(dem, fact = 20, fun = mean, expand = F, na.rm = T)
dem.df <- as.data.frame(dem1600ha)
plot(dem1600ha)
#aspect <- raster("Z:/ClimateChange_Results_Oct6_2017/Historical/replicate1/azimuthmap.img")
#aspect64ha <- aggregate(aspect, fact = 4, fun = mean, expand = F, na.rm = T)
#aspect.df <- as.data.frame(aspect)
slope <- raster("Z:/ClimateChange_Results_Oct6_2017/Historical/replicate1/slopemap.img")
slope1600ha <- aggregate(slope, fact = 20, fun = max, expand = F, na.rm = T)
slope.df <- as.data.frame(slope1600ha)
plot(slope1600ha)
soils <- raster("Z:/soil_type.img")
soils1600ha <- aggregate(soils, fact = 20, fun = modal, expand = F, na.rm = T)
soils.df <- as.data.frame(soils1600ha)
plot(soils1600ha)


#Transform data as follows: dem & slope by z-scores - then cbind dem, slope, soils. 
dem.df2 <- scale(dem.df, center = TRUE, scale = TRUE) #set to z-scores by centering and scaling

slope.df2 <- scale(slope.df, center = TRUE, scale = TRUE)

independent <- cbind(dem.df2, slope.df2, soils.df)

#then repeat 3 times since these are the same for all scenarios
independent1 <- rbind(independent, independent, independent)

#Standardize, then cbind precip, temp & times burned for each scenario, 

precip.df2 <- as.data.frame(scale(precip.df, center = T, scale = T))
temp.df2 <- as.data.frame(scale(temp.df, center = T, scale = T))


fire <- read.csv("Z:/ClimateChange_Results_Oct6_2017/Area_Burned_Data/times.burned.per.cell.1600ha.csv", header = T, strip.white=T)
#This dataframe has the scenarios.

fire.df2 <- as.data.frame(scale(fire[,1], center = T, scale = T))
colnames(fire.df2)[1]<- "Burns"

#cbind into one

independent2 <- as.data.frame(cbind(fire[,2], fire.df2, precip.df2, temp.df2, slope.df2, dem.df2, soils.df))
colnames(independent2) <- c("Scenario", "Burns", "Precip", "Temp", "Slope", "Elevation", "Soil")
independent2$Soil <- as.factor(independent2$Soil)
independent.df <- na.omit(independent2)



cor.matrix(independent.df)

#high correlations are scenario with precip (R^2=-0.64) and with temp (0.95), and between temp & precip (-0.74)

############################## Total Biomass ##########################################

years <- c(0,90) #to look just at start & end years
#timesteps <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90) # input number of timesteps (years in which biomass maps are created) here
no_reps <- 10  # input nummber of replicates here
scenario <- c("Historical", "RCP_4.5", "RCP_8.5")
#scenario <- "Historical"
#seq.cols <- cbind(1:10, 11:20, 21:30, 31:40, 41:50, 51:60, 61:70, 71:80, 81:90, 91:100) #Not needed for biomass version because it's decadal and not annual.


##setting up loop objects - This version is at decadal time steps, so I couldn't figure out how to create a matrix and then fill in. Instead I'm creating NULL items and then cbind'ing into them.
AGB.years <- NULL
AGB.reps <- NULL
AGB.maps <- list() #final list of all scenarios

##Looping through years, reps, and scenarios to create a list of all biomass maps (change to appropriate species code throughout)
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for (i in 1:no_reps){
    print(paste('rep=', i))
    #for(k in years){
      #print(paste('years=', k))
      map.AGB0 <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/", "TotalBiomass-0", ".img", sep=""), template=ecomap)
      map.AGB90 <-raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/", "TotalBiomass-90", ".img", sep=""), template=ecomap)
      map.AGB <- map.AGB90 - map.AGB0 #Subtract year-0 from year-90 to give the change in total biomass for each cell on the landscape
      map.matrix.AGB <- as.matrix(map.AGB)
      map.raster.AGB <- raster(map.matrix.AGB, template=ecomap)
      map.na.AGB <- mask(map.raster.AGB, ecomap) #use NAs from "ecomap" to assign NA values instead of 0s (which can be inactive or 0 biomass)
      map.df.AGB <- as.data.frame(map.na.AGB)
      AGB.years <- cbind(AGB.years, map.df.AGB[,1]) ## binding all years
    #}
    AGB.reps<- cbind(AGB.reps, AGB.years) #binding years across reps
    AGB.years <- NULL #remove previous scenario for next loop
  }
  AGB.maps[[Scenario]] <- AGB.reps
  AGB.reps <- NULL #remove previous scenario for next loop
  
}
rm(Scenario)
rm(i)
#rm(k)


Historical_biomass <- as.data.frame(AGB.maps$Historical) #Index out each scenario
RCP_4.5_biomass <- as.data.frame(AGB.maps$RCP_4.5)
RCP_8.5_biomass <- as.data.frame(AGB.maps$RCP_8.5)

#Map mean change across reps for each scenario
#set up mapping color palette
library(RColorBrewer)
bg <- brewer.pal(20, "BrBG")

par(mfrow=c(1,3))
hist.cell.means <- as.matrix(rowMeans(Historical_biomass, na.rm = TRUE))
hist.map <- raster(hist.cell.means, template = ecomap)
plot(hist.map, col = bg, box = F, axes = F, legend = F)

RCP_4.5.cell.means <- as.matrix(rowMeans(RCP_4.5_biomass, na.rm = TRUE))
RCP_4.5.map <- raster(RCP_4.5.cell.means, template = ecomap)
plot(RCP_4.5.map, col = bg, box = F, axes = F, legend = F)

RCP_8.5.cell.means <- as.matrix(rowMeans(RCP_8.5_biomass, na.rm = TRUE))
RCP_8.5.map <- raster(RCP_8.5.cell.means, template = ecomap)
plot(RCP_8.5.map, col = bg, box = F, axes = F, legend = F)

#Calculate landscape mean total biomass for each rep

hist.mean.bio <- as.data.frame(colMeans(Historical_biomass, na.rm = T))
colnames(hist.mean.bio) <- "change_Mg_ha"
hist.mean.bio$scenario <- as.factor("Historical")
hist.mean.bio$rep <- as.factor(1:10)

RCP_4.5.mean.bio <- as.data.frame(colMeans(RCP_4.5_biomass, na.rm = T))
colnames(RCP_4.5.mean.bio) <- "change_Mg_ha"
RCP_4.5.mean.bio$scenario <- as.factor("RCP_4.5")
RCP_4.5.mean.bio$rep <- as.factor(1:10)

RCP_8.5.mean.bio <- as.data.frame(colMeans(RCP_8.5_biomass, na.rm = T))
colnames(RCP_8.5.mean.bio) <- "change_Mg_ha"
RCP_8.5.mean.bio$scenario <- as.factor("RCP_8.5")
RCP_8.5.mean.bio$rep <- as.factor(1:10)

Change_in_Biomass <- rbind(hist.mean.bio, RCP_4.5.mean.bio, RCP_8.5.mean.bio)

grouped_biomass <- group_by(Change_in_Biomass, scenario)
summary_biomass <- summarise(grouped_biomass, mean = mean(change_Mg_ha), sd = sd(change_Mg_ha))


shapiro.test(grouped_biomass$change_Mg_ha) #W = 0.96, p = 0.31, data are normally distributed

#Test for homoscedasticity of the log-transformed data
bartlett.test(grouped_biomass$change_Mg_ha ~ grouped_biomass$scenario)# K^2 = 2.43 #p = 0.30, equal variances

#Run an ANOVA
a <- aov(grouped_biomass$change_Mg_ha ~ grouped_biomass$scenario) #df = 2, F = 0.016, p = 0.984 - fail to reject H0
summary(a)




############## Individual Species Biomass #########################################


species <- c("abiegran", "abielasi", "juniocci", "lariocci", "piceenge", "pinualbi", "pinucont", "pinupond", "pseumenz")
years <- c(0,90) #to look just at start & end years
no_reps <- 10  # input nummber of replicates here
scenario <- c("Historical", "RCP_4.5", "RCP_8.5")

#To bring in just the reps I'll be analysing in the subset:

scenario <- "RCP_8.5"
no_reps <- 9 #indicate the replicate # here: hist #9, RCP_4.5 #4 and RCP_8.5 #9 (hottest/driest))
species <- c("abiegran", "abielasi", "juniocci", "lariocci", "piceenge", "pinualbi", "pinucont", "pinupond", "pseumenz")
years <- c(0,90) #to look just at start & end years

#if bringing in all reps, all scenarios, use these settings
#no_reps <- 10  # input nummber of replicates here
#scenario <- c("Historical", "RCP_4.5", "RCP_8.5")

species <- c("abiegran", "abielasi", "juniocci", "lariocci", "piceenge", "pinualbi", "pinucont", "pinupond", "pseumenz")
years <- c(0,90) #to look just at start & end years
no_reps <- 10  # input nummber of replicates here
scenario <- c("Historical", "RCP_4.5", "RCP_8.5")

AGB.years <- NULL
AGB.species <- NULL
AGB.reps <- NULL
AGB.scenarios <- NULL

##Looping through years, reps, and scenarios to create a list of all biomass maps (change to appropriate species code throughout)
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for (i in 9:no_reps){
    print(paste('rep=', i))
    for(k in species){
      print(paste('species=', k))
      map.AGB0 <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/", k, "-0", ".img", sep=""), template=ecomap)
      map.AGB90 <-raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/", k, "-90", ".img", sep=""), template=ecomap)
      map.AGB <- map.AGB90 - map.AGB0 #Subtract year-0 from year-90 to give the change in total biomass for each cell on the landscape
      map.matrix.AGB <- as.matrix(map.AGB)
      map.raster.AGB <- raster(map.matrix.AGB, template=ecomap)
      map.na.AGB <- mask(map.raster.AGB, ecomap) #use NAs from "ecomap" to assign NA values instead of 0s (which can be inactive or 0 biomass)
      map.resample <- aggregate(map.na.AGB, fact = 20, fun = mean, expand = F, na.rm = T) #resample to 1600 ha cells because of RAM limitations
      map.df.AGB <- as.data.frame(map.resample)
      AGB.species <- cbind(AGB.species, map.df.AGB[,1]) ## binding all species - should have 9 columns
    }
    AGB.reps <- rbind(AGB.reps, AGB.species) # binding years across reps
    AGB.species <- NULL
  }
  AGB.scenarios <- rbind(AGB.scenarios, AGB.reps)
  AGB.reps <- NULL
}
rm(Scenario)
rm(i)
rm(k)

AGB.hist <- AGB.scenarios #convert after 1st run
AGB.4.5 <- AGB.scenarios #convert after 2nd run
AGB.8.5 <- AGB.scenarios #convert after 3rd run

AGB.scenarios <- rbind(AGB.hist, AGB.4.5, AGB.8.5)

write.csv(AGB.scenarios, "Z:/ClimateChange_Results_Oct6_2017/Biomass_Data/All_Species_Reps_Scenarios_Chg.1600ha.csv", row.names = F)

######## Create Bray-Curtis Dissimilarity Matrix on relativized data ####################

#decostand and vegdist cannot handle negative values, so I need to add to make each column's min 0
AGB.scenarios <- as.data.frame(AGB.scenarios)

#remembering that columns are in this order: "abiegran", "abielasi"
#, "juniocci", "lariocci", "piceenge", "pinualbi", "pinucont", "pinupond", "pseumenz"
AGB.scenarios$abiegran <- AGB.scenarios[,1]-min(AGB.scenarios[,1], na.rm = T)
AGB.scenarios$abielasi <- AGB.scenarios[,2]-min(AGB.scenarios[,2], na.rm = T)
AGB.scenarios$juniocci <- AGB.scenarios[,3]-min(AGB.scenarios[,3], na.rm = T)
AGB.scenarios$lariocci <- AGB.scenarios[,4]-min(AGB.scenarios[,4], na.rm = T)
AGB.scenarios$piceenge <- AGB.scenarios[,5]-min(AGB.scenarios[,5], na.rm = T)
AGB.scenarios$pinualbi <- AGB.scenarios[,6]-min(AGB.scenarios[,6], na.rm = T)
AGB.scenarios$pinucont <- AGB.scenarios[,7]-min(AGB.scenarios[,7], na.rm = T)
AGB.scenarios$pinupond <- AGB.scenarios[,8]-min(AGB.scenarios[,8], na.rm = T)
AGB.scenarios$pseumenz <- AGB.scenarios[,9]-min(AGB.scenarios[,9], na.rm = T)
AGB.species <- as.data.frame(AGB.scenarios[,10:18])


##Vegdist cannot handle millions of rows of data, so I'm going to look at a randomly selected
# historical replicate and the 2 most extreme climate change scenarios.

#this is Historical replicate #9 & #5, RCP_4.5 replicates #4 (CESM1-BGC) and #8 (inmcm4) and RCP_8.5 reps #7 (IPSL-CM5A-LR) & #9 (MIROC_ESM_CHEM)
 ##(also didn't work - too large. I'll try 1 rep of each - hist #9, RCP_4.5 #4 and RCP_8.5 #9 (hottest/driest))
#AGB.total <- as.data.frame(AGB.species[c(217201:244350, 352951:380100,760201:787350),])

Total.Matrix <- as.data.frame(cbind(AGB.species, independent2))
write.csv(Total.Matrix, "Z:/ClimateChange_Results_Oct6_2017/Multivariate_Data/Total_Matrix.1600ha.csv", row.names = F)

Total.nona <- na.omit(Total.Matrix)

AGB.nona <- as.data.frame(Total.nona[,1:9])
independent.df <- as.data.frame(Total.nona[,10:16])

AGB.relativized <- decostand(AGB.nona, method = "range", na.rm = T)
AGB.relativized <- decostand(AGB.relativized, method = "total", na.rm = T)
#Bray_Dissimilar<- vegdist(AGB.relativized, method = "bray") #This will be done automatically during NMDS

#If needed, separate scenarios
#AGB.hist <- as.data.frame(AGB.species[c(217201:244350),])
#AGB.4.5 <- as.data.frame(AGB.species[c(352951:380100),])
#AGB.8.5 <- as.data.frame(AGB.species[c(760201:787350),])


#Relativize by column range and by row total
#AGB.hist.nona <- na.omit(AGB.hist) #remove rows with NA (inactive cells)
#AGB.hist.relativized <- decostand(AGB.hist.nona, method = "range", na.rm = T)
#AGB.hist.relativized <- decostand(AGB.hist.relativized, method = "total", na.rm = T)

#Bray_Dissimilar.hist <- vegdist(AGB.hist.relativized, method = "bray")

#AGB.4.5.nona <- na.omit(AGB.4.5) #remove rows with NA (inactive cells)
#AGB.4.5.relativized <- decostand(AGB.4.5.nona, method = "range", na.rm = T)
#AGB.4.5.relativized <- decostand(AGB.4.5.relativized, method = "total", na.rm = T)

#Bray_Dissimilar.4.5 <- vegdist(AGB.4.5.relativized, method = "bray")

#AGB.8.5.nona <- na.omit(AGB.8.5) #remove rows with NA (inactive cells)
#AGB.8.5.relativized <- decostand(AGB.8.5.nona, method = "range", na.rm = T)
#AGB.8.5.relativized <- decostand(AGB.8.5.relativized, method = "total", na.rm = T)

#Bray_Dissimilar.8.5 <- vegdist(AGB.8.5.relativized, method = "bray")

#Now I have my 3 dissimilarity matrixes, one for each, and I have them all in one matrix as well.


####################### On to the NMDS to visualize ################################
 #My dependent matrix is AGB.relativized
 #My independent dataframe is independent.df

#Sites 312 and 2472 are far outliers. I will remove them and re-analyze.

Total.Matrix <- Total.Matrix[-c(312,2472),]
Total.nona <- na.omit(Total.Matrix)

#Make sure to have the dataframes with outliers removed

write.csv(AGB.relativized, "Z:/ClimateChange_Results_Oct6_2017/Multivariate_Data/Species_Sites_Matrix.1600ha.csv", row.names = F)
write.csv(independent.df, "Z:/ClimateChange_Results_Oct6_2017/Multivariate_Data/Explanatory_Matrix.1600ha.csv", row.names = F )

AGB.relativized <- read.csv("Z:/ClimateChange_Results_Oct6_2017/Multivariate_Data/Species_Sites_Matrix.csv", header = T)
#AGB.RCP_8.5 <- AGB.relativized[c(33863:50973),]

#2-dimensional solution
z <- metaMDS(comm = AGB.relativized, autotransform = F, engine = "monoMDS", distance = "bray", k = 2, weakties = T,
             model = "global", maxit = 100, trymax = 20, trace = 1, pc = T, scaling = T,
             wascores = T, expand = T, parallel=8)
#3-dimensional solution
x <- metaMDS(comm = AGB.relativized, autotransform = F, engine = "monoMDS", distance = "bray", k = 3, weakties = T,
             model = "global", maxit = 1000, trymax =100, trace = 1, pc = T, scaling = T,
             wascores = T, expand = T, parallel=8, stratmax = 0.9999999)

#basic plot
plot(x, display = "sites", type = "n", xaxt = "n", xlab = "", yaxt = "n", ylab = "")
text(x)



AGB.nona <- as.data.frame(Total.nona[,1:9])
independent.df <- as.data.frame(Total.nona[,10:16])

x <- metaMDS(comm = AGB.relativized, autotransform = F, engine = "monoMDS", distance = "bray", k = 3, weakties = T,
             model = "global", maxit = 1000, trymax =100, trace = 1, pc = T, scaling = T,
             wascores = T, expand = T, parallel=8, stratmax = 0.9999999)

plot(x, display = "sites", type = "n", xaxt = "n", xlab = "", yaxt = "n", ylab = "")
points(x)

stressplot(object = x, title = "Shepard Plot", main = "Shepard Plot", sub = "Stress = 0.109")
title(main = "Shepard Plot")
goodness <- goodness(x)
plot(x, display = "sites", type = "n", xaxt = "n", xlab = "", yaxt = "n", ylab = "")
points(x, "sites")

exp.fit3 <- envfit(x ~ Burns*Elevation*Slope,independent.df, perm = 999) #create vectors of the 
#significant explanatory variables

plot(exp.fit3)

plot(x, display = "sites", type = "n", xaxt = "n", xlab = "", yaxt = "n", ylab = "")
ordihull(x, independent.df$Scenario, col = c("blue", "orange", "red"))
ordispider(x, independent.df$Scenario, col = c("blue", "orange", "red"))

points(x$points[independent.df$Scenario == "Historical",],pch = 0, col = "blue")
points(x$points[independent.df$Scenario == "RCP_4.5",],pch = 2, col = "orange")
points(x$points[independent.df$Scenario == "RCP_8.5",],pch = 4, col = "red")
legend(x = "topleft", pch = c(0,2,4), col = c("blue", "orange", "red"), legend = c("Historical", "RCP 4.5", "RCP 8.5"))

color <- c("blue", "orange", "red")

plot(x, display = "sites", type = "n", xaxt = "n", xlab = "", yaxt = "n", ylab = "", choices = c(1,2))
plot(exp.fit3, bg = rgb(1,1,1, 0.7), add = T, col = "black")


plot(x, display = "sites", type = "n", xaxt = "n", xlab = "", yaxt = "n", ylab = "", choices = c(1,2))
ordihull(x, independent.df$Scenario, col = c("blue", "orange", "red"), lwd = 2)
ordispider(x, independent.df$Scenario, col = c("blue", "orange", "red"))


colors <- c(colors()[128],colors()[81], colors()[30], colors()[91], colors()[41],colors()[39])

legend("topleft", legend = c("Whitebark Pine", "Sub-Alpine Fir", 
                             "Engelmann Spruce"), col = colors, pch = c(1,1,1), bty="n") 
par(mfrow=c(1,1), xpd = F, cex.axis = 0.8)
plot(x, display = "sites", type = "n", xaxt = "n", xlab = "", yaxt = "n", ylab = "", bty = "n")
ordisurf(x, independent.df$Burns, add = T, col = "black", lwd = 2)
ordisurf(x, dem.df3[,1], add = T, col = "black", lwd = 2)
points(x, cex = .1/(AGB.relativized$pinualbi +.01), col = colors()[128], pch = 1)
points(x, cex = .1/(AGB.relativized$abielasi +.01), col = colors()[81], pch = 1)
points(x, cex = .1/(AGB.relativized$piceenge +.01), col = colors()[30], pch = 1)
plot(exp.fit3, bg = rgb(1,1,1, 0.7), add = T, col = "black")
legend("topright", legend = c("Whitebark Pine", "Sub-Alpine Fir", "Engelmann Spruce",
                             "Ponderosa Pine", "Grand Fir", "Douglas Fir"),
                             col = c("turquoise", "green", "blue", "orange", "red", "pink"), pch = c(1,1,1,2,2,2), bty="n")


points(x, cex = 15*AGB.relativized$pinupond, col = colors()[91], pch = 2)
points(x, cex = 15*AGB.relativized$abiegran, col = colors()[41], pch = 2)
points(x, cex = 15*AGB.relativized$pseumenz, col = colors()[39], pch = 2)


##Plot species in ordination space

plot(x, display = "sites", type = "n",  xlab = "", ylab = "", bty = "n") #to get full plot axes to use in ordikplot
orditkplot(x, display = "species", xlim = c(-0.6, 1.0), ylim = c(-0.2, 0.4)) #allows manually moving of labels to avoid overlap
plot(species_labels)
ordihull(x, independent.df$Scenario, col = c("blue", "orange", "red"), lwd = 2) #add hulls for climate scenarios
legend("right", legend = c("Historical", "RCP 4.5", "RCP 8.5")
       , col = c("Blue", "Orange", "Red"), lty = 1, lwd = 2, bty = "n")

############### PERMANOVA ANALYSIS ##############
## both datasets have outliers removed and are at 1600-ha resolution

species <- read.csv("W:/ClimateChange_Results_Oct6_2017/Multivariate_Data/Species_Sites_Matrix.1600ha.csv", header = T)
env <- read.csv("W:/ClimateChange_Results_Oct6_2017/Multivariate_Data/Explanatory_Matrix.1600ha.csv", header = T)


#all terms, Scenario as blocking term
perm2 <- adonis(species ~ Burns*Precip*Temp*Slope*Elevation*Soil, data = env, permutations = 999, method = "bray", parallel = 16, strata = env$Scenario)

perm3 <- adonis(species ~ Burns/Scenario*Elevation/Scenario*Slope*Precip*Temp , data = env, permutations = 999, method = "bray", parallel = 16, strata = env$Scenario)

perm4 <- adonis(species ~ Scenario/Burns*Elevation*Slope, data = env, permutations = 999, method = "bray", parallel = 8, strata = env$Scenario)

perm5 <- adonis(species ~ Scenario + Burns*Elevation*Slope, data = env, permutations = 999, method = "bray", parallel = 8, strata = env$Scenario)

perm6 <- adonis(species ~ Burns*Elevation*Slope, data = env, permutations = 999, method = "bray", parallel = 8, strata = env$Scenario)
