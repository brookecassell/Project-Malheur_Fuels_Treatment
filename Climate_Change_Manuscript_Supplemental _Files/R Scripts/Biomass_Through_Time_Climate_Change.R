#### Project: More widespread and severe wildfires under climate change lead to dramatic declines in 
#### high-elevation species in the dry mixed conifer forests of the inland western U.S. by 
#### Brooke A. Cassell, Portland State University, Dissertation defended February 2018


#### This script is for calculating annual biomass, averaged across reps, for graphing through time 
# Written by Brooke A. Cassell in December 2017

w.dir <- "W:/ClimateChange_Results_Oct6_2017/"

library(raster)
library(rgdal)
library(viridis)
library(readr)


years <- 90
timesteps <- c(0,10,20,30,40,50,60,70,80,90) # input number of timesteps (years in which fire maps are created) here
no_reps <- 10  # input nummber of replicates here
scenario <- c("Historical", "RCP_4.5", "RCP_8.5")


#seq.cols <- cbind(1:10, 11:20, 21:30, 31:40, 41:50, 51:60, 61:70, 71:80, 81:90, 91:100) #create sequence to fill columns for each replicate. Each set should be equal to the number of years in your simulation. 

ecomap <- raster("W:/ClimateChange_Results_Oct6_2017/Historical/replicate1/ecoregions4ha1.img") #read in the map of ecoregions to use as a mask
plot(ecomap) #plot to make sure it looks right
ecomap[(ecomap ==0)] <- NA #change cells with value = 0 to NA. Since all active cells have an ecoregion value, this will give a mask for inactive cells to fire maps

ICmap <- raster("W:/Treatments_Results_Oct9_2017/BAU_Historical/replicate1/mapcodes4ha1.img") #Read in Initial Communities map so forested cells can be identified & overlaid
plot(ICmap)
forestmap <- ICmap
forestmap[(forestmap < 100)] <- NA
plot(forestmap)
##setting up loop objects
#AGB.years <- matrix(ncol=10, nrow=435000)
AGB.years <- NULL
#AGB.reps <- matrix(ncol=100, nrow=435000)
AGB.reps <- NULL
AGB.maps <- list() #final list of all scenarios

##Looping through years, reps, and scenarios to create a list of all fire maps
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for (i in 1:no_reps){
    print(paste('rep=', i))
    for(k in timesteps){
      print(paste('years=', k))
      map.select <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/TotalBiomass-", k, ".img", sep=""))
      map.matrix <- as.matrix(map.select)
      map.select <- raster(map.matrix, template = ecomap)
      map.mask <- mask(map.select, ecomap)
      map.df <- as.data.frame(map.mask)
      AGB.years <- cbind(AGB.years,map.df[,1]) ## binding all years
    }
    AGB.reps <- cbind(AGB.reps,AGB.years) #binding years across reps
    AGB.years <- NULL
  }
  AGB.maps[[Scenario]] <- AGB.reps
  AGB.reps <- NULL
}
rm(Scenario)
rm(i)
rm(k)

hist.AGB <- AGB.maps$Historical
RCP_4.5.AGB <- AGB.maps$RCP_4.5
RCP_8.5.AGB <- AGB.maps$RCP_8.5 

hist.AGB.cell.means <- as.data.frame(colMeans(hist.AGB, na.rm = T)) #add down columns for annual area burned & multiply by 4 for ha
RCP_4.5.AGB.cell.means <- as.data.frame(colMeans(RCP_4.5.AGB, na.rm = T))
RCP_8.5.AGB.cell.means <- as.data.frame(colMeans(RCP_8.5.AGB, na.rm = T))

hist.AGB.cell.means$scenario <- as.factor("Historical")
hist.AGB.cell.means$year <- as.factor(rep(c(0,10,20,30,40,50,60,70,80,90), times = 10)) #add year numbers 
hist.AGB.cell.means$rep <- as.factor(rep(1:10, each=10))
colnames(hist.AGB.cell.means)[1] <- "Biomass"

RCP_4.5.AGB.cell.means$scenario <- as.factor("RCP_4.5")
RCP_4.5.AGB.cell.means$year <- as.factor(rep(c(0,10,20,30,40,50,60,70,80,90), times = 10)) #add year numbers 
RCP_4.5.AGB.cell.means$rep <- as.factor(rep(1:10, each=10))
colnames(RCP_4.5.AGB.cell.means)[1] <- "Biomass"

RCP_8.5.AGB.cell.means$scenario <- as.factor("RCP_8.5")
RCP_8.5.AGB.cell.means$year <- as.factor(rep(c(0,10,20,30,40,50,60,70,80,90), times = 10)) #add year numbers 
RCP_8.5.AGB.cell.means$rep <- as.factor(rep(1:10, each=10))
colnames(RCP_8.5.AGB.cell.means)[1] <- "Biomass"

Total_AGB <- rbind(hist.AGB.cell.means, RCP_4.5.AGB.cell.means, RCP_8.5.AGB.cell.means)

### Plot through time
Total_AGB$year <- as.factor(rep(c(2010,2020,2030,2040,2050,2060,2070,2080,2090,2100), times = 30)) #replace year # with simulation years


library(dplyr)
library(ggplot2)

#Use group_by and summarise to calculate mean and 5th/95th quantiles of area burned for each year across reps

AGB_Grouped <- group_by(Total_AGB, scenario, year, add = TRUE)
AGB_Summary <- as.data.frame(summarise(AGB_Grouped, MeanBiomass = mean(Biomass), ninetyfifth = quantile(Biomass, probs=0.95), fifth = quantile(Biomass, probs=0.05)))



par(mfrow = c(1,1))

a <- ggplot(AGB_Summary, aes(x = year, y = MeanBiomass, colour = scenario, group = scenario)) + 
  geom_line(size = 2) +
  geom_ribbon(aes(ymin = fifth, ymax = ninetyfifth, fill = scenario), alpha = 0.2) +
  #stat_smooth(aes(fill=scenario), method = "loess", alpha = 0.2)+
  theme_classic() +
  scale_color_manual(values = c("blue", "orange", "red")) +
  scale_fill_manual(values = c("blue", "orange", "red")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(breaks=seq(2010, 2100, 10)) + 
  ylab(bquote('Mean Biomass ('*g ~ m^-2*')')) +
  #ylab("Mean Biomass (g m-2)") +
  scale_y_continuous(limits = c(4000,6000)) +
  ggtitle("Total Biomass") +
  theme(plot.title = element_text(hjust = 0.5))
  

a



########### Ponderosa Pine Only



##setting up loop objects
#AGB.years <- matrix(ncol=10, nrow=435000)
PIPO.years <- NULL
#PIPO.reps <- matrix(ncol=100, nrow=435000)
PIPO.reps <- NULL
PIPO.maps <- list() #final list of all scenarios

##Looping through years, reps, and scenarios to create a list of all fire maps
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for (i in 1:no_reps){
    print(paste('rep=', i))
    for(k in timesteps){
      print(paste('years=', k))
      map.select <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/pinupond-", k, ".img", sep=""))
      map.matrix <- as.matrix(map.select)
      map.select <- raster(map.matrix, template = ecomap)
      map.mask <- mask(map.select, ecomap)
      map.df <- as.data.frame(map.mask)
      PIPO.years <- cbind(PIPO.years,map.df[,1]) ## binding all years
    }
    PIPO.reps <- cbind(PIPO.reps,PIPO.years) #binding years across reps
    PIPO.years <- NULL
  }
  PIPO.maps[[Scenario]] <- PIPO.reps
  PIPO.reps <- NULL
}
rm(Scenario)
rm(i)
rm(k)

hist.PIPO <- PIPO.maps$Historical
RCP_4.5.PIPO <- PIPO.maps$RCP_4.5
RCP_8.5.PIPO <- PIPO.maps$RCP_8.5 

hist.PIPO.cell.means <- as.data.frame(colMeans(hist.PIPO, na.rm = T)) #add down columns for annual area burned & multiply by 4 for ha
RCP_4.5.PIPO.cell.means <- as.data.frame(colMeans(RCP_4.5.PIPO, na.rm = T))
RCP_8.5.PIPO.cell.means <- as.data.frame(colMeans(RCP_8.5.PIPO, na.rm = T))

hist.PIPO.cell.means$scenario <- as.factor("Historical")
hist.PIPO.cell.means$year <- as.factor(rep(c(0,10,20,30,40,50,60,70,80,90), times = 10)) #add year numbers 
hist.PIPO.cell.means$rep <- as.factor(rep(1:10, each=10))
colnames(hist.PIPO.cell.means)[1] <- "Biomass"

RCP_4.5.PIPO.cell.means$scenario <- as.factor("RCP_4.5")
RCP_4.5.PIPO.cell.means$year <- as.factor(rep(c(0,10,20,30,40,50,60,70,80,90), times = 10)) #add year numbers 
RCP_4.5.PIPO.cell.means$rep <- as.factor(rep(1:10, each=10))
colnames(RCP_4.5.PIPO.cell.means)[1] <- "Biomass"

RCP_8.5.PIPO.cell.means$scenario <- as.factor("RCP_8.5")
RCP_8.5.PIPO.cell.means$year <- as.factor(rep(c(0,10,20,30,40,50,60,70,80,90), times = 10)) #add year numbers 
RCP_8.5.PIPO.cell.means$rep <- as.factor(rep(1:10, each=10))
colnames(RCP_8.5.PIPO.cell.means)[1] <- "Biomass"

Total_PIPO <- rbind(hist.PIPO.cell.means, RCP_4.5.PIPO.cell.means, RCP_8.5.PIPO.cell.means)

### Plot through time
Total_PIPO$year <- as.factor(rep(c(2010,2020,2030,2040,2050,2060,2070,2080,2090,2100), times = 30)) #replace year # with simulation years


library(dplyr)
library(ggplot2)

#Use group_by and summarise to calculate mean and 5th/95th quantiles of area burned for each year across reps

PIPO_Grouped <- group_by(Total_PIPO, scenario, year, add = TRUE)
PIPO_Summary <- as.data.frame(summarise(PIPO_Grouped, MeanBiomass = mean(Biomass), ninetyfifth = quantile(Biomass, probs=0.95), fifth = quantile(Biomass, probs=0.05)))



par(mfrow = c(1,1))

pp <- ggplot(PIPO_Summary, aes(x = year, y = MeanBiomass, colour = scenario, group = scenario)) + 
  geom_line(size = 2) +
  geom_ribbon(aes(ymin = fifth, ymax = ninetyfifth, fill = scenario), alpha = 0.2) +
  #stat_smooth(aes(fill=scenario), method = "loess", alpha = 0.2)+
  theme_classic() +
  scale_color_manual(values = c("blue", "orange", "red")) +
  scale_fill_manual(values = c("blue", "orange", "red")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(breaks=seq(2010, 2100, 10)) + 
  ylab(bquote('Mean Biomass ('*g ~ m^-2*')')) +
  #ylab("Mean Biomass (g m-2)") +
  scale_y_continuous(limits = c(2000,3000)) +
  ggtitle("Ponderosa Pine") +
  theme(plot.title = element_text(hjust = 0.5))

pp



########### Western Larch Only



##setting up loop objects
#AGB.years <- matrix(ncol=10, nrow=435000)
LAOC.years <- NULL
#LAOC.reps <- matrix(ncol=100, nrow=435000)
LAOC.reps <- NULL
LAOC.maps <- list() #final list of all scenarios

##Looping through years, reps, and scenarios to create a list of all fire maps
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for (i in 1:no_reps){
    print(paste('rep=', i))
    for(k in timesteps){
      print(paste('years=', k))
      map.select <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/lariocci-", k, ".img", sep=""))
      map.matrix <- as.matrix(map.select)
      map.select <- raster(map.matrix, template = ecomap)
      map.mask <- mask(map.select, ecomap)
      map.df <- as.data.frame(map.mask)
      LAOC.years <- cbind(LAOC.years,map.df[,1]) ## binding all years
    }
    LAOC.reps <- cbind(LAOC.reps,LAOC.years) #binding years across reps
    LAOC.years <- NULL
  }
  LAOC.maps[[Scenario]] <- LAOC.reps
  LAOC.reps <- NULL
}
rm(Scenario)
rm(i)
rm(k)

hist.LAOC <- LAOC.maps$Historical
RCP_4.5.LAOC <- LAOC.maps$RCP_4.5
RCP_8.5.LAOC <- LAOC.maps$RCP_8.5 

hist.LAOC.cell.means <- as.data.frame(colMeans(hist.LAOC, na.rm = T)) #add down columns for annual area burned & multiply by 4 for ha
RCP_4.5.LAOC.cell.means <- as.data.frame(colMeans(RCP_4.5.LAOC, na.rm = T))
RCP_8.5.LAOC.cell.means <- as.data.frame(colMeans(RCP_8.5.LAOC, na.rm = T))

hist.LAOC.cell.means$scenario <- as.factor("Historical")
hist.LAOC.cell.means$year <- as.factor(rep(c(0,10,20,30,40,50,60,70,80,90), times = 10)) #add year numbers 
hist.LAOC.cell.means$rep <- as.factor(rep(1:10, each=10))
colnames(hist.LAOC.cell.means)[1] <- "Biomass"

RCP_4.5.LAOC.cell.means$scenario <- as.factor("RCP_4.5")
RCP_4.5.LAOC.cell.means$year <- as.factor(rep(c(0,10,20,30,40,50,60,70,80,90), times = 10)) #add year numbers 
RCP_4.5.LAOC.cell.means$rep <- as.factor(rep(1:10, each=10))
colnames(RCP_4.5.LAOC.cell.means)[1] <- "Biomass"

RCP_8.5.LAOC.cell.means$scenario <- as.factor("RCP_8.5")
RCP_8.5.LAOC.cell.means$year <- as.factor(rep(c(0,10,20,30,40,50,60,70,80,90), times = 10)) #add year numbers 
RCP_8.5.LAOC.cell.means$rep <- as.factor(rep(1:10, each=10))
colnames(RCP_8.5.LAOC.cell.means)[1] <- "Biomass"

Total_LAOC <- rbind(hist.LAOC.cell.means, RCP_4.5.LAOC.cell.means, RCP_8.5.LAOC.cell.means)

### Plot through time
Total_LAOC$year <- as.factor(rep(c(2010,2020,2030,2040,2050,2060,2070,2080,2090,2100), times = 30)) #replace year # with simulation years


library(dplyr)
library(ggplot2)

#Use group_by and summarise to calculate mean and 5th/95th quantiles of area burned for each year across reps

LAOC_Grouped <- group_by(Total_LAOC, scenario, year, add = TRUE)
LAOC_Summary <- as.data.frame(summarise(LAOC_Grouped, MeanBiomass = mean(Biomass), ninetyfifth = quantile(Biomass, probs=0.95), fifth = quantile(Biomass, probs=0.05)))



par(mfrow = c(1,1))

lo <- ggplot(LAOC_Summary, aes(x = year, y = MeanBiomass, colour = scenario, group = scenario)) + 
  geom_line(size = 2) +
  geom_ribbon(aes(ymin = fifth, ymax = ninetyfifth, fill = scenario), alpha = 0.2) +
  #stat_smooth(aes(fill=scenario), method = "loess", alpha = 0.2)+
  theme_classic() +
  scale_color_manual(values = c("blue", "orange", "red")) +
  scale_fill_manual(values = c("blue", "orange", "red")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(breaks=seq(2010, 2100, 10)) + 
  ylab(bquote('Mean Biomass ('*g ~ m^-2*')')) +
  scale_y_continuous(limits = c(100,225)) +
  ggtitle("Western Larch") +
  theme(plot.title = element_text(hjust = 0.5))

lo

########### Douglas fir Only



##setting up loop objects
#AGB.years <- matrix(ncol=10, nrow=435000)
PSME.years <- NULL
#PSME.reps <- matrix(ncol=100, nrow=435000)
PSME.reps <- NULL
PSME.maps <- list() #final list of all scenarios

##Looping through years, reps, and scenarios to create a list of all fire maps
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for (i in 1:no_reps){
    print(paste('rep=', i))
    for(k in timesteps){
      print(paste('years=', k))
      map.select <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/pseumenz-", k, ".img", sep=""))
      map.matrix <- as.matrix(map.select)
      map.select <- raster(map.matrix, template = ecomap)
      map.mask <- mask(map.select, ecomap)
      map.df <- as.data.frame(map.mask)
      PSME.years <- cbind(PSME.years,map.df[,1]) ## binding all years
    }
    PSME.reps <- cbind(PSME.reps,PSME.years) #binding years across reps
    PSME.years <- NULL
  }
  PSME.maps[[Scenario]] <- PSME.reps
  PSME.reps <- NULL
}
rm(Scenario)
rm(i)
rm(k)

hist.PSME <- PSME.maps$Historical
RCP_4.5.PSME <- PSME.maps$RCP_4.5
RCP_8.5.PSME <- PSME.maps$RCP_8.5 

hist.PSME.cell.means <- as.data.frame(colMeans(hist.PSME, na.rm = T)) #add down columns for annual area burned & multiply by 4 for ha
RCP_4.5.PSME.cell.means <- as.data.frame(colMeans(RCP_4.5.PSME, na.rm = T))
RCP_8.5.PSME.cell.means <- as.data.frame(colMeans(RCP_8.5.PSME, na.rm = T))

hist.PSME.cell.means$scenario <- as.factor("Historical")
hist.PSME.cell.means$year <- as.factor(rep(c(0,10,20,30,40,50,60,70,80,90), times = 10)) #add year numbers 
hist.PSME.cell.means$rep <- as.factor(rep(1:10, each=10))
colnames(hist.PSME.cell.means)[1] <- "Biomass"

RCP_4.5.PSME.cell.means$scenario <- as.factor("RCP_4.5")
RCP_4.5.PSME.cell.means$year <- as.factor(rep(c(0,10,20,30,40,50,60,70,80,90), times = 10)) #add year numbers 
RCP_4.5.PSME.cell.means$rep <- as.factor(rep(1:10, each=10))
colnames(RCP_4.5.PSME.cell.means)[1] <- "Biomass"

RCP_8.5.PSME.cell.means$scenario <- as.factor("RCP_8.5")
RCP_8.5.PSME.cell.means$year <- as.factor(rep(c(0,10,20,30,40,50,60,70,80,90), times = 10)) #add year numbers 
RCP_8.5.PSME.cell.means$rep <- as.factor(rep(1:10, each=10))
colnames(RCP_8.5.PSME.cell.means)[1] <- "Biomass"

Total_PSME <- rbind(hist.PSME.cell.means, RCP_4.5.PSME.cell.means, RCP_8.5.PSME.cell.means)

### Plot through time
Total_PSME$year <- as.factor(rep(c(2010,2020,2030,2040,2050,2060,2070,2080,2090,2100), times = 30)) #replace year # with simulation years


library(dplyr)
library(ggplot2)

#Use group_by and summarise to calculate mean and 5th/95th quantiles of area burned for each year across reps

PSME_Grouped <- group_by(Total_PSME, scenario, year, add = TRUE)
PSME_Summary <- as.data.frame(summarise(PSME_Grouped, MeanBiomass = mean(Biomass), ninetyfifth = quantile(Biomass, probs=0.95), fifth = quantile(Biomass, probs=0.05)))



par(mfrow = c(1,1))

pm <- ggplot(PSME_Summary, aes(x = year, y = MeanBiomass, colour = scenario, group = scenario)) + 
  geom_line(size = 2) +
  geom_ribbon(aes(ymin = fifth, ymax = ninetyfifth, fill = scenario), alpha = 0.2) +
  #stat_smooth(aes(fill=scenario), method = "loess", alpha = 0.2)+
  theme_classic() +
  scale_color_manual(values = c("blue", "orange", "red")) +
  scale_fill_manual(values = c("blue", "orange", "red")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(breaks=seq(2010, 2100, 10)) + 
  ylab(bquote('Mean Biomass ('*g ~ m^-2*')')) +
  #ylab("Mean Biomass (g m-2)") +
  scale_y_continuous(limits = c(500,900)) +
  ggtitle("Douglas Fir") +
  theme(plot.title = element_text(hjust = 0.5))

pm

########### Western Juniper Only



##setting up loop objects
#AGB.years <- matrix(ncol=10, nrow=435000)
JUOC.years <- NULL
#JUOC.reps <- matrix(ncol=100, nrow=435000)
JUOC.reps <- NULL
JUOC.maps <- list() #final list of all scenarios

##Looping through years, reps, and scenarios to create a list of all fire maps
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for (i in 1:no_reps){
    print(paste('rep=', i))
    for(k in timesteps){
      print(paste('years=', k))
      map.select <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/juniocci-", k, ".img", sep=""))
      map.matrix <- as.matrix(map.select)
      map.select <- raster(map.matrix, template = ecomap)
      map.mask <- mask(map.select, ecomap)
      map.df <- as.data.frame(map.mask)
      JUOC.years <- cbind(JUOC.years,map.df[,1]) ## binding all years
    }
    JUOC.reps <- cbind(JUOC.reps,JUOC.years) #binding years across reps
    JUOC.years <- NULL
  }
  JUOC.maps[[Scenario]] <- JUOC.reps
  JUOC.reps <- NULL
}
rm(Scenario)
rm(i)
rm(k)

hist.JUOC <- JUOC.maps$Historical
RCP_4.5.JUOC <- JUOC.maps$RCP_4.5
RCP_8.5.JUOC <- JUOC.maps$RCP_8.5 

hist.JUOC.cell.means <- as.data.frame(colMeans(hist.JUOC, na.rm = T)) #add down columns for annual area burned & multiply by 4 for ha
RCP_4.5.JUOC.cell.means <- as.data.frame(colMeans(RCP_4.5.JUOC, na.rm = T))
RCP_8.5.JUOC.cell.means <- as.data.frame(colMeans(RCP_8.5.JUOC, na.rm = T))

hist.JUOC.cell.means$scenario <- as.factor("Historical")
hist.JUOC.cell.means$year <- as.factor(rep(c(0,10,20,30,40,50,60,70,80,90), times = 10)) #add year numbers 
hist.JUOC.cell.means$rep <- as.factor(rep(1:10, each=10))
colnames(hist.JUOC.cell.means)[1] <- "Biomass"

RCP_4.5.JUOC.cell.means$scenario <- as.factor("RCP_4.5")
RCP_4.5.JUOC.cell.means$year <- as.factor(rep(c(0,10,20,30,40,50,60,70,80,90), times = 10)) #add year numbers 
RCP_4.5.JUOC.cell.means$rep <- as.factor(rep(1:10, each=10))
colnames(RCP_4.5.JUOC.cell.means)[1] <- "Biomass"

RCP_8.5.JUOC.cell.means$scenario <- as.factor("RCP_8.5")
RCP_8.5.JUOC.cell.means$year <- as.factor(rep(c(0,10,20,30,40,50,60,70,80,90), times = 10)) #add year numbers 
RCP_8.5.JUOC.cell.means$rep <- as.factor(rep(1:10, each=10))
colnames(RCP_8.5.JUOC.cell.means)[1] <- "Biomass"

Total_JUOC <- rbind(hist.JUOC.cell.means, RCP_4.5.JUOC.cell.means, RCP_8.5.JUOC.cell.means)

### Plot through time
Total_JUOC$year <- as.factor(rep(c(2010,2020,2030,2040,2050,2060,2070,2080,2090,2100), times = 30)) #replace year # with simulation years


library(dplyr)
library(ggplot2)

#Use group_by and summarise to calculate mean and 5th/95th quantiles of area burned for each year across reps

JUOC_Grouped <- group_by(Total_JUOC, scenario, year, add = TRUE)
JUOC_Summary <- as.data.frame(summarise(JUOC_Grouped, MeanBiomass = mean(Biomass), ninetyfifth = quantile(Biomass, probs=0.95), fifth = quantile(Biomass, probs=0.05)))



par(mfrow = c(1,1))

jo <- ggplot(JUOC_Summary, aes(x = year, y = MeanBiomass, colour = scenario, group = scenario)) + 
  geom_line(size = 2) +
  geom_ribbon(aes(ymin = fifth, ymax = ninetyfifth, fill = scenario), alpha = 0.2) +
  #stat_smooth(aes(fill=scenario), method = "loess", alpha = 0.2)+
  theme_classic() +
  scale_color_manual(values = c("blue", "orange", "red")) +
  scale_fill_manual(values = c("blue", "orange", "red")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(breaks=seq(2010, 2100, 10)) + 
  ylab(bquote('Mean Biomass ('*g ~ m^-2*')')) +
  scale_y_continuous(limits = c(50,450)) +
  ggtitle("Western Juniper") +
  theme(plot.title = element_text(hjust = 0.5))

jo



################### Engelmann Spruce Only


PIEN.years <- NULL
#PIEN.reps <- matrix(ncol=100, nrow=435000)
PIEN.reps <- NULL
PIEN.maps <- list() #final list of all scenarios

##Looping through years, reps, and scenarios to create a list of all fire maps
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for (i in 1:no_reps){
    print(paste('rep=', i))
    for(k in timesteps){
      print(paste('years=', k))
      map.select <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/piceenge-", k, ".img", sep=""))
      map.matrix <- as.matrix(map.select)
      map.select <- raster(map.matrix, template = ecomap)
      map.mask <- mask(map.select, ecomap)
      map.df <- as.data.frame(map.mask)
      PIEN.years <- cbind(PIEN.years,map.df[,1]) ## binding all years
    }
    PIEN.reps <- cbind(PIEN.reps,PIEN.years) #binding years across reps
    PIEN.years <- NULL
  }
  PIEN.maps[[Scenario]] <- PIEN.reps
  PIEN.reps <- NULL
}
rm(Scenario)
rm(i)
rm(k)

hist.PIEN <- PIEN.maps$Historical
RCP_4.5.PIEN <- PIEN.maps$RCP_4.5
RCP_8.5.PIEN <- PIEN.maps$RCP_8.5 

hist.PIEN.cell.means <- as.data.frame(colMeans(hist.PIEN, na.rm = T)) #add down columns for annual area burned & multiply by 4 for ha
RCP_4.5.PIEN.cell.means <- as.data.frame(colMeans(RCP_4.5.PIEN, na.rm = T))
RCP_8.5.PIEN.cell.means <- as.data.frame(colMeans(RCP_8.5.PIEN, na.rm = T))

hist.PIEN.cell.means$scenario <- as.factor("Historical")
hist.PIEN.cell.means$year <- as.factor(rep(c(0,10,20,30,40,50,60,70,80,90), times = 10)) #add year numbers 
hist.PIEN.cell.means$rep <- as.factor(rep(1:10, each=10))
colnames(hist.PIEN.cell.means)[1] <- "Biomass"

RCP_4.5.PIEN.cell.means$scenario <- as.factor("RCP_4.5")
RCP_4.5.PIEN.cell.means$year <- as.factor(rep(c(0,10,20,30,40,50,60,70,80,90), times = 10)) #add year numbers 
RCP_4.5.PIEN.cell.means$rep <- as.factor(rep(1:10, each=10))
colnames(RCP_4.5.PIEN.cell.means)[1] <- "Biomass"

RCP_8.5.PIEN.cell.means$scenario <- as.factor("RCP_8.5")
RCP_8.5.PIEN.cell.means$year <- as.factor(rep(c(0,10,20,30,40,50,60,70,80,90), times = 10)) #add year numbers 
RCP_8.5.PIEN.cell.means$rep <- as.factor(rep(1:10, each=10))
colnames(RCP_8.5.PIEN.cell.means)[1] <- "Biomass"

Total_PIEN <- rbind(hist.PIEN.cell.means, RCP_4.5.PIEN.cell.means, RCP_8.5.PIEN.cell.means)

### Plot through time
Total_PIEN$year <- as.factor(rep(c(2010,2020,2030,2040,2050,2060,2070,2080,2090,2100), times = 30)) #replace year # with simulation years


library(dplyr)
library(ggplot2)

#Use group_by and summarise to calculate mean and 5th/95th quantiles of area burned for each year across reps

PIEN_Grouped <- group_by(Total_PIEN, scenario, year, add = TRUE)
PIEN_Summary <- as.data.frame(summarise(PIEN_Grouped, MeanBiomass = mean(Biomass), ninetyfifth = quantile(Biomass, probs=0.95), fifth = quantile(Biomass, probs=0.05)))



par(mfrow = c(1,1))

pe <- ggplot(PIEN_Summary, aes(x = year, y = MeanBiomass, colour = scenario, group = scenario)) + 
  geom_line(size = 2) +
  geom_ribbon(aes(ymin = fifth, ymax = ninetyfifth, fill = scenario), alpha = 0.2) +
  #stat_smooth(aes(fill=scenario), method = "loess", alpha = 0.2)+
  theme_classic() +
  scale_color_manual(values = c("blue", "orange", "red")) +
  scale_fill_manual(values = c("blue", "orange", "red")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(breaks=seq(2010, 2100, 10)) + 
  ylab(bquote('Mean Biomass ('*g ~ m^-2*')')) +
  scale_y_continuous(limits = c(0,50)) +
  ggtitle("Engelmann Spruce") +
  theme(plot.title = element_text(hjust = 0.5))


pe

################### Sub-Alpine Fir Only


ABLA.years <- NULL
#ABLA.reps <- matrix(ncol=100, nrow=435000)
ABLA.reps <- NULL
ABLA.maps <- list() #final list of all scenarios

##Looping through years, reps, and scenarios to create a list of all fire maps
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for (i in 1:no_reps){
    print(paste('rep=', i))
    for(k in timesteps){
      print(paste('years=', k))
      map.select <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/abielasi-", k, ".img", sep=""))
      map.matrix <- as.matrix(map.select)
      map.select <- raster(map.matrix, template = ecomap)
      map.mask <- mask(map.select, ecomap)
      map.df <- as.data.frame(map.mask)
      ABLA.years <- cbind(ABLA.years,map.df[,1]) ## binding all years
    }
    ABLA.reps <- cbind(ABLA.reps,ABLA.years) #binding years across reps
    ABLA.years <- NULL
  }
  ABLA.maps[[Scenario]] <- ABLA.reps
  ABLA.reps <- NULL
}
rm(Scenario)
rm(i)
rm(k)

hist.ABLA <- ABLA.maps$Historical
RCP_4.5.ABLA <- ABLA.maps$RCP_4.5
RCP_8.5.ABLA <- ABLA.maps$RCP_8.5 

hist.ABLA.cell.means <- as.data.frame(colMeans(hist.ABLA, na.rm = T)) #add down columns for annual area burned & multiply by 4 for ha
RCP_4.5.ABLA.cell.means <- as.data.frame(colMeans(RCP_4.5.ABLA, na.rm = T))
RCP_8.5.ABLA.cell.means <- as.data.frame(colMeans(RCP_8.5.ABLA, na.rm = T))

hist.ABLA.cell.means$scenario <- as.factor("Historical")
hist.ABLA.cell.means$year <- as.factor(rep(c(0,10,20,30,40,50,60,70,80,90), times = 10)) #add year numbers 
hist.ABLA.cell.means$rep <- as.factor(rep(1:10, each=10))
colnames(hist.ABLA.cell.means)[1] <- "Biomass"

RCP_4.5.ABLA.cell.means$scenario <- as.factor("RCP_4.5")
RCP_4.5.ABLA.cell.means$year <- as.factor(rep(c(0,10,20,30,40,50,60,70,80,90), times = 10)) #add year numbers 
RCP_4.5.ABLA.cell.means$rep <- as.factor(rep(1:10, each=10))
colnames(RCP_4.5.ABLA.cell.means)[1] <- "Biomass"

RCP_8.5.ABLA.cell.means$scenario <- as.factor("RCP_8.5")
RCP_8.5.ABLA.cell.means$year <- as.factor(rep(c(0,10,20,30,40,50,60,70,80,90), times = 10)) #add year numbers 
RCP_8.5.ABLA.cell.means$rep <- as.factor(rep(1:10, each=10))
colnames(RCP_8.5.ABLA.cell.means)[1] <- "Biomass"

Total_ABLA <- rbind(hist.ABLA.cell.means, RCP_4.5.ABLA.cell.means, RCP_8.5.ABLA.cell.means)

### Plot through time
Total_ABLA$year <- as.factor(rep(c(2010,2020,2030,2040,2050,2060,2070,2080,2090,2100), times = 30)) #replace year # with simulation years


library(dplyr)
library(ggplot2)

#Use group_by and summarise to calculate mean and 5th/95th quantiles of area burned for each year across reps

ABLA_Grouped <- group_by(Total_ABLA, scenario, year, add = TRUE)
ABLA_Summary <- as.data.frame(summarise(ABLA_Grouped, MeanBiomass = mean(Biomass), ninetyfifth = quantile(Biomass, probs=0.95), fifth = quantile(Biomass, probs=0.05)))



par(mfrow = c(1,1))

al <- ggplot(ABLA_Summary, aes(x = year, y = MeanBiomass, colour = scenario, group = scenario)) + 
  geom_line(size = 2) +
  geom_ribbon(aes(ymin = fifth, ymax = ninetyfifth, fill = scenario), alpha = 0.2) +
  #stat_smooth(aes(fill=scenario), method = "loess", alpha = 0.2)+
  theme_classic() +
  scale_color_manual(values = c("blue", "orange", "red")) +
  scale_fill_manual(values = c("blue", "orange", "red")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(breaks=seq(2010, 2100, 10)) + 
  ylab(bquote('Mean Biomass ('*g ~ m^-2*')')) +
  scale_y_continuous(limits = c(0,40)) +
  ggtitle("Sub-Alpine Fir") +
  theme(plot.title = element_text(hjust = 0.5))


al


################### Whitebark Pine Only


PIAL.years <- NULL
#PIAL.reps <- matrix(ncol=100, nrow=435000)
PIAL.reps <- NULL
PIAL.maps <- list() #final list of all scenarios

##Looping through years, reps, and scenarios to create a list of all fire maps
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for (i in 1:no_reps){
    print(paste('rep=', i))
    for(k in timesteps){
      print(paste('years=', k))
      map.select <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/pinualbi-", k, ".img", sep=""))
      map.matrix <- as.matrix(map.select)
      map.select <- raster(map.matrix, template = ecomap)
      map.mask <- mask(map.select, ecomap)
      map.df <- as.data.frame(map.mask)
      PIAL.years <- cbind(PIAL.years,map.df[,1]) ## binding all years
    }
    PIAL.reps <- cbind(PIAL.reps,PIAL.years) #binding years across reps
    PIAL.years <- NULL
  }
  PIAL.maps[[Scenario]] <- PIAL.reps
  PIAL.reps <- NULL
}
rm(Scenario)
rm(i)
rm(k)

hist.PIAL <- PIAL.maps$Historical
RCP_4.5.PIAL <- PIAL.maps$RCP_4.5
RCP_8.5.PIAL <- PIAL.maps$RCP_8.5 

hist.PIAL.cell.means <- as.data.frame(colMeans(hist.PIAL, na.rm = T)) #add down columns for annual area burned & multiply by 4 for ha
RCP_4.5.PIAL.cell.means <- as.data.frame(colMeans(RCP_4.5.PIAL, na.rm = T))
RCP_8.5.PIAL.cell.means <- as.data.frame(colMeans(RCP_8.5.PIAL, na.rm = T))

hist.PIAL.cell.means$scenario <- as.factor("Historical")
hist.PIAL.cell.means$year <- as.factor(rep(c(0,10,20,30,40,50,60,70,80,90), times = 10)) #add year numbers 
hist.PIAL.cell.means$rep <- as.factor(rep(1:10, each=10))
colnames(hist.PIAL.cell.means)[1] <- "Biomass"

RCP_4.5.PIAL.cell.means$scenario <- as.factor("RCP_4.5")
RCP_4.5.PIAL.cell.means$year <- as.factor(rep(c(0,10,20,30,40,50,60,70,80,90), times = 10)) #add year numbers 
RCP_4.5.PIAL.cell.means$rep <- as.factor(rep(1:10, each=10))
colnames(RCP_4.5.PIAL.cell.means)[1] <- "Biomass"

RCP_8.5.PIAL.cell.means$scenario <- as.factor("RCP_8.5")
RCP_8.5.PIAL.cell.means$year <- as.factor(rep(c(0,10,20,30,40,50,60,70,80,90), times = 10)) #add year numbers 
RCP_8.5.PIAL.cell.means$rep <- as.factor(rep(1:10, each=10))
colnames(RCP_8.5.PIAL.cell.means)[1] <- "Biomass"

Total_PIAL <- rbind(hist.PIAL.cell.means, RCP_4.5.PIAL.cell.means, RCP_8.5.PIAL.cell.means)

### Plot through time
Total_PIAL$year <- as.factor(rep(c(2010,2020,2030,2040,2050,2060,2070,2080,2090,2100), times = 30)) #replace year # with simulation years


library(dplyr)
library(ggplot2)

#Use group_by and summarise to calculate mean and 5th/95th quantiles of area burned for each year across reps

PIAL_Grouped <- group_by(Total_PIAL, scenario, year, add = TRUE)
PIAL_Summary <- as.data.frame(summarise(PIAL_Grouped, MeanBiomass = mean(Biomass), ninetyfifth = quantile(Biomass, probs=0.95), fifth = quantile(Biomass, probs=0.05)))



par(mfrow = c(1,1))

pa <- ggplot(PIAL_Summary, aes(x = year, y = MeanBiomass, colour = scenario, group = scenario)) + 
  geom_line(size = 2) +
  geom_ribbon(aes(ymin = fifth, ymax = ninetyfifth, fill = scenario), alpha = 0.2) +
  #stat_smooth(aes(fill=scenario), method = "loess", alpha = 0.2)+
  theme_classic() +
  scale_color_manual(values = c("blue", "orange", "red")) +
  scale_fill_manual(values = c("blue", "orange", "red")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(breaks=seq(2010, 2100, 10)) + 
  ylab(bquote('Mean Biomass ('*g ~ m^-2*')')) +
  scale_y_continuous(limits = c(0,6)) +
  ggtitle("Whitebark Pine") +
  theme(plot.title = element_text(hjust = 0.5))


pa

################### Lodgepole Pine Only


PICO.years <- NULL
#PICO.reps <- matrix(ncol=100, nrow=435000)
PICO.reps <- NULL
PICO.maps <- list() #final list of all scenarios

##Looping through years, reps, and scenarios to create a list of all fire maps
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for (i in 1:no_reps){
    print(paste('rep=', i))
    for(k in timesteps){
      print(paste('years=', k))
      map.select <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/pinucont-", k, ".img", sep=""))
      map.matrix <- as.matrix(map.select)
      map.select <- raster(map.matrix, template = ecomap)
      map.mask <- mask(map.select, ecomap)
      map.df <- as.data.frame(map.mask)
      PICO.years <- cbind(PICO.years,map.df[,1]) ## binding all years
    }
    PICO.reps <- cbind(PICO.reps,PICO.years) #binding years across reps
    PICO.years <- NULL
  }
  PICO.maps[[Scenario]] <- PICO.reps
  PICO.reps <- NULL
}
rm(Scenario)
rm(i)
rm(k)

hist.PICO <- PICO.maps$Historical
RCP_4.5.PICO <- PICO.maps$RCP_4.5
RCP_8.5.PICO <- PICO.maps$RCP_8.5 

hist.PICO.cell.means <- as.data.frame(colMeans(hist.PICO, na.rm = T)) #add down columns for annual area burned & multiply by 4 for ha
RCP_4.5.PICO.cell.means <- as.data.frame(colMeans(RCP_4.5.PICO, na.rm = T))
RCP_8.5.PICO.cell.means <- as.data.frame(colMeans(RCP_8.5.PICO, na.rm = T))

hist.PICO.cell.means$scenario <- as.factor("Historical")
hist.PICO.cell.means$year <- as.factor(rep(c(0,10,20,30,40,50,60,70,80,90), times = 10)) #add year numbers 
hist.PICO.cell.means$rep <- as.factor(rep(1:10, each=10))
colnames(hist.PICO.cell.means)[1] <- "Biomass"

RCP_4.5.PICO.cell.means$scenario <- as.factor("RCP_4.5")
RCP_4.5.PICO.cell.means$year <- as.factor(rep(c(0,10,20,30,40,50,60,70,80,90), times = 10)) #add year numbers 
RCP_4.5.PICO.cell.means$rep <- as.factor(rep(1:10, each=10))
colnames(RCP_4.5.PICO.cell.means)[1] <- "Biomass"

RCP_8.5.PICO.cell.means$scenario <- as.factor("RCP_8.5")
RCP_8.5.PICO.cell.means$year <- as.factor(rep(c(0,10,20,30,40,50,60,70,80,90), times = 10)) #add year numbers 
RCP_8.5.PICO.cell.means$rep <- as.factor(rep(1:10, each=10))
colnames(RCP_8.5.PICO.cell.means)[1] <- "Biomass"

Total_PICO <- rbind(hist.PICO.cell.means, RCP_4.5.PICO.cell.means, RCP_8.5.PICO.cell.means)

### Plot through time
Total_PICO$year <- as.factor(rep(c(2010,2020,2030,2040,2050,2060,2070,2080,2090,2100), times = 30)) #replace year # with simulation years


library(dplyr)
library(ggplot2)

#Use group_by and summarise to calculate mean and 5th/95th quantiles of area burned for each year across reps

PICO_Grouped <- group_by(Total_PICO, scenario, year, add = TRUE)
PICO_Summary <- as.data.frame(summarise(PICO_Grouped, MeanBiomass = mean(Biomass), ninetyfifth = quantile(Biomass, probs=0.95), fifth = quantile(Biomass, probs=0.05)))



par(mfrow = c(1,1))

pc <- ggplot(PICO_Summary, aes(x = year, y = MeanBiomass, colour = scenario, group = scenario)) + 
  geom_line(size = 2) +
  geom_ribbon(aes(ymin = fifth, ymax = ninetyfifth, fill = scenario), alpha = 0.2) +
  #stat_smooth(aes(fill=scenario), method = "loess", alpha = 0.2)+
  theme_classic() +
  scale_color_manual(values = c("blue", "orange", "red")) +
  scale_fill_manual(values = c("blue", "orange", "red")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(breaks=seq(2010, 2100, 10)) + 
  ylab(bquote('Mean Biomass ('*g ~ m^-2*')')) +
  scale_y_continuous(limits = c(0,300)) +
  ggtitle("Lodgepole Pine") +
  theme(plot.title = element_text(hjust = 0.5))


pc


################### Grand Fir Only


ABGR.years <- NULL
#ABGR.reps <- matrix(ncol=100, nrow=435000)
ABGR.reps <- NULL
ABGR.maps <- list() #final list of all scenarios

##Looping through years, reps, and scenarios to create a list of all fire maps
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for (i in 1:no_reps){
    print(paste('rep=', i))
    for(k in timesteps){
      print(paste('years=', k))
      map.select <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/abiegran-", k, ".img", sep=""))
      map.matrix <- as.matrix(map.select)
      map.select <- raster(map.matrix, template = ecomap)
      map.mask <- mask(map.select, ecomap)
      map.df <- as.data.frame(map.mask)
      ABGR.years <- cbind(ABGR.years,map.df[,1]) ## binding all years
    }
    ABGR.reps <- cbind(ABGR.reps,ABGR.years) #binding years across reps
    ABGR.years <- NULL
  }
  ABGR.maps[[Scenario]] <- ABGR.reps
  ABGR.reps <- NULL
}
rm(Scenario)
rm(i)
rm(k)

hist.ABGR <- ABGR.maps$Historical
RCP_4.5.ABGR <- ABGR.maps$RCP_4.5
RCP_8.5.ABGR <- ABGR.maps$RCP_8.5 

hist.ABGR.cell.means <- as.data.frame(colMeans(hist.ABGR, na.rm = T)) #add down columns for annual area burned & multiply by 4 for ha
RCP_4.5.ABGR.cell.means <- as.data.frame(colMeans(RCP_4.5.ABGR, na.rm = T))
RCP_8.5.ABGR.cell.means <- as.data.frame(colMeans(RCP_8.5.ABGR, na.rm = T))

hist.ABGR.cell.means$scenario <- as.factor("Historical")
hist.ABGR.cell.means$year <- as.factor(rep(c(0,10,20,30,40,50,60,70,80,90), times = 10)) #add year numbers 
hist.ABGR.cell.means$rep <- as.factor(rep(1:10, each=10))
colnames(hist.ABGR.cell.means)[1] <- "Biomass"

RCP_4.5.ABGR.cell.means$scenario <- as.factor("RCP_4.5")
RCP_4.5.ABGR.cell.means$year <- as.factor(rep(c(0,10,20,30,40,50,60,70,80,90), times = 10)) #add year numbers 
RCP_4.5.ABGR.cell.means$rep <- as.factor(rep(1:10, each=10))
colnames(RCP_4.5.ABGR.cell.means)[1] <- "Biomass"

RCP_8.5.ABGR.cell.means$scenario <- as.factor("RCP_8.5")
RCP_8.5.ABGR.cell.means$year <- as.factor(rep(c(0,10,20,30,40,50,60,70,80,90), times = 10)) #add year numbers 
RCP_8.5.ABGR.cell.means$rep <- as.factor(rep(1:10, each=10))
colnames(RCP_8.5.ABGR.cell.means)[1] <- "Biomass"

Total_ABGR <- rbind(hist.ABGR.cell.means, RCP_4.5.ABGR.cell.means, RCP_8.5.ABGR.cell.means)

### Plot through time
Total_ABGR$year <- as.factor(rep(c(2010,2020,2030,2040,2050,2060,2070,2080,2090,2100), times = 30)) #replace year # with simulation years


library(dplyr)
library(ggplot2)

#Use group_by and summarise to calculate mean and 5th/95th quantiles of area burned for each year across reps

ABGR_Grouped <- group_by(Total_ABGR, scenario, year, add = TRUE)
ABGR_Summary <- as.data.frame(summarise(ABGR_Grouped, MeanBiomass = mean(Biomass), ninetyfifth = quantile(Biomass, probs=0.95), fifth = quantile(Biomass, probs=0.05)))



par(mfrow = c(1,1))

ag <- ggplot(ABGR_Summary, aes(x = year, y = MeanBiomass, colour = scenario, group = scenario)) + 
  geom_line(size = 2) +
  geom_ribbon(aes(ymin = fifth, ymax = ninetyfifth, fill = scenario), alpha = 0.2) +
  #stat_smooth(aes(fill=scenario), method = "loess", alpha = 0.2)+
  theme_classic() +
  scale_color_manual(values = c("blue", "orange", "red")) +
  scale_fill_manual(values = c("blue", "orange", "red")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(breaks=seq(2010, 2100, 10)) + 
  ylab(bquote('Mean Biomass ('*g ~ m^-2*')')) +
  scale_y_continuous(limits = c(600,1500)) +
  ggtitle("Grand Fir") +
  theme(plot.title = element_text(hjust = 0.5))


ag