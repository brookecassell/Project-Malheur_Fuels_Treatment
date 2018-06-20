#### Project: Optimizing placement of fuel treatments and accelerating prescribed fire 
#### ameliorates extreme weather-driven wildfires in western mixed-conifer forests by 
#### Brooke A. Cassell, Portland State University, Dissertation defended February 2018


#This script is to look at harvest maps and biomass harvested across reps
#Written by Brooke Cassell October 2017



w.dir <- "D:/LANDIS-II Runs/Treatments_Results_Oct9_2017/"


library(raster)
library(rasterVis) #this allows plotting a categorical variable in raster
library(rgdal)
#library(viridis)
library(dplyr)
library(vegan)
library(RColorBrewer)
library(wesanderson)

ecomap <- raster("D:/LANDIS-II Runs/Treatments_Results_Oct9_2017/BAU_Historical/replicate1/ecoregions4ha1.img") #read in the map of ecoregions to use as a mask
#plot(ecomap) #plot to make sure it looks right
ecomap[(ecomap ==0)] <- NA #change cells with value = 0 to NA. Since all active cells have an ecoregion value, this will give a mask for inactive cells


years <- c(10,20,30,40,50,60,70,80,90) #each decade
no_reps <- 10  # input nummber of replicates here
scenario <- c("BAU_Historical", "Opt_BAU_Historical", "RxFire3x_Historical", "Opt_RxFire3x_Historical")

#seq.cols <- cbind(1:10, 11:20, 21:30, 31:40, 41:50, 51:60, 61:70, 71:80, 81:90, 91:100) #Not needed for biomass version because it's decadal and not annual.

# I want to bring in all reps (1-10) for each harvest year (each decade starting with year 10) and cbind them so the structure
# is columns 1-10: Restoration year rep 1, year 10, 20, etc..., 

##setting up loop objects - This version is at decadal time steps, so I couldn't figure out how to create a matrix and then fill in. Instead I'm creating NULL items and then cbind'ing into them.
harvest.years <- NULL
harvest.reps <- NULL
harvest.maps <- list() #final list of all scenarios

##Looping through years, reps, and scenarios to create a list of all biomass maps (change to appropriate species code throughout)
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for (i in 1:no_reps){
  print(paste('rep=', i))
    for(k in years){
    print(paste('years=', k))
      map.harvest <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","harvest/", "biomass-removed-", k, ".img", sep=""), template=ecomap)
      map.df <- as.data.frame(map.harvest)
      harvest.years <- cbind(harvest.years, map.df[,1]) ## binding all years
    }
    harvest.reps<- cbind(harvest.reps, harvest.years) #binding reps across years
    harvest.years <- NULL #remove previous reps for next loop
  }
  harvest.maps[[Scenario]] <- harvest.reps
  harvest.reps <- NULL #remove previous scenario for next loop
  
}
rm(Scenario)
rm(i)
rm(k)

#Index out each scenario
BAU_Historical.harvest <- harvest.maps$BAU_Historical 
Opt_BAU_Historical.harvest <- harvest.maps$Opt_BAU_Historical
RxFire3x.harvest <- harvest.maps$RxFire3x
Opt_RxFire3x.harvest <- harvest.maps$Opt_RxFire3x

BAU.harvest.annual.sums <- as.data.frame(colSums(BAU_Historical.harvest, na.rm = T)) #add down columns for annual area burned
Opt_BAU.harvest.annual.sums <- as.data.frame(colSums(Opt_BAU_Historical.harvest, na.rm = T))
RxFire3x.harvest.annual.sums <- as.data.frame(colSums(RxFire3x.harvest, na.rm = T))
Opt_RxFire3x.harvest.annual.sums <- as.data.frame(colSums(Opt_RxFire3x.harvest, na.rm = T))

BAU.harvest.annual.sums$Scenario <- as.factor("BAU")
BAU.harvest.annual.sums$Year <- as.factor(rep(seq(2020,2100, by=10), times = 10)) #add year numbers 
BAU.harvest.annual.sums$Rep <- as.factor(rep(1:10, each=9))
colnames(BAU.harvest.annual.sums)[1] <- "Biomass"

Opt_BAU.harvest.annual.sums$Scenario <- as.factor("Opt_BAU")
Opt_BAU.harvest.annual.sums$Year <- as.factor(rep(seq(2020,2100, by=10), times = 10)) #add year numbers 
Opt_BAU.harvest.annual.sums$Rep <- as.factor(rep(1:10, each=9))
colnames(Opt_BAU.harvest.annual.sums)[1] <- "Biomass"

RxFire3x.harvest.annual.sums$Scenario <- as.factor("RxFire3x")
RxFire3x.harvest.annual.sums$Year <- as.factor(rep(seq(2020,2100, by=10), times = 10)) #add year numbers 
RxFire3x.harvest.annual.sums$Rep <- as.factor(rep(1:10, each=9))
colnames(RxFire3x.harvest.annual.sums)[1] <- "Biomass"

Opt_RxFire3x.harvest.annual.sums$Scenario <- as.factor("Opt_RxFire3x")
Opt_RxFire3x.harvest.annual.sums$Year <- as.factor(rep(seq(2020,2100, by=10), times = 10)) #add year numbers 
Opt_RxFire3x.harvest.annual.sums$Rep <- as.factor(rep(1:10, each=9))
colnames(Opt_RxFire3x.harvest.annual.sums)[1] <- "Biomass"

Annual_Harvest <- rbind(BAU.harvest.annual.sums, Opt_BAU.harvest.annual.sums, RxFire3x.harvest.annual.sums, Opt_RxFire3x.harvest.annual.sums)

write.csv(Annual_Harvest, "D:/LANDIS-II Runs/Treatments_Results_Oct9_2017/Biomass_Harvested/Biomass_Harvested_by_Rep.csv", row.names = F)


###
library(dplyr)


Mean_biomass_across_reps <- group_by(Annual_Harvest, Scenario) #Overall mean & sd
summarise_mean_biomass_across_reps <- summarise(Mean_biomass_across_reps, biomass_Mg = mean(Biomass), sd = sd(Biomass))
summarise_mean_biomass_across_reps 

group_by_scenario <- group_by(Annual_Harvest, Scenario, Rep, add = TRUE) #mean & sd for each rep
biomass_by_scenario <- summarise(group_by_scenario, Biomass_Mg = mean(Biomass), sd = sd(Biomass))
biomass_by_scenario <- as.data.frame(biomass_by_scenario)



par(mfrow=c(1,1), xpd = NA, cex.axis = 0.8)
boxplot(biomass_by_scenario$Biomass_Mg ~ biomass_by_scenario$Scenario, las=2,
        ylab = "Biomass Harvested (Mg)")
title(main = "Annual Biomass Harvested", line = 2)


##Plotting harvested biomass through time

library(dplyr)
library(ggplot2)

#Use group_by and summarise to calculate mean and 5th/95th quantiles of area burned for each year across reps

Annual_Harvest$Biomass_per_ha <- Annual_Harvest$Biomass

Biomass_Grouped <- group_by(Annual_Harvest, Scenario, Year, add = TRUE)
Biomass_Summary <- as.data.frame(summarise(Biomass_Grouped, mean_biomass = mean(Biomass), ninetyfifth = quantile(Biomass, probs=0.95), fifth = quantile(Biomass, probs=0.05)))


par(mfrow = c(1,1))

a <- ggplot(Biomass_Summary, aes(x = Year, y = mean_biomass/1000000, colour = Scenario, group = Scenario)) + 
  geom_line(size = 2) +
  geom_ribbon(aes(ymin = fifth/1000000, ymax = ninetyfifth/1000000, fill = Scenario), alpha = 0.3) +
  theme_classic() +
  scale_color_manual(values = c("green", "blue", "red", "brown")) +
  scale_fill_manual(values = c("green", "blue", "red", "brown")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(breaks=seq(2010, 2100, 10)) + ylab("Biomass Harvested (Millions of Mg)") 
  scale_y_continuous(breaks = seq(0,250,25))
 a

