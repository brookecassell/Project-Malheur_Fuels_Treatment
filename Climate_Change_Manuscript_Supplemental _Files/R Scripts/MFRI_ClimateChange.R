#### Project: More widespread and severe wildfires under climate change lead to dramatic declines in 
#### high-elevation species in the dry mixed conifer forests of the inland western U.S. by 
#### Brooke A. Cassell, Portland State University, Dissertation defended February 2018


#### This script is for calculating the frequency of fire and the probability of fire in each cell on the landscape 
# Written by Brooke A. Cassell in December 2017


w.dir <- "W:/ClimateChange_Results_Oct6_2017/" #specify folder where files are saved

#File structure should be Parent_Folder/Scenario_Folder/replicate#
#For example: W:/ClimateChange_Results_Oct6_2017/Historical/replicate1

#load pertinant packages

library(raster)
library(rgdal)
library(viridis)

years <- 90 #number of years in simulations
timesteps <- 1:90 # input number of timesteps (years in which fire maps are created) here
no_reps <- 10  # input nummber of replicates here
scenario <- c("Historical", "RCP_4.5", "RCP_8.5") #This should be the same as the names of the main subfolders for each scenario
#colnames(scenario) <- "Scenario"
seq.cols <- cbind(1:90, 91:180, 181:270, 271:360, 361:450, 451:540, 541:630, 631:720, 721:810, 811:900) #create sequence to fill columns for each replicate. Each set should be equal to the number of years in your simulation. 

ecomap <- raster("W:/ClimateChange_Results_Oct6_2017/Historical/replicate1/ecoregions4ha1.img") #read in the map of ecoregions to use as a mask
plot(ecomap) #plot to make sure it looks right
ecomap[(ecomap ==0)] <- NA #change cells with value = 0 to NA. Since all active cells have an ecoregion value, this will give a mask for inactive cells to fire maps

##setting up loop objects
burn.years <- matrix(ncol=90, nrow=435000) #ncol = number of years, nrow = numbr of cells in your landscape (active + inactive)
burn.reps <- matrix(ncol=900, nrow=435000) #ncol = number of years x reps)
burn.maps <- list() #final list of all scenarios

##Looping through years, reps, and scenarios to create a list of all fire maps
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for (i in 1:no_reps){
    print(paste('rep=', i))
    for(k in 1:years){
      print(paste('years=', k))
      map.select <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","fire/", "severity-", k, ".img", sep="")) #read in raster file
      map.df <- as.data.frame(map.select) #change to dataframe
      map.df[(map.df < 2),] <- 0 ##Reclassifying 0 and 1 as 0 (unburned or inactive)
      map.df[(map.df >1),] <- 1 ##Recalssifying any cells w fire as 1
      burn.years[,k] <- map.df[,1] ## binding all years
    }
    burn.reps[,seq.cols[,i]] <- burn.years #binding years across reps
  }
  burn.maps[[Scenario]] <- burn.reps #create stack of rasters
  
}
rm(Scenario)
rm(i)
rm(k)

##calculating relevant fire statistics from scenarios

hist.burn <- rowSums(burn.maps$Historical) #index out just Historical and calculate the cumulative number of burns per site across all reps and years.
sum(hist.burn)
hist.burn <- as.matrix(hist.burn) #make a matrix so that it can be converted back into a raster
hist.raster <- raster(hist.burn, template=ecomap) #use ecoregion map to set projection, extent & resolution
hist.raster.na <- mask(hist.raster, ecomap) #Set inactive cells to NA using the ecoregion map as a mask
hist.burn.na <- as.data.frame(hist.raster.na) #create dataframe for quantitative analysis
table(hist.burn.na) #table of cumulative burns per site
writeRaster(hist.raster.na, "W:/ClimateChange_Results_Oct6_2017/Historical_Cumulative_Burned_Cells.img", format = "HFA", overwrite = TRUE)

#Repeat for climate change scenarios
RCP_4.5.burn <- rowSums(burn.maps$RCP_4.5)
sum(RCP_4.5.burn)
RCP_4.5.burn <- as.matrix(RCP_4.5.burn)
RCP_4.5.raster <- raster(RCP_4.5.burn, template=ecomap)
RCP_4.5.raster.na <- mask(RCP_4.5.raster, ecomap)
RCP_4.5.burn.na <- as.data.frame(RCP_4.5.raster.na)
table(RCP_4.5.burn.na) #table of cumulative burns per site
writeRaster(RCP_4.5.raster.na, "W:/ClimateChange_Results_Oct6_2017/RCP_4.5_Cumulative_Burned_Cells.img", format = "HFA", overwrite = TRUE)


RCP_8.5.burn <- rowSums(burn.maps$RCP_8.5)
sum(RCP_8.5.burn)
RCP_8.5.burn <- as.matrix(RCP_8.5.burn)
RCP_8.5.raster <- raster(RCP_8.5.burn, template=ecomap)
RCP_8.5.raster.na <- mask(RCP_8.5.raster, ecomap)
RCP_8.5.burn.na <- as.data.frame(RCP_8.5.raster.na)
table(RCP_8.5.burn.na) #table of cumulative burns per site
writeRaster(RCP_8.5.raster.na, "W:/ClimateChange_Results_Oct6_2017/RCP_8.5_Cumulative_Burned_Cells.img", format = "HFA", overwrite = TRUE)



###### plot fire frequency #######

par(mfrow=c(1,3), mar = c(1,1,1,1), oma = c(0,0,0,1), xpd = NA)

plot(hist.raster.na, col = inferno(10, begin = 0, end = 1, direction = -1),zlim = c(0,40), box = F, axes = F, legend = F)
title("Historical", line = -5)
plot(RCP_4.5.raster.na, col = inferno(10, begin = 0, end = 1, direction = -1),zlim = c(0,40), box = F, axes = F, legend = F)
title("RCP_4.5", line = -5)
plot(RCP_8.5.raster.na, col = inferno(10, begin = 0, end = 1, direction = -1),zlim = c(0,40), box = F, axes = F)
title("RCP_8.5", line = -5)

title(main = "Fire Frequency", outer = TRUE, line = -3, cex.main = 2.0)


### Plot these as probabilities of burning instead of frequency ###


hist.prob <- hist.raster.na/900 ##calculate return interval (90 years x 10 replicates = 900 simulation-years)
sapply(as.data.frame(hist.prob), FUN = mean, na.rm = T) #What is the mean probability that any given cell will burn in any given year?
table(as.data.frame(hist.prob))
RCP_4.5.prob <- RCP_4.5.raster.na/900
sapply(as.data.frame(RCP_4.5.prob), FUN = mean, na.rm = T)
table(as.data.frame(RCP_4.5.prob))
RCP_8.5.prob <- RCP_8.5.raster.na/900
sapply(as.data.frame(RCP_8.5.prob), FUN = mean, na.rm = T)
table(as.data.frame(RCP_8.5.prob))


par(mfrow=c(1,3), mar = c(1,1,1,1), oma = c(0,0,0,1), xpd = NA)
plot(hist.prob, col = inferno(10, begin = 0, end = 1, direction = -1),zlim = c(0,.042), box = F, axes = F, legend = F)
title("Historical", line = -5)
plot(RCP_4.5.prob, col = inferno(10, begin = 0, end = 1, direction = -1),zlim = c(0,.042), box = F, axes = F, legend = F)
title("RCP_4.5", line = -5)
plot(RCP_8.5.prob, col = inferno(10, begin = 0, end = 1, direction = -1),zlim = c(0,.042), box = F, axes = F)

title("RCP_8.5", line = -5)


title(main = "Probability of Fire", outer = TRUE, line = -3, cex.main = 2.0)
