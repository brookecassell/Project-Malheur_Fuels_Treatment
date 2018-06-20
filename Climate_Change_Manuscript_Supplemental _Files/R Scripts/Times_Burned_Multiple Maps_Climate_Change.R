#### Project: More widespread and severe wildfires under climate change lead to dramatic declines in 
#### high-elevation species in the dry mixed conifer forests of the inland western U.S. by 
#### Brooke A. Cassell, Portland State University, Dissertation defended February 2018


#### This script is calculates the number of times each cell burned for use in multivariate analysis.
# Cell resolution is 1600ha, and the same replicates are used as in multivariate analysis.
# Written by Brooke A. Cassell in December 2017


w.dir <- "Z:/ClimateChange_Results_Oct6_2017/" #specify folder where files are saved

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
seq.cols <- cbind(1,2,3,4,5,6,7,8,9,10) #create sequence to fill columns for each replicate. Each set should be equal to the number of years in your simulation. 

ecomap <- raster("Z:/ClimateChange_Results_Oct6_2017/Historical/replicate1/ecoregions4ha1.img") #read in the map of ecoregions to use as a mask
plot(ecomap) #plot to make sure it looks right
ecomap[(ecomap ==0)] <- NA #change cells with value = 0 to NA. Since all active cells have an ecoregion value, this will give a mask for inactive cells to fire maps

##setting up loop objects
burn.years <- matrix(ncol=90, nrow=435000) #ncol = number of years, nrow = numbr of cells in your landscape (active + inactive)
burn.reps <- NULL #blank matrix)
burn.maps <- list() #final list of all scenarios

##Looping through years, reps, and scenarios to create a list of all fire maps
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for (i in 9:no_reps){
    print(paste('rep=', i))
    for(k in 1:years){
      print(paste('years=', k))
      map.select <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","fire/", "severity-", k, ".img", sep="")) #read in raster file
      map.df <- as.data.frame(map.select) #change to dataframe
      map.df[(map.df < 2),] <- 0 ##Reclassifying inactive & unburned cells as 0
      map.df[(map.df >1),] <- 1 ##Reclassifying any cells w fire as 1
      map.matrix <- as.matrix(map.df)
      map.select <- raster(map.matrix, template = ecomap)
      map.mask <- mask(map.select, ecomap) #replace inactive cells with NA
      map.df <- as.data.frame(map.mask)
      burn.years[,k] <- map.df[,1] ## binding all years
    }
    burn.sum <- as.data.frame(rowSums(burn.years, na.rm = T))
    burn.matrix <- as.matrix(burn.sum)
    burn.raster <- raster(burn.matrix, template = ecomap)
    map.resample <- aggregate(burn.raster, fact = 20, fun = mean, expand = F, na.rm = T) #resample to 400 ha cells because of RAM limitations
    map.df <- as.data.frame(map.resample)
    #replicate <- rep(i, nrow(burn.sum))         # create a vector of the length of your input file to record the replicate using the rep (replicate) function
    #reps_data <- cbind(burn.sum, replicate)     # bind the replicate number to the data
    #scenar <- rep(Scenario, nrow(burn.sum))     # create a vector of the length of your input file to record the scenario using the rep (replicate) function
    #scen_data <- cbind(reps_data, Scenario)     # bind the scenario name to the data
    #burn.reps <- rbind(burn.reps,scen_data)      # binding years across reps
  }
 # burn.maps[[Scenario]] <- burn.reps #create stack of rasters
  #burn.reps <- NULL
}
rm(Scenario)
rm(i)
rm(k)

#hist.burns.df <- as.data.frame(map.df)
#RCP_4.5.burns.df <- as.data.frame(map.df)
RCP_8.5.burns.df <- map.df



all.burns.df <- as.data.frame(rbind(hist.burns.df, RCP_4.5.burns.df, RCP_8.5.burns.df))
colnames(all.burns.df)[1] <- "Burns"
all.burns.df$Scenario <- rep(c("Historical", "RCP_4.5", "RCP_8.5"), each = 1080)


write.csv(all.burns.df, "Z:/ClimateChange_Results_Oct6_2017/Area_Burned_Data/times.burned.per.cell.1600ha.csv", row.names = F)
