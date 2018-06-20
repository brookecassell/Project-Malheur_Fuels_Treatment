#### Project: Optimizing placement of fuel treatments and accelerating prescribed fire 
#### ameliorates extreme weather-driven wildfires in western mixed-conifer forests by 
#### Brooke A. Cassell, Portland State University, Dissertation defended February 2018

### This script creates optimized management and stand maps for use in LANDIS-II. These maps target the areas
# most likely to burn, as identified through replicates of the untreated landscape under extreme weather conditions
# and overlays them with the management areas that can legally be treated (excluding wilderness and roadless areas)

#Written by Brooke Cassell November 2017

w.dir <- "W:/Treatments_Results_Oct9_2017/"

library(raster)
library(rgdal)
library(viridis)

#Read in maps we'll need: ecoregions, initial communities, and management

ecomap <- raster("W:/Treatments_Results_Oct9_2017/Untreated_Extreme/replicate1/ecoregions4ha1.img") #read in the map of ecoregions to use as a mask
plot(ecomap) #plot to make sure it looks right
ecomap[(ecomap ==0)] <- NA #change cells with value = 0 to NA. Since all active cells have an ecoregion value, this will give a mask for inactive cells to fire maps

icmap <- raster("W:/Treatments_Results_Oct9_2017/Untreated_Extreme/replicate1/mapcodes4ha1.img") #Read in initial communities map
plot(icmap)
forest_area <- icmap
forest_area[forest_area < 100] <- NA
plot(forest_area)

management <- raster("W:/Treatments_Results_Oct9_2017/Untreated_Extreme/replicate1/management.img") #Read in Management map
plot(management)
                     
                     
#Set up loop to bring in all reps of the Untreated_Extreme scenario

years <- 100
timesteps <- 1:100 # input number of timesteps (years in which fire maps are created) here
no_reps <- 10  # input nummber of replicates here
scenario <- "Untreated_Extreme"

seq.cols <- cbind(1:100, 101:200, 201:300, 301:400, 401:500, 501:600, 601:700, 701:800, 801:900, 901:1000)

##setting up loop objects
burn.years <- matrix(ncol=100, nrow=435000)
burn.reps <- matrix(ncol=1000, nrow=435000)
burn.maps <- list() #final list of all scenarios

##Looping through years, reps, and scenarios to create a list of all fire maps
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for (i in 1:no_reps){
    print(paste('rep=', i))
    for(k in 1:years){
      print(paste('years=', k))
      map.select <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","fire/", "severity-", k, ".img", sep=""))
      map.df <- as.data.frame(map.select)
      burn.years[,k] <- map.df[,1] ## binding all years
    }
    burn.reps[,seq.cols[,i]] <- burn.years #binding years across reps
  }
  burn.maps[[Scenario]] <- burn.reps
  
}
rm(Scenario)
rm(i)
rm(k)


Untreated_Severity <- burn.maps$Untreated_Extreme #Create object
Untreated_Severity[Untreated_Severity == 0] <- NA #make inactive cells NA
Untreated_Severity[Untreated_Severity == 1] <- NA #make unburned cells NA
Untreated_Severity[Untreated_Severity == 2] <- NA #make burned but no mortality cells NA
Untreated_Severity[Untreated_Severity == 3] <- NA #make severity 1 cells NA
Untreated_Severity[Untreated_Severity == 4] <- NA #make severity 2 cells NA
Untreated_Severity[Untreated_Severity == 5] <- NA #make severity 3 cells NA
Untreated_Severity[Untreated_Severity == 6] <- 4  #adjust to reflect true severity value
Untreated_Severity[Untreated_Severity == 7] <- 5  #adjust to reflect true severity value

Untreated_ext.severity <- rowSums(Untreated_Severity, na.rm = T) #calculate sum for cumulative high severity values for each year & replicate
Untreated_ext.severity <- as.matrix(Untreated_ext.severity)
Untreated_Severity.raster <- raster(Untreated_ext.severity, template=ecomap)
plot(Untreated_Severity.raster)
summary(Untreated_ext.severity)

####Create a map of optimized treatment areas

Highest_Severity.raster <- Untreated_Severity.raster #create a copy of the raster
Highest_Severity.raster[Untreated_Severity.raster < 450] <- NA #keep only values >= 500, the sites most likely to burn at the highest severities
#This could range in probability of a cell burning in any given year over 1000 sim-years
# at severity 4 (12.5 - 17.5% chance of burning) to severity 5 (8 - 14% chance of burning).  
plot(Highest_Severity.raster)

Highest_Severity.raster[management == 1 | management == 4] <- NA #remove Forest Service - No Harvest-designated areas
#And take out Private Nonindustrial, because I'm going to let harvest in those areas stay as is. I'll add it back later.
plot(Highest_Severity.raster)

###Create the new management map

optimized_management_map <- mask(management, Highest_Severity.raster)
optimized_management_map[is.na(optimized_management_map)] <- 0 #replace NAs with 0 as per LANDIS-II requirements
plot(optimized_management_map)

#add private lands back into the management map so private landowners can also harvest
private <- management 
private[private != 4] <- 0

optimized_management <- optimized_management_map + private
plot(optimized_management)

writeRaster(optimized_management, "W:/Treatments_Results_Oct9_2017/Untreated_Extreme/replicate1/optimized_management.img", format = "HFA", datatype='INT2U', overwrite=TRUE) #must specify datatype integer, otherwise it will automatically save as type: double.

#calculate sizes of management areas

management_ha <- (table(as.data.frame(optimized_management)))*4; management_ha

#2: BLM - 5,796 ha, #3: Forest Service - 146,808, #6: ODF - 84, #7: Forest Service Riparian Areas - 12744
#Target harvest amounts/decade under BAU:
#BLM - 1,040ha - 18% 
#FS - PIPO, dry mixed & moist mixed & pre-commercial - 13,000 each - 9% each
#ODF - too small to worry about
#Riparian - 20% each of Rx Fire & harvest (50-year rotation)

### create a new stand map

stands <- raster("W:/Treatments_Results_Oct9_2017/Untreated_Extreme/replicate1/standmap.img")
plot(stands)
optimized_stands <- mask(stands, Highest_Severity.raster) 
optimized_stands[is.na(optimized_stands)] <- 0  #replace NAs with 0 as per LANDIS-II requirements
plot(optimized_stands)

#add stands back in for private landowners
management_mask <- management
management_mask[management !=4] <- NA
private_stands <- mask(stands, management_mask)
private_stands[is.na(private_stands)] <- 0

optimized_stands <- optimized_stands + private_stands
plot(optimized_stands)

writeRaster(optimized_stands, "W:/Treatments_Results_Oct9_2017/Untreated_Extreme/replicate1/optimized_stands.img", format = "HFA", datatype='INT2U', overwrite = T)
