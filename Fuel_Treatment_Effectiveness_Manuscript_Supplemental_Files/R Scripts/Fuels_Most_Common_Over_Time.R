#### Project: Optimizing placement of fuel treatments and accelerating prescribed fire 
#### ameliorates extreme weather-driven wildfires in western mixed-conifer forests by 
#### Brooke A. Cassell, Portland State University, Dissertation defended February 2018


#This script looks at the prevalence of different fuel types
#Written by Brooke Cassell October 2017

w.dir <- "W:/Results_July26_2017/"

library(raster)
library(rgdal)
library(viridis)

years <- 90
timesteps <- 1:90 # input number of timesteps (years in which fire maps are created) here
no_reps <- 10  # input nummber of replicates here
#sta <- stack()
#sta2<- stack()
scenario <- c("Historical", "RCP_4.5", "RCP_8.5")
#colnames(scenario) <- "Scenario"
seq.cols <- cbind(1:90, 91:180, 181:270, 271:360, 361:450, 451:540, 541:630, 631:720, 721:810, 811:900)

##setting up loop objects
fuel.years <- matrix(ncol=90, nrow=435000)
fuel.reps <- matrix(ncol=900, nrow=435000)
fuel.maps <- list() #final list of all scenarios

##Looping through years, reps, and scenarios to create a list of all fire maps
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for (i in 1:no_reps){
    print(paste('rep=', i))
    for(k in 1:years){
      print(paste('years=', k))
      map.select <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","DFFS-output/", "FuelType-", k, ".img", sep=""))
      map.df <- as.data.frame(map.select)
      map.df <- map.df-1 #subtract 1 to assign correct fuel type to all cells
      map.df[map.df < 1] <- 0 #make any inactive cells AND unnassigned fuels = 0 (can't have entire rows of NA for the next step)
      fuel.years[,k] <- map.df[,1] ## binding all years
    }
    fuel.reps[,seq.cols[,i]] <- fuel.years #binding years across reps
  }
  fuel.maps[[Scenario]] <- fuel.reps
  
}
rm(Scenario)
rm(i)
rm(k)

##calculating relevant fuels statistics from scenarios
hist.fuels <- fuel.maps$Historical #Index out just historical
RCP.4.5.fuels <- fuel.maps$RCP_4.5 #Index out just RCP_4.5
RCP.8.5.fuels <- fuel.maps$RCP_8.5 #Index out just RCP_8.5

hist.freq.fuels <- matrix(nrow = nrow(hist.fuels), ncol=1)
  for (j in 1:nrow(hist.fuels)){
    print(paste(j))
    most.freq <- as.numeric(names(which.max(table(hist.fuels[j,])))) 
    hist.freq.fuels[j,] <- most.freq
  }

rm(j)

RCP_4.5.freq.fuels <- matrix(nrow = nrow(RCP.4.5.fuels), ncol=1)
for (j in 1:nrow(RCP.4.5.fuels)){
  print(paste(j))
  most.freq <- as.numeric(names(which.max(table(RCP.4.5.fuels[j,])))) 
  RCP_4.5.freq.fuels[j,] <- most.freq
}

rm(j)

RCP_8.5.freq.fuels <- matrix(nrow = nrow(RCP.8.5.fuels), ncol=1)
for (j in 1:nrow(RCP.8.5.fuels)){
  print(paste(j))
  most.freq <- as.numeric(names(which.max(table(RCP.8.5.fuels[j,])))) 
  RCP_8.5.freq.fuels[j,] <- most.freq
}

rm(j)

#class(hist.freq.fuels) <- "numeric"
#hist.freq.fuels[hist.freq.fuels == 555] <- 0
hist.freq.fuels[hist.freq.fuels == 0] <- NA
hist.fuels.map <- raster(hist.freq.fuels, template = map.select)

RCP_4.5.freq.fuels[RCP_4.5.freq.fuels == 555] <- NA
RCP_4.5.fuels.map <- raster(RCP_4.5.freq.fuels, template = map.select)

RCP_8.5.freq.fuels[RCP_8.5.freq.fuels == 555] <- NA
RCP_8.5.fuels.map <- raster(RCP_8.5.freq.fuels, template = map.select)


par(mfrow = c(1,3))
plot(hist.fuels.map)
plot(RCP_4.5.fuels.map)
plot(RCP_8.5.fuels.map)

plot(hist(hist.fuels.map, breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,20,31,32,33), main = "Historical Fuels")) #plot a histogram of raster values
plot(hist(RCP_4.5.fuels.map, breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,20,31,32,33),main = "RCP_4.5 Fuels")) #plot a histogram of raster values
plot(hist(RCP_8.5.fuels.map, breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,20,31,32,33), main = "RCP_8.5 Fuels")) #plot a histogram of raster values

#Look at fuels other than ponderosa pine

hist.fuels.noPIPO <- hist.fuels.map
hist.fuels.noPIPO[hist.fuels.noPIPO == 6] <- NA
plot(hist(hist.fuels.noPIPO, freq = FALSE))

RCP_4.5.fuels.noPIPO <- RCP_4.5.fuels.map
RCP_4.5.fuels.noPIPO[RCP_4.5.fuels.noPIPO == 6] <- NA
plot(hist(RCP_4.5.fuels.noPIPO))

RCP_8.5.fuels.noPIPO <- RCP_8.5.fuels.map
RCP_8.5.fuels.noPIPO[RCP_8.5.fuels.noPIPO == 6] <- NA
plot(hist(RCP_8.5.fuels.noPIPO))
