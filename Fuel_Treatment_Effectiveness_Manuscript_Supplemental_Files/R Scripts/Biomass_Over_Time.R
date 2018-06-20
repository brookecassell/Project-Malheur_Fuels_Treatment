#### Project: Optimizing placement of fuel treatments and accelerating prescribed fire 
#### ameliorates extreme weather-driven wildfires in western mixed-conifer forests by 
#### Brooke A. Cassell, Portland State University, Dissertation defended February 2018


#This script looks at total biomass and biomass for each individual species.
#Written by Brooke Cassell September 2017

w.dir <- "W:/Results_Treatments_Sept25_2017/"

install.packages("raster")
library(raster)
install.packages("rgdal")
library(rgdal)
install.packages("viridis")
library(viridis)

years <- seq(from=0, to=100, by=10)
#timesteps <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90) # input number of timesteps (years in which fire maps are created) here
no_reps <- 10  # input nummber of replicates here
scenario <- c("BAU_Extreme", "BAU_Historical", "Riparian_Extreme", "Riparian_Historical", "RxFire_Extreme", "RxFire_Historical", "Untreated_Extreme", "Untreated_Historical")
seq.cols <- cbind(1:10, 11:20, 21:30, 31:40, 41:50, 51:60, 61:70, 71:80, 81:90, 91:100)

##setting up loop objects
AGB.years <- NULL
AGB.reps <- NULL
AGB.maps <- list() #final list of all scenarios

##Looping through years, reps, and scenarios to create a list of all biomass maps (change to appropriate species code throughout)
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for (i in 1:no_reps){
    print(paste('rep=', i))
    for(k in years){
      print(paste('years=', k))
      map.AGB <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/", "TotalBiomass-", k, ".img", sep=""))
      map.df.AGB <- as.data.frame(map.AGB)
      AGB.years <- cbind(AGB.years,map.df.AGB[,1]) ## binding all years
    }
    AGB.reps<- cbind(AGB.reps, AGB.years) #binding years across reps
  }
  AGB.maps[[Scenario]] <- AGB.reps
  
}
rm(Scenario)
rm(i)
rm(k)


gc() #to free up memory







########## Ponderosa Pine ####################

##setting up loop objects 
PIPO.years <- NULL
PIPO.reps <- NULL
PIPO.maps <- list() #final list of all scenarios

##Looping through years, reps, and scenarios to create a list of all biomass maps (change to appropriate species code throughout)
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for (i in 1:no_reps){
    print(paste('rep=', i))
    for(k in years){
      print(paste('years=', k))
      map.PIPO <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/", "pinupond-", k, ".img", sep=""))
      map.df.PIPO <- as.data.frame(map.PIPO)
      PIPO.years <- cbind(PIPO.years,map.df.PIPO[,1]) ## binding all years
    }
    PIPO.reps<- cbind(PIPO.reps, PIPO.years) #binding years across reps
  }
  PIPO.maps[[Scenario]] <- PIPO.reps
  
}
rm(Scenario)
rm(i)
rm(k)

#Calculate relevant statistics

#Calculate mean across cells INCLUDING zeros (instances where PIPO is NOT present)
hist.PIPO <- PIPO.maps$historical #Index out just RCP_4.5orical
hist.mean.PIPO <- rowMeans(hist.PIPO, na.rm = T) #calculate means within each cell across year & replicate
mean(hist.mean.PIPO, na.rm = T) #check to make sure value makes sense
hist.mean.PIPO <- as.matrix(hist.mean.PIPO)
hist.mean.PIPO_no0 <- as.matrix(hist.mean.PIPO_no0)
hist.PIPO.raster <- raster(hist.mean.PIPO, template=map.PIPO)
plot(hist.PIPO.raster)


RCP_4.5.PIPO <- PIPO.maps$RCP_4.5 #Index out just RCP_4.5
RCP_4.5.mean.PIPO <- rowMeans(RCP_4.5.PIPO, na.rm = T) #calculate means within each cell across year & replicate
mean(RCP_4.5.mean.PIPO, na.rm = T) #check to make sure value makes sense
RCP_4.5.mean.PIPO <- as.matrix(RCP_4.5.mean.PIPO)
RCP_4.5.PIPO.raster <- raster(RCP_4.5.mean.PIPO, template=map.PIPO)
plot(RCP_4.5.PIPO.raster)

RCP_8.5.PIPO <- PIPO.maps$RCP_8.5 #Index out just RCP_8.5
RCP_8.5.mean.PIPO <- rowMeans(RCP_8.5.PIPO, na.rm = T) #calculate means within each cell across year & replicate
mean(RCP_8.5.mean.PIPO, na.rm = T) #check to make sure value makes sense
RCP_8.5.mean.PIPO <- as.matrix(RCP_8.5.mean.PIPO)
RCP_8.5.PIPO.raster <- raster(RCP_8.5.mean.PIPO, template=map.PIPO)
plot(RCP_8.5.PIPO.raster)


par(mfrow=c(1,3))
plot(hist.PIPO.raster, col = rev(viridis(100)), zlim = c(0.1,10000), box = F, axes = F)
title("Historical", line = -3)
plot(RCP_4.5.PIPO.raster, col = rev(viridis(100)), zlim = c(0.1,10000), box = F, axes = F)
title("RCP_4.5", line = -3)
plot(RCP_8.5.PIPO.raster, col = rev(viridis(100)), zlim = c(0.1,10000), box = F, axes = F)
title("RCP_8.5", line = -3)
title(main = "Mean Ponderosa Pine Biomass", outer = TRUE, line = -3, cex.main = 2.0)


#make a version that only includes instances where PIPO is present

hist.PIPO_no0 <- hist.PIPO
hist.PIPO_no0[hist.PIPO_no0 == 0] <- NA
hist.mean.PIPO_no0 <- rowMeans(hist.mean.PIPO_no0, na.rm = T)
mean(hist.mean.PIPO_no0, na.rm = T)
hist.mean.PIPO_no0 <- as.matrix(hist.mean.PIPO_no0)
hist.PIPO_no0.raster <- raster(hist.mean.PIPO_no0, template = map.PIPO)


RCP_4.5.PIPO_no0 <- RCP_4.5.PIPO
RCP_4.5.PIPO_no0[RCP_4.5.PIPO_no0 == 0] <- NA
RCP_4.5.mean.PIPO_no0 <- rowMeans(RCP_4.5.PIPO_no0, na.rm = T)
mean(RCP_4.5.mean.PIPO_no0, na.rm = T)
RCP_4.5.mean.PIPO_no0 <- as.matrix (RCP_4.5.mean.PIPO_no0)
RCP_4.5.PIPO_no0.raster <- raster(RCP_4.5.mean.PIPO_no0, template = map.PIPO)

RCP_8.5.PIPO_no0 <- RCP_8.5.PIPO
RCP_8.5.PIPO_no0[RCP_8.5.PIPO_no0 == 0] <- NA
RCP_8.5.mean.PIPO_no0 <- rowMeans(RCP_8.5.PIPO_no0, na.rm = T)
mean(RCP_8.5.mean.PIPO_no0, na.rm = T)
RCP_8.5.mean.PIPO_no0 <- as.matrix (RCP_8.5.mean.PIPO_no0)
RCP_8.5.PIPO_no0.raster <- raster(RCP_8.5.mean.PIPO_no0, template = map.PIPO)


par(mfrow=c(1,3))
plot(hist.PIPO_no0.raster, col = rev(viridis(100)), zlim = c(0.1,10000), box = F, axes = F)
title("Historical", line = -3)
plot(RCP_4.5.PIPO_no0.raster, col = rev(viridis(100)), zlim = c(0.1,10000), box = F, axes = F)
title("RCP_4.5", line = -3)
plot(RCP_8.5.PIPO_no0.raster, col = rev(viridis(100)), zlim = c(0.1,10000), box = F, axes = F)
title("RCP_8.5", line = -3)
title(main = "Mean Ponderosa Pine Biomass Where it Occurs", outer = TRUE, line = -3, cex.main = 2.0)

#Plot standard deviations

par(mfrow=c(1,3))
hist.sd.PIPO <- as.matrix(apply(hist.PIPO, 1, sd)) #calculate sd within each cell across year & replicate
hist.sd.PIPO.raster <- raster(hist.sd.PIPO, template=map.PIPO)


RCP_4.5.sd.PIPO <- as.matrix(apply(RCP_4.5.PIPO, 1, sd)) #calculate sd within each cell across year & replicate
RCP_4.5.sd.PIPO.raster <- raster(RCP_4.5.sd.PIPO, template=map.PIPO)


RCP_8.5.sd.PIPO <- as.matrix(apply(RCP_8.5.PIPO, 1, sd)) #calculate sd within each cell across year & replicate
RCP_8.5.sd.PIPO.raster <- raster(RCP_8.5.sd.PIPO, template=map.PIPO)


par(mfrow=c(1,3))
plot(hist.sd.PIPO.raster, col = rev(viridis(4)), zlim = c(0.1,400), box = F, axes = F)
title("Historical", line = -3)
plot(RCP_4.5.sd.PIPO.raster, col = rev(viridis(4)), zlim = c(0.1,400), box = F, axes = F)
title("RCP_4.5", line = -3)
plot(RCP_8.5.sd.PIPO.raster, col = rev(viridis(4)), zlim = c(0.1,400), box = F, axes = F)
title("RCP_8.5", line = -3)
title(main = "Standard Deviation of Ponderosa Pine Biomass", outer = TRUE, line = -3, cex.main = 2.0)


#Abies lasiocarpa

w.dir <- "W:/Results_July26_2017/"

library(raster)
library(rgdal)
library(viridis)

years <- seq(from=0, to=90, by=10)
#timesteps <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90) # input number of timesteps (years in which fire maps are created) here
no_reps <- 10  # input nummber of replicates here
scenario <- c("Historical", "RCP_4.5", "RCP_8.5")
seq.cols <- cbind(1:10, 11:20, 21:30, 31:40, 41:50, 51:60, 61:70, 71:80, 81:90, 91:100)

##setting up loop objects - This version is at decadal time steps, so I couldn't figure out how to create a matrix and then fill in. Instead I'm creating NULL items and then cbind'ing into them.
ABLA.years <- NULL
ABLA.reps <- NULL
ABLA.maps <- list() #final list of all scenarios

##Looping through years, reps, and scenarios to create a list of all biomass maps (change to appropriate species code throughout)
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for (i in 1:no_reps){
    print(paste('rep=', i))
    for(k in years){
      print(paste('years=', k))
      map.ABLA <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/", "abielasi-", k, ".img", sep=""))
      map.df.ABLA <- as.data.frame(map.ABLA)
      ABLA.years <- cbind(ABLA.years,map.df.ABLA[,1]) ## binding all years
    }
    ABLA.reps<- cbind(ABLA.reps, ABLA.years) #binding years across reps
  }
  ABLA.maps[[Scenario]] <- ABLA.reps
  
}
rm(Scenario)
rm(i)
rm(k)

#Calculate relevant statistics

#Calculate mean across cells INCLUDING zeros (instances where ABLA is NOT present)
hist.ABLA <- ABLA.maps$Historical #Index out just historical
hist.mean.ABLA <- rowMeans(hist.ABLA, na.rm = T) #calculate means within each cell across year & replicate
mean(hist.mean.ABLA, na.rm = T) #check to make sure value makes sense
hist.mean.ABLA <- as.matrix(hist.mean.ABLA)
hist.ABLA.raster <- raster(hist.mean.ABLA, template=map.ABLA)
plot(hist.ABLA.raster)


RCP_4.5.ABLA <- ABLA.maps$RCP_4.5 #Index out just RCP_4.5
RCP_4.5.mean.ABLA <- rowMeans(RCP_4.5.ABLA, na.rm = T) #calculate means within each cell across year & replicate
mean(RCP_4.5.mean.ABLA, na.rm = T) #check to make sure value makes sense
RCP_4.5.mean.ABLA <- as.matrix(RCP_4.5.mean.ABLA)
RCP_4.5.ABLA.raster <- raster(RCP_4.5.mean.ABLA, template=map.ABLA)
plot(RCP_4.5.ABLA.raster)

RCP_8.5.ABLA <- ABLA.maps$RCP_8.5 #Index out just RCP_8.5
RCP_8.5.mean.ABLA <- rowMeans(RCP_8.5.ABLA, na.rm = T) #calculate means within each cell across year & replicate
mean(RCP_8.5.mean.ABLA, na.rm = T) #check to make sure value makes sense
RCP_8.5.mean.ABLA <- as.matrix(RCP_8.5.mean.ABLA)
RCP_8.5.ABLA.raster <- raster(RCP_8.5.mean.ABLA, template=map.ABLA)
plot(RCP_8.5.ABLA.raster)


par(mfrow=c(1,3))
plot(hist.ABLA.raster, col = rev(viridis(100)), zlim = c(0.1,5000), box = F, axes = F)
title("Historical", line = -3)
plot(RCP_4.5.ABLA.raster, col = rev(viridis(100)), zlim = c(0.1,5000), box = F, axes = F)
title("RCP_4.5", line = -3)
plot(RCP_8.5.ABLA.raster, col = rev(viridis(100)), zlim = c(0.1,5000), box = F, axes = F)
title("RCP_8.5", line = -3)
title(main = "Mean Sub-Alpine Fir Biomass", outer = TRUE, line = -3, cex.main = 2.0)


#make a version that only includes instances where ABLA is present

hist.ABLA_no0 <- hist.ABLA
hist.ABLA_no0[hist.ABLA_no0 == 0] <- NA
hist.mean.ABLA_no0 <- rowMeans(hist.ABLA_no0, na.rm = T)
mean(hist.mean.ABLA_no0, na.rm = T)
hist.mean.ABLA_no0 <- as.matrix(hist.mean.ABLA_no0)
hist.ABLA_no0.raster <- raster(hist.mean.ABLA_no0, template = map.ABLA)

RCP_4.5.ABLA_no0 <- RCP_4.5.ABLA
RCP_4.5.ABLA_no0[RCP_4.5.ABLA_no0 == 0] <- NA
RCP_4.5.mean.ABLA_no0 <- rowMeans(RCP_4.5.ABLA_no0, na.rm = T)
mean(RCP_4.5.mean.ABLA_no0, na.rm = T)
RCP_4.5.mean.ABLA_no0 <- as.matrix (RCP_4.5.mean.ABLA_no0)
RCP_4.5.ABLA_no0.raster <- raster(RCP_4.5.mean.ABLA_no0, template = map.ABLA)

RCP_8.5.ABLA_no0 <- RCP_8.5.ABLA
RCP_8.5.ABLA_no0[RCP_8.5.ABLA_no0 == 0] <- NA
RCP_8.5.mean.ABLA_no0 <- rowMeans(RCP_8.5.ABLA_no0, na.rm = T)
mean(RCP_8.5.mean.ABLA_no0, na.rm = T)
RCP_8.5.mean.ABLA_no0 <- as.matrix (RCP_8.5.mean.ABLA_no0)
RCP_8.5.ABLA_no0.raster <- raster(RCP_8.5.mean.ABLA_no0, template = map.ABLA)


par(mfrow=c(1,3))
plot(hist.ABLA_no0.raster, col = rev(viridis(30)), zlim = c(0.1,7000), box = F, axes = F)
title("Historical", line = -3)
plot(RCP_4.5.ABLA_no0.raster, col = rev(viridis(30)), zlim = c(0.1,7000), box = F, axes = F)
title("RCP_4.5", line = -3)
plot(RCP_8.5.ABLA_no0.raster, col = rev(viridis(30)), zlim = c(0.1,7000), box = F, axes = F)
title("RCP_8.5", line = -3)
title(main = "Mean Alpine Fir Biomass Where it Occurs", outer = TRUE, line = -3, cex.main = 2.0)

#Plot standard deviations

par(mfrow=c(1,3))
hist.sd.ABLA <- as.matrix(apply(hist.ABLA, 1, sd)) #calculate sd within each cell across year & replicate
hist.sd.ABLA.raster <- raster(hist.sd.ABLA, template=map.ABLA)


RCP_4.5.sd.ABLA <- as.matrix(apply(RCP_4.5.ABLA, 1, sd)) #calculate sd within each cell across year & replicate
RCP_4.5.sd.ABLA.raster <- raster(RCP_4.5.sd.ABLA, template=map.ABLA)


RCP_8.5.sd.ABLA <- as.matrix(apply(RCP_8.5.ABLA, 1, sd)) #calculate sd within each cell across year & replicate
RCP_8.5.sd.ABLA.raster <- raster(RCP_8.5.sd.ABLA, template=map.ABLA)


par(mfrow=c(1,3))
plot(hist.sd.ABLA.raster, col = rev(viridis(4)), zlim = c(0.1,7000), box = F, axes = F)
title("Historical", line = -3)
plot(RCP_4.5.sd.ABLA.raster, col = rev(viridis(4)), zlim = c(0.1,700), box = F, axes = F)
title("RCP_4.5", line = -3)
plot(RCP_8.5.sd.ABLA.raster, col = rev(viridis(4)), zlim = c(0.1,700), box = F, axes = F)
title("RCP_8.5", line = -3)
title(main = "Standard Deviation of Subalpine Fir Biomass", outer = TRUE, line = -3, cex.main = 2.0)


#Picea engelmannii

w.dir <- "W:/Results_July26_2017/"

library(raster)
library(rgdal)
library(viridis)

years <- seq(from=0, to=90, by=10)
#timesteps <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90) # input number of timesteps (years in which fire maps are created) here
no_reps <- 10  # input nummber of replicates here
scenario <- c("RCP_4.5orical", "RCP_4.5", "RCP_8.5")
seq.cols <- cbind(1:10, 11:20, 21:30, 31:40, 41:50, 51:60, 61:70, 71:80, 81:90, 91:100)

##setting up loop objects - This version is at decadal time steps, so I couldn't figure out how to create a matrix and then fill in. Instead I'm creating NULL items and then cbind'ing into them.
PIEN.years <- NULL
PIEN.reps <- NULL
PIEN.maps <- list() #final list of all scenarios

##Looping through years, reps, and scenarios to create a list of all biomass maps (change to appropriate species code throughout)
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for (i in 1:no_reps){
    print(paste('rep=', i))
    for(k in years){
      print(paste('years=', k))
      map.PIEN <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/", "abielasi-", k, ".img", sep=""))
      map.df.PIEN <- as.data.frame(map.PIEN)
      PIEN.years <- cbind(PIEN.years,map.df.PIEN[,1]) ## binding all years
    }
    PIEN.reps<- cbind(PIEN.reps, PIEN.years) #binding years across reps
  }
  PIEN.maps[[Scenario]] <- PIEN.reps
  
}
rm(Scenario)
rm(i)
rm(k)

#Calculate relevant statistics

#Calculate mean across cells INCLUDING zeros (instances where PIEN is NOT present)
hist.PIEN <- PIEN.maps$Historical #Index out just historical
hist.mean.PIEN <- rowMeans(hist.PIEN, na.rm = T) #calculate means within each cell across year & replicate
mean(hist.mean.PIEN, na.rm = T) #check to make sure value makes sense
hist.mean.PIEN <- as.matrix(hist.mean.PIEN)
hist.PIEN.raster <- raster(hist.mean.PIEN, template=map.PIEN)
plot(hist.PIEN.raster)


RCP_4.5.PIEN <- PIEN.maps$RCP_4.5 #Index out just RCP_4.5
RCP_4.5.mean.PIEN <- rowMeans(RCP_4.5.PIEN, na.rm = T) #calculate means within each cell across year & replicate
mean(RCP_4.5.mean.PIEN, na.rm = T) #check to make sure value makes sense
RCP_4.5.mean.PIEN <- as.matrix(RCP_4.5.mean.PIEN)
RCP_4.5.PIEN.raster <- raster(RCP_4.5.mean.PIEN, template=map.PIEN)
plot(RCP_4.5.PIEN.raster)

RCP_8.5.PIEN <- PIEN.maps$RCP_8.5 #Index out just RCP_8.5
RCP_8.5.mean.PIEN <- rowMeans(RCP_8.5.PIEN, na.rm = T) #calculate means within each cell across year & replicate
mean(RCP_8.5.mean.PIEN, na.rm = T) #check to make sure value makes sense
RCP_8.5.mean.PIEN <- as.matrix(RCP_8.5.mean.PIEN)
RCP_8.5.PIEN.raster <- raster(RCP_8.5.mean.PIEN, template=map.PIEN)
plot(RCP_8.5.PIEN.raster)


par(mfrow=c(1,3))
plot(hist.PIEN.raster, col = rev(viridis(100)), zlim = c(0.1,10000), box = F, axes = F)
title("Historical", line = -3)
plot(RCP_4.5.PIEN.raster, col = rev(viridis(100)), zlim = c(0.1,10000), box = F, axes = F)
title("RCP_4.5", line = -3)
plot(RCP_8.5.PIEN.raster, col = rev(viridis(100)), zlim = c(0.1,10000), box = F, axes = F)
title("RCP_8.5", line = -3)
title(main = "Mean Engelmann Biomass", outer = TRUE, line = -3, cex.main = 2.0)


#make a version that only includes instances where PIEN is present

hist.PIEN_no0 <- hist.PIEN
hist.PIEN_no0[hist.PIEN_no0 == 0] <- NA
hist.mean.PIEN_no0 <- rowMeans(hist.PIEN_no0, na.rm = T)
mean(hist.mean.PIEN_no0, na.rm = T)
hist.mean.PIEN_no0 <- as.matrix(hist.mean.PIEN_no0)
hist.PIEN_no0.raster <- raster(hist.mean.PIEN_no0, template = map.PIEN)

RCP_4.5.PIEN_no0 <- RCP_4.5.PIEN
RCP_4.5.PIEN_no0[RCP_4.5.PIEN_no0 == 0] <- NA
RCP_4.5.mean.PIEN_no0 <- rowMeans(RCP_4.5.PIEN_no0, na.rm = T)
mean(RCP_4.5.mean.PIEN_no0, na.rm = T)
RCP_4.5.mean.PIEN_no0 <- as.matrix (RCP_4.5.mean.PIEN_no0)
RCP_4.5.PIEN_no0.raster <- raster(RCP_4.5.mean.PIEN_no0, template = map.PIEN)

RCP_8.5.PIEN_no0 <- RCP_8.5.PIEN
RCP_8.5.PIEN_no0[RCP_8.5.PIEN_no0 == 0] <- NA
RCP_8.5.mean.PIEN_no0 <- rowMeans(RCP_8.5.PIEN_no0, na.rm = T)
mean(RCP_8.5.mean.PIEN_no0, na.rm = T)
RCP_8.5.mean.PIEN_no0 <- as.matrix (RCP_8.5.mean.PIEN_no0)
RCP_8.5.PIEN_no0.raster <- raster(RCP_8.5.mean.PIEN_no0, template = map.PIEN)


par(mfrow=c(1,3))
plot(hist.PIEN_no0.raster, col = rev(viridis(30)), zlim = c(0.1,7000), box = F, axes = F)
title("Historical", line = -3)
plot(RCP_4.5.PIEN_no0.raster, col = rev(viridis(30)), zlim = c(0.1,7000), box = F, axes = F)
title("RCP_4.5", line = -3)
plot(RCP_8.5.PIEN_no0.raster, col = rev(viridis(30)), zlim = c(0.1,7000), box = F, axes = F)
title("RCP_8.5", line = -3)
title(main = "Mean Engelmann Spruce Biomass Where it Occurs", outer = TRUE, line = -3, cex.main = 2.0)
