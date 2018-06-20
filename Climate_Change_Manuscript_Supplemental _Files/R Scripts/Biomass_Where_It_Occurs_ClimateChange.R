#### Project: More widespread and severe wildfires under climate change lead to dramatic declines in 
#### high-elevation species in the dry mixed conifer forests of the inland western U.S. by 
#### Brooke A. Cassell, Portland State University, Dissertation defended February 2018


#This script looks at biomass for each species *where it occurs only* and how it changes through time.
#Written by Brooke Cassell December 2017

w.dir <- "Z:/ClimateChange_Results_Oct6_2017/"


library(raster)
library(rgdal)
#library(viridis)
library(dplyr)
library(vegan)

ecomap <- raster("Z:/ClimateChange_Results_Oct6_2017/Historical/replicate1/ecoregions4ha1.img") #read in the map of ecoregions to use as a mask
#plot(ecomap) #plot to make sure it looks right
ecomap[(ecomap ==0)] <- NA #change cells with value = 0 to NA. Since all active cells have an ecoregion value, this will give a mask for inactive cells



years <- c(0,90) #to look just at start & end years
#timesteps <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90) # input number of timesteps (years in which biomass maps are created) here
no_reps <- 10  # input nummber of replicates here
scenario <- c("Historical", "RCP_4.5", "RCP_8.5")
#scenario <- "Historical"
#seq.cols <- cbind(1:10, 11:20, 21:30, 31:40, 41:50, 51:60, 61:70, 71:80, 81:90, 91:100) #Not needed for biomass version because it's decadal and not annual.


### Total Biomass ###

##setting up loop objects - This version is at decadal time steps, so I couldn't figure out how to create a matrix and then fill in. Instead I'm creating NULL items and then cbind'ing into them.
AGB.years <- NULL
AGB.reps <- NULL
AGB.maps <- list() #final list of all scenarios


##Looping through years, reps, and scenarios to create a list of all biomass maps (change to appropriate species code throughout)
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for(k in years){
    print(paste('years=', k))  
    for (i in 1:no_reps){
      print(paste('rep=', i))
      map.AGB <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/", "TotalBiomass-",k, ".img", sep=""), template=ecomap)
      map.df.AGB <- as.data.frame(map.AGB)
      map.df.AGB[(map.df.AGB == 0),] <- NA
      ave.AGB <- as.data.frame(colMeans(map.df.AGB, na.rm = T))
      AGB.reps <- cbind(AGB.reps, ave.AGB[,1]) ## binding all years
    }
    AGB.years<- cbind(AGB.years, AGB.reps) #binding years across reps
    AGB.reps <- NULL #remove previous scenario for next loop
  }
  AGB.maps[[Scenario]] <- AGB.years
  AGB.years <- NULL #remove previous scenario for next loop
  
}
rm(Scenario)
rm(i)
rm(k)

Historical_biomass <- as.data.frame(t(AGB.maps$Historical)) #Index out each scenario
RCP_4.5_biomass <- as.data.frame(t(AGB.maps$RCP_4.5))
RCP_8.5_biomass <- as.data.frame(t(AGB.maps$RCP_8.5))

colnames(Historical_biomass) <- "g_m_2"
Historical_biomass$year <- as.factor(rep(c(0,90),each = 10))
Historical_biomass$scenario <- as.factor("Historical")
Historical_biomass$rep <- as.factor(1:10)

colnames(RCP_4.5_biomass) <- "g_m_2"
RCP_4.5_biomass$year <- as.factor(rep(c(0,90),each = 10))
RCP_4.5_biomass$scenario <- as.factor("RCP_4.5")
RCP_4.5_biomass$rep <- as.factor(1:10)

colnames(RCP_8.5_biomass) <- "g_m_2"
RCP_8.5_biomass$year <- as.factor(rep(c(0,90),each = 10))
RCP_8.5_biomass$scenario <- as.factor("RCP_8.5")
RCP_8.5_biomass$rep <- as.factor(1:10)

AGB_Biomass <- as.data.frame(rbind(Historical_biomass, RCP_4.5_biomass, RCP_8.5_biomass ))
AGB_Biomass$species <- as.factor("AGB")


### PIPO ###

##setting up loop objects - This version is at decadal time steps, so I couldn't figure out how to create a matrix and then fill in. Instead I'm creating NULL items and then cbind'ing into them.
PIPO.years <- NULL
PIPO.reps <- NULL
PIPO.maps <- list() #final list of all scenarios


##Looping through years, reps, and scenarios to create a list of all biomass maps (change to appropriate species code throughout)
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for(k in years){
    print(paste('years=', k))  
    for (i in 1:no_reps){
      print(paste('rep=', i))
      map.PIPO <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/", "pinupond-",k, ".img", sep=""), template=ecomap)
      map.df.PIPO <- as.data.frame(map.PIPO)
      map.df.PIPO[(map.df.PIPO == 0),] <- NA
      ave.PIPO <- as.data.frame(colMeans(map.df.PIPO, na.rm = T))
      PIPO.reps <- cbind(PIPO.reps, ave.PIPO[,1]) ## binding all years
    }
    PIPO.years<- cbind(PIPO.years, PIPO.reps) #binding years across reps
    PIPO.reps <- NULL #remove previous scenario for next loop
  }
  PIPO.maps[[Scenario]] <- PIPO.years
  PIPO.years <- NULL #remove previous scenario for next loop
  
}
rm(Scenario)
rm(i)
rm(k)

Historical_PIPO <- as.data.frame(t(PIPO.maps$Historical)) #Index out each scenario
RCP_4.5_PIPO <- as.data.frame(t(PIPO.maps$RCP_4.5))
RCP_8.5_PIPO <- as.data.frame(t(PIPO.maps$RCP_8.5))

colnames(Historical_PIPO) <- "g_m_2"
Historical_PIPO$year <- as.factor(rep(c(0,90),each = 10))
Historical_PIPO$scenario <- as.factor("Historical")
Historical_PIPO$rep <- as.factor(1:10)

colnames(RCP_4.5_PIPO) <- "g_m_2"
RCP_4.5_PIPO$year <- as.factor(rep(c(0,90),each = 10))
RCP_4.5_PIPO$scenario <- as.factor("RCP_4.5")
RCP_4.5_PIPO$rep <- as.factor(1:10)

colnames(RCP_8.5_PIPO) <- "g_m_2"
RCP_8.5_PIPO$year <- as.factor(rep(c(0,90),each = 10))
RCP_8.5_PIPO$scenario <- as.factor("RCP_8.5")
RCP_8.5_PIPO$rep <- as.factor(1:10)

PIPO_Biomass <- as.data.frame(rbind(Historical_PIPO, RCP_4.5_PIPO, RCP_8.5_PIPO ))
PIPO_Biomass$species <- as.factor("PIPO")


### ABGR ###

##setting up loop objects - This version is at decadal time steps, so I couldn't figure out how to create a matrix and then fill in. Instead I'm creating NULL items and then cbind'ing into them.
ABGR.years <- NULL
ABGR.reps <- NULL
ABGR.maps <- list() #final list of all scenarios


##Looping through years, reps, and scenarios to create a list of all biomass maps (change to appropriate species code throughout)
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for(k in years){
    print(paste('years=', k))  
    for (i in 1:no_reps){
      print(paste('rep=', i))
      map.ABGR <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/", "abiegran-",k, ".img", sep=""), template=ecomap)
      map.df.ABGR <- as.data.frame(map.ABGR)
      map.df.ABGR[(map.df.ABGR == 0),] <- NA
      ave.ABGR <- as.data.frame(colMeans(map.df.ABGR, na.rm = T))
      ABGR.reps <- cbind(ABGR.reps, ave.ABGR[,1]) ## binding all years
    }
    ABGR.years<- cbind(ABGR.years, ABGR.reps) #binding years across reps
    ABGR.reps <- NULL #remove previous scenario for next loop
  }
  ABGR.maps[[Scenario]] <- ABGR.years
  ABGR.years <- NULL #remove previous scenario for next loop
  
}
rm(Scenario)
rm(i)
rm(k)

Historical_ABGR <- as.data.frame(t(ABGR.maps$Historical)) #Index out each scenario
RCP_4.5_ABGR <- as.data.frame(t(ABGR.maps$RCP_4.5))
RCP_8.5_ABGR <- as.data.frame(t(ABGR.maps$RCP_8.5))

colnames(Historical_ABGR) <- "g_m_2"
Historical_ABGR$year <- as.factor(rep(c(0,90),each = 10))
Historical_ABGR$scenario <- as.factor("Historical")
Historical_ABGR$rep <- as.factor(1:10)

colnames(RCP_4.5_ABGR) <- "g_m_2"
RCP_4.5_ABGR$year <- as.factor(rep(c(0,90),each = 10))
RCP_4.5_ABGR$scenario <- as.factor("RCP_4.5")
RCP_4.5_ABGR$rep <- as.factor(1:10)

colnames(RCP_8.5_ABGR) <- "g_m_2"
RCP_8.5_ABGR$year <- as.factor(rep(c(0,90),each = 10))
RCP_8.5_ABGR$scenario <- as.factor("RCP_8.5")
RCP_8.5_ABGR$rep <- as.factor(1:10)

ABGR_Biomass <- as.data.frame(rbind(Historical_ABGR, RCP_4.5_ABGR, RCP_8.5_ABGR ))
ABGR_Biomass$species <- as.factor("ABGR")

### PICO ###

##setting up loop objects - This version is at decadal time steps, so I couldn't figure out how to create a matrix and then fill in. Instead I'm creating NULL items and then cbind'ing into them.
PICO.years <- NULL
PICO.reps <- NULL
PICO.maps <- list() #final list of all scenarios


##Looping through years, reps, and scenarios to create a list of all biomass maps (change to appropriate species code throughout)
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for(k in years){
    print(paste('years=', k))  
    for (i in 1:no_reps){
      print(paste('rep=', i))
      map.PICO <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/", "pinucont-",k, ".img", sep=""), template=ecomap)
      map.df.PICO <- as.data.frame(map.PICO)
      map.df.PICO[(map.df.PICO == 0),] <- NA
      ave.PICO <- as.data.frame(colMeans(map.df.PICO, na.rm = T))
      PICO.reps <- cbind(PICO.reps, ave.PICO[,1]) ## binding all years
    }
    PICO.years<- cbind(PICO.years, PICO.reps) #binding years across reps
    PICO.reps <- NULL #remove previous scenario for next loop
  }
  PICO.maps[[Scenario]] <- PICO.years
  PICO.years <- NULL #remove previous scenario for next loop
  
}
rm(Scenario)
rm(i)
rm(k)

Historical_PICO <- as.data.frame(t(PICO.maps$Historical)) #Index out each scenario
RCP_4.5_PICO <- as.data.frame(t(PICO.maps$RCP_4.5))
RCP_8.5_PICO <- as.data.frame(t(PICO.maps$RCP_8.5))

colnames(Historical_PICO) <- "g_m_2"
Historical_PICO$year <- as.factor(rep(c(0,90),each = 10))
Historical_PICO$scenario <- as.factor("Historical")
Historical_PICO$rep <- as.factor(1:10)

colnames(RCP_4.5_PICO) <- "g_m_2"
RCP_4.5_PICO$year <- as.factor(rep(c(0,90),each = 10))
RCP_4.5_PICO$scenario <- as.factor("RCP_4.5")
RCP_4.5_PICO$rep <- as.factor(1:10)

colnames(RCP_8.5_PICO) <- "g_m_2"
RCP_8.5_PICO$year <- as.factor(rep(c(0,90),each = 10))
RCP_8.5_PICO$scenario <- as.factor("RCP_8.5")
RCP_8.5_PICO$rep <- as.factor(1:10)

PICO_Biomass <- as.data.frame(rbind(Historical_PICO, RCP_4.5_PICO, RCP_8.5_PICO ))
PICO_Biomass$species <- as.factor("PICO")

### PSME ###

##setting up loop objects - This version is at decadal time steps, so I couldn't figure out how to create a matrix and then fill in. Instead I'm creating NULL items and then cbind'ing into them.
PSME.years <- NULL
PSME.reps <- NULL
PSME.maps <- list() #final list of all scenarios


##Looping through years, reps, and scenarios to create a list of all biomass maps (change to appropriate species code throughout)
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for(k in years){
    print(paste('years=', k))  
    for (i in 1:no_reps){
      print(paste('rep=', i))
      map.PSME <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/", "pseumenz-",k, ".img", sep=""), template=ecomap)
      map.df.PSME <- as.data.frame(map.PSME)
      map.df.PSME[(map.df.PSME == 0),] <- NA
      ave.PSME <- as.data.frame(colMeans(map.df.PSME, na.rm = T))
      PSME.reps <- cbind(PSME.reps, ave.PSME[,1]) ## binding all years
    }
    PSME.years<- cbind(PSME.years, PSME.reps) #binding years across reps
    PSME.reps <- NULL #remove previous scenario for next loop
  }
  PSME.maps[[Scenario]] <- PSME.years
  PSME.years <- NULL #remove previous scenario for next loop
  
}
rm(Scenario)
rm(i)
rm(k)

Historical_PSME <- as.data.frame(t(PSME.maps$Historical)) #Index out each scenario
RCP_4.5_PSME <- as.data.frame(t(PSME.maps$RCP_4.5))
RCP_8.5_PSME <- as.data.frame(t(PSME.maps$RCP_8.5))

colnames(Historical_PSME) <- "g_m_2"
Historical_PSME$year <- as.factor(rep(c(0,90),each = 10))
Historical_PSME$scenario <- as.factor("Historical")
Historical_PSME$rep <- as.factor(1:10)

colnames(RCP_4.5_PSME) <- "g_m_2"
RCP_4.5_PSME$year <- as.factor(rep(c(0,90),each = 10))
RCP_4.5_PSME$scenario <- as.factor("RCP_4.5")
RCP_4.5_PSME$rep <- as.factor(1:10)

colnames(RCP_8.5_PSME) <- "g_m_2"
RCP_8.5_PSME$year <- as.factor(rep(c(0,90),each = 10))
RCP_8.5_PSME$scenario <- as.factor("RCP_8.5")
RCP_8.5_PSME$rep <- as.factor(1:10)

PSME_Biomass <- as.data.frame(rbind(Historical_PSME, RCP_4.5_PSME, RCP_8.5_PSME ))
PSME_Biomass$species <- as.factor("PSME")


### PIAL ###

##setting up loop objects - This version is at decadal time steps, so I couldn't figure out how to create a matrix and then fill in. Instead I'm creating NULL items and then cbind'ing into them.
PIAL.years <- NULL
PIAL.reps <- NULL
PIAL.maps <- list() #final list of all scenarios


##Looping through years, reps, and scenarios to create a list of all biomass maps (change to appropriate species code throughout)
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for(k in years){
    print(paste('years=', k))  
    for (i in 1:no_reps){
      print(paste('rep=', i))
      map.PIAL <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/", "pinualbi-",k, ".img", sep=""), template=ecomap)
      map.df.PIAL <- as.data.frame(map.PIAL)
      map.df.PIAL[(map.df.PIAL == 0),] <- NA
      ave.PIAL <- as.data.frame(colMeans(map.df.PIAL, na.rm = T))
      PIAL.reps <- cbind(PIAL.reps, ave.PIAL[,1]) ## binding all years
    }
    PIAL.years<- cbind(PIAL.years, PIAL.reps) #binding years across reps
    PIAL.reps <- NULL #remove previous scenario for next loop
  }
  PIAL.maps[[Scenario]] <- PIAL.years
  PIAL.years <- NULL #remove previous scenario for next loop
  
}
rm(Scenario)
rm(i)
rm(k)

Historical_PIAL <- as.data.frame(t(PIAL.maps$Historical)) #Index out each scenario
RCP_4.5_PIAL <- as.data.frame(t(PIAL.maps$RCP_4.5))
RCP_8.5_PIAL <- as.data.frame(t(PIAL.maps$RCP_8.5))

colnames(Historical_PIAL) <- "g_m_2"
Historical_PIAL$year <- as.factor(rep(c(0,90),each = 10))
Historical_PIAL$scenario <- as.factor("Historical")
Historical_PIAL$rep <- as.factor(1:10)

colnames(RCP_4.5_PIAL) <- "g_m_2"
RCP_4.5_PIAL$year <- as.factor(rep(c(0,90),each = 10))
RCP_4.5_PIAL$scenario <- as.factor("RCP_4.5")
RCP_4.5_PIAL$rep <- as.factor(1:10)

colnames(RCP_8.5_PIAL) <- "g_m_2"
RCP_8.5_PIAL$year <- as.factor(rep(c(0,90),each = 10))
RCP_8.5_PIAL$scenario <- as.factor("RCP_8.5")
RCP_8.5_PIAL$rep <- as.factor(1:10)

PIAL_Biomass <- as.data.frame(rbind(Historical_PIAL, RCP_4.5_PIAL, RCP_8.5_PIAL ))
PIAL_Biomass$species <- as.factor("PIAL")


### PIEN ###

##setting up loop objects - This version is at decadal time steps, so I couldn't figure out how to create a matrix and then fill in. Instead I'm creating NULL items and then cbind'ing into them.
PIEN.years <- NULL
PIEN.reps <- NULL
PIEN.maps <- list() #final list of all scenarios


##Looping through years, reps, and scenarios to create a list of all biomass maps (change to appropriate species code throughout)
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for(k in years){
    print(paste('years=', k))  
    for (i in 1:no_reps){
      print(paste('rep=', i))
      map.PIEN <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/", "piceenge-",k, ".img", sep=""), template=ecomap)
      map.df.PIEN <- as.data.frame(map.PIEN)
      map.df.PIEN[(map.df.PIEN == 0),] <- NA
      ave.PIEN <- as.data.frame(colMeans(map.df.PIEN, na.rm = T))
      PIEN.reps <- cbind(PIEN.reps, ave.PIEN[,1]) ## binding all years
    }
    PIEN.years<- cbind(PIEN.years, PIEN.reps) #binding years across reps
    PIEN.reps <- NULL #remove previous scenario for next loop
  }
  PIEN.maps[[Scenario]] <- PIEN.years
  PIEN.years <- NULL #remove previous scenario for next loop
  
}
rm(Scenario)
rm(i)
rm(k)

Historical_PIEN <- as.data.frame(t(PIEN.maps$Historical)) #Index out each scenario
RCP_4.5_PIEN <- as.data.frame(t(PIEN.maps$RCP_4.5))
RCP_8.5_PIEN <- as.data.frame(t(PIEN.maps$RCP_8.5))

colnames(Historical_PIEN) <- "g_m_2"
Historical_PIEN$year <- as.factor(rep(c(0,90),each = 10))
Historical_PIEN$scenario <- as.factor("Historical")
Historical_PIEN$rep <- as.factor(1:10)

colnames(RCP_4.5_PIEN) <- "g_m_2"
RCP_4.5_PIEN$year <- as.factor(rep(c(0,90),each = 10))
RCP_4.5_PIEN$scenario <- as.factor("RCP_4.5")
RCP_4.5_PIEN$rep <- as.factor(1:10)

colnames(RCP_8.5_PIEN) <- "g_m_2"
RCP_8.5_PIEN$year <- as.factor(rep(c(0,90),each = 10))
RCP_8.5_PIEN$scenario <- as.factor("RCP_8.5")
RCP_8.5_PIEN$rep <- as.factor(1:10)

PIEN_Biomass <- as.data.frame(rbind(Historical_PIEN, RCP_4.5_PIEN, RCP_8.5_PIEN ))
PIEN_Biomass$species <- as.factor("PIEN")

### ABLA ###

##setting up loop objects - This version is at decadal time steps, so I couldn't figure out how to create a matrix and then fill in. Instead I'm creating NULL items and then cbind'ing into them.
ABLA.years <- NULL
ABLA.reps <- NULL
ABLA.maps <- list() #final list of all scenarios


##Looping through years, reps, and scenarios to create a list of all biomass maps (change to appropriate species code throughout)
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for(k in years){
    print(paste('years=', k))  
    for (i in 1:no_reps){
      print(paste('rep=', i))
      map.ABLA <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/", "abielasi-",k, ".img", sep=""), template=ecomap)
      map.df.ABLA <- as.data.frame(map.ABLA)
      map.df.ABLA[(map.df.ABLA == 0),] <- NA
      ave.ABLA <- as.data.frame(colMeans(map.df.ABLA, na.rm = T))
      ABLA.reps <- cbind(ABLA.reps, ave.ABLA[,1]) ## binding all years
    }
    ABLA.years<- cbind(ABLA.years, ABLA.reps) #binding years across reps
    ABLA.reps <- NULL #remove previous scenario for next loop
  }
  ABLA.maps[[Scenario]] <- ABLA.years
  ABLA.years <- NULL #remove previous scenario for next loop
  
}
rm(Scenario)
rm(i)
rm(k)

Historical_ABLA <- as.data.frame(t(ABLA.maps$Historical)) #Index out each scenario
RCP_4.5_ABLA <- as.data.frame(t(ABLA.maps$RCP_4.5))
RCP_8.5_ABLA <- as.data.frame(t(ABLA.maps$RCP_8.5))

colnames(Historical_ABLA) <- "g_m_2"
Historical_ABLA$year <- as.factor(rep(c(0,90),each = 10))
Historical_ABLA$scenario <- as.factor("Historical")
Historical_ABLA$rep <- as.factor(1:10)

colnames(RCP_4.5_ABLA) <- "g_m_2"
RCP_4.5_ABLA$year <- as.factor(rep(c(0,90),each = 10))
RCP_4.5_ABLA$scenario <- as.factor("RCP_4.5")
RCP_4.5_ABLA$rep <- as.factor(1:10)

colnames(RCP_8.5_ABLA) <- "g_m_2"
RCP_8.5_ABLA$year <- as.factor(rep(c(0,90),each = 10))
RCP_8.5_ABLA$scenario <- as.factor("RCP_8.5")
RCP_8.5_ABLA$rep <- as.factor(1:10)

ABLA_Biomass <- as.data.frame(rbind(Historical_ABLA, RCP_4.5_ABLA, RCP_8.5_ABLA ))
ABLA_Biomass$species <- as.factor("ABLA")

### LAOC ###

##setting up loop objects - This version is at decadal time steps, so I couldn't figure out how to create a matrix and then fill in. Instead I'm creating NULL items and then cbind'ing into them.
LAOC.years <- NULL
LAOC.reps <- NULL
LAOC.maps <- list() #final list of all scenarios


##Looping through years, reps, and scenarios to create a list of all biomass maps (change to appropriate species code throughout)
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for(k in years){
    print(paste('years=', k))  
    for (i in 1:no_reps){
      print(paste('rep=', i))
      map.LAOC <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/", "lariocci-",k, ".img", sep=""), template=ecomap)
      map.df.LAOC <- as.data.frame(map.LAOC)
      map.df.LAOC[(map.df.LAOC == 0),] <- NA
      ave.LAOC <- as.data.frame(colMeans(map.df.LAOC, na.rm = T))
      LAOC.reps <- cbind(LAOC.reps, ave.LAOC[,1]) ## binding all years
    }
    LAOC.years<- cbind(LAOC.years, LAOC.reps) #binding years across reps
    LAOC.reps <- NULL #remove previous scenario for next loop
  }
  LAOC.maps[[Scenario]] <- LAOC.years
  LAOC.years <- NULL #remove previous scenario for next loop
  
}
rm(Scenario)
rm(i)
rm(k)

Historical_LAOC <- as.data.frame(t(LAOC.maps$Historical)) #Index out each scenario
RCP_4.5_LAOC <- as.data.frame(t(LAOC.maps$RCP_4.5))
RCP_8.5_LAOC <- as.data.frame(t(LAOC.maps$RCP_8.5))

colnames(Historical_LAOC) <- "g_m_2"
Historical_LAOC$year <- as.factor(rep(c(0,90),each = 10))
Historical_LAOC$scenario <- as.factor("Historical")
Historical_LAOC$rep <- as.factor(1:10)

colnames(RCP_4.5_LAOC) <- "g_m_2"
RCP_4.5_LAOC$year <- as.factor(rep(c(0,90),each = 10))
RCP_4.5_LAOC$scenario <- as.factor("RCP_4.5")
RCP_4.5_LAOC$rep <- as.factor(1:10)

colnames(RCP_8.5_LAOC) <- "g_m_2"
RCP_8.5_LAOC$year <- as.factor(rep(c(0,90),each = 10))
RCP_8.5_LAOC$scenario <- as.factor("RCP_8.5")
RCP_8.5_LAOC$rep <- as.factor(1:10)

LAOC_Biomass <- as.data.frame(rbind(Historical_LAOC, RCP_4.5_LAOC, RCP_8.5_LAOC ))
LAOC_Biomass$species <- as.factor("LAOC")

### JUOC ###

##setting up loop objects - This version is at decadal time steps, so I couldn't figure out how to create a matrix and then fill in. Instead I'm creating NULL items and then cbind'ing into them.
JUOC.years <- NULL
JUOC.reps <- NULL
JUOC.maps <- list() #final list of all scenarios


##Looping through years, reps, and scenarios to create a list of all biomass maps (change to appropriate species code throughout)
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for(k in years){
    print(paste('years=', k))  
    for (i in 1:no_reps){
      print(paste('rep=', i))
      map.JUOC <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/", "juniocci-",k, ".img", sep=""), template=ecomap)
      map.df.JUOC <- as.data.frame(map.JUOC)
      map.df.JUOC[(map.df.JUOC == 0),] <- NA
      ave.JUOC <- as.data.frame(colMeans(map.df.JUOC, na.rm = T))
      JUOC.reps <- cbind(JUOC.reps, ave.JUOC[,1]) ## binding all years
    }
    JUOC.years<- cbind(JUOC.years, JUOC.reps) #binding years across reps
    JUOC.reps <- NULL #remove previous scenario for next loop
  }
  JUOC.maps[[Scenario]] <- JUOC.years
  JUOC.years <- NULL #remove previous scenario for next loop
  
}
rm(Scenario)
rm(i)
rm(k)

Historical_JUOC <- as.data.frame(t(JUOC.maps$Historical)) #Index out each scenario
RCP_4.5_JUOC <- as.data.frame(t(JUOC.maps$RCP_4.5))
RCP_8.5_JUOC <- as.data.frame(t(JUOC.maps$RCP_8.5))

colnames(Historical_JUOC) <- "g_m_2"
Historical_JUOC$year <- as.factor(rep(c(0,90),each = 10))
Historical_JUOC$scenario <- as.factor("Historical")
Historical_JUOC$rep <- as.factor(1:10)

colnames(RCP_4.5_JUOC) <- "g_m_2"
RCP_4.5_JUOC$year <- as.factor(rep(c(0,90),each = 10))
RCP_4.5_JUOC$scenario <- as.factor("RCP_4.5")
RCP_4.5_JUOC$rep <- as.factor(1:10)

colnames(RCP_8.5_JUOC) <- "g_m_2"
RCP_8.5_JUOC$year <- as.factor(rep(c(0,90),each = 10))
RCP_8.5_JUOC$scenario <- as.factor("RCP_8.5")
RCP_8.5_JUOC$rep <- as.factor(1:10)

JUOC_Biomass <- as.data.frame(rbind(Historical_JUOC, RCP_4.5_JUOC, RCP_8.5_JUOC ))
JUOC_Biomass$species <- as.factor("JUOC")

########################################
# plot individual species biomass as boxplots using dplyr to summarise the data


All_Biomass <- as.data.frame(rbind(PIPO_Biomass, ABGR_Biomass, PSME_Biomass, PICO_Biomass, JUOC_Biomass, LAOC_Biomass, PIEN_Biomass, ABLA_Biomass, 
                                   PIAL_Biomass))

All_Biomass_0 <- as.data.frame(All_Biomass[(All_Biomass$year == 0),]) #Index out year 0 and 90
All_Biomass_90 <- as.data.frame(All_Biomass[(All_Biomass$year == 90),])


All_Biomass_change <- as.data.frame(All_Biomass_90$g_m_2-All_Biomass_0$g_m_2) #create dataframe with the change from year 0 to 90
All_Biomass_change$species <- All_Biomass_90$species
All_Biomass_change$scenario <- All_Biomass_90$scenario
colnames(All_Biomass_change)[1] <- "g_m_2"

Percent_Change <- as.data.frame((All_Biomass_90$g_m_2-All_Biomass_0$g_m_2)/All_Biomass_0$g_m_2) #create dataframe with the % change from year 0 to 90
Percent_Change$per_change <- (round(Percent_Change[,1],4)*100)
Percent_Change$species <- All_Biomass_90$species
Percent_Change$scenario <- All_Biomass_90$scenario

#summarise % change
Mean_Percent_Change <- group_by(Percent_Change, scenario, species, add = T)
summarise_mean_percent_change <- summarise(Mean_Percent_Change, change = mean(per_change), sd_change = sd(per_change))
write.csv(summarise_mean_percent_change, "Z:/ClimateChange_Results_Oct6_2017/Biomass_Data/Biomass_Density_Percent_Change.csv", row.names = F)


#summarise overall biomass data
Mean_biomass_across_reps <- group_by(All_Biomass, scenario, species, year, add = T) #Overall mean & sd
summarise_mean_biomass_across_reps <- summarise(Mean_biomass_across_reps, biomass_g = mean(g_m_2), sd = sd(g_m_2))
summarise_mean_biomass_across_reps

write.csv(summarise_mean_biomass_across_reps, "Z:/ClimateChange_Results_Oct6_2017/Biomass_Data/Mean_Biomass_Species.csv", row.names = F)

#summarise year 0 biomass
Mean_biomass_across_reps_0 <- group_by(All_Biomass_0, scenario, species, add = T) #Overall mean & sd
summarise_mean_biomass_across_reps_0 <- summarise(Mean_biomass_across_reps_0, biomass_g = mean(g_m_2), sd = sd(g_m_2))
summarise_mean_biomass_across_reps_0

#summarise year 90 biomass
Mean_biomass_across_reps_90 <- group_by(All_Biomass_90, scenario, species, add = T) #Overall mean & sd
summarise_mean_biomass_across_reps_90 <- summarise(Mean_biomass_across_reps_90, biomass_g = mean(g_m_2), sd = sd(g_m_2))
summarise_mean_biomass_across_reps_90

#summarise overall change data
Mean_biomass_change <- group_by(All_Biomass_change, scenario, species, add = T)

#vectors of colors and species for plotting
colors <- rep(c("blue", "orange", "red"), times = 3)
speciesNames <- (c("Ponderosa Pine", "Grand Fir", "Douglas Fir", "Lodgepole Pine", "Western Juniper", "Western Larch",  "Sub-alpine Fir", "Engelmann Spruce",  "Whitebark Pine"))

#boxplot year 90 biomass to look at varibility at the final timestep
boxplot(g_m_2 ~ scenario + species, data = Mean_biomass_across_reps_90, las=2, col = colors)

#boxplot change in biomass from year 0 to 90
boxplot(g_m_2 ~ scenario + species, data = Mean_biomass_change, las=1, col = colors, main = "Average Change in Biomass Density Where Species Present", 
        ylab = "Biomass (gm2)", names = speciesNames, at =c(1,2,3, 5,6,7, 9,10,11, 13,14,15, 17,18,19, 21,22,23, 25,26,27, 29,30,31, 33,34,35))
abline(h=0)


#boxplot % change in biomass from year 0 to 90 - this is a better representation of relative changes
par(mar = c(8.6, 4.1, 4.1, 2.1))
boxplot(g_m_2 ~ scenario + species, data = Percent_Change, col = colors, main = "Average % Change in Biomass Density Where Species Present", 
        ylab = "% change", font.axis = 2, at =c(1,2,3, 5,6,7, 9,10,11, 13,14,15, 17,18,19, 21,22,23, 25,26,27, 29,30,31, 33,34,35),
        xaxt = "n", yaxt = "n")
abline(h=0)
axis(1, at = c(2,6,10,14,18,22,26,30,34), labels = speciesNames, las = 2, font = 2)
axis(2, at = c(-75,-50, -25, 0, 25, 50), font = 2, ylab = "% change")
legend("bottomleft", c("Historical", "RCP 4.5", "RCP 8.5"), fill = colors )


######################################################################
#Look at total biomass

AGB_0 <- AGB_Biomass[(AGB_Biomass$year == 0),]
AGB_90 <- AGB_Biomass[(AGB_Biomass$year == 90),]
AGB_Change <- as.data.frame(AGB_90$g_m_2 - AGB_0$g_m_2)
colnames(AGB_Change)[1] <- "g_m_2"
AGB_Change$scenario <- AGB_90$scenario
AGB_Change$percent <- (AGB_Change$g_m_2/AGB_0$g_m_2)*100

Mean_AGB_Biomass <- group_by(AGB_Change, scenario)
summary_AGB_Biomass <- summarise(Mean_AGB_Biomass, meanBiomass = mean(g_m_2), sdBiomass = sd(g_m_2), meanPercent = mean(percent), sdPercent = sd(percent))

Mean_AGB_90 <- group_by(AGB_90, scenario)
summary_AGB_90 <- summarise(Mean_AGB_90, mean = mean(g_m_2), sd = sd(g_m_2))
