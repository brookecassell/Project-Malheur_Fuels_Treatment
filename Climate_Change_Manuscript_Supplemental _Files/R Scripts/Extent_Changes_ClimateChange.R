#### Project: More widespread and severe wildfires under climate change lead to dramatic declines in 
#### high-elevation species in the dry mixed conifer forests of the inland western U.S. by 
#### Brooke A. Cassell, Portland State University, Dissertation defended February 2018


#This script calculates change in extent from year 0 to year 90 for individual conifer species
#Written by Brooke A. Cassell December 2017

w.dir <- "Z:/ClimateChange_Results_Oct6_2017/"


library(raster)
library(rgdal)
library(dplyr)
library(vegan)


years <- c(0,90) #to look just at start & end years
no_reps <- 10  # input nummber of replicates here
scenario <- c("Historical", "RCP_4.5", "RCP_8.5")


### PIPO ###

##setting up loop objects 
PIPOext.years <- NULL
PIPOext.reps <- NULL
PIPOext.maps <- list() #final list of all scenarios


##Looping through years, reps, and scenarios to create a list of all biomass maps (change to appropriate species code throughout)
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for(k in years){
    print(paste('years=', k))  
    for (i in 1:no_reps){
      print(paste('rep=', i))
      map.PIPO <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/", "pinupond-",k, ".img", sep=""), template=ecomap)
      map.df.PIPO <- as.data.frame(map.PIPO)
      map.df.PIPO[(map.df.PIPO > 0),] <- 4
      map.df.PIPO[(map.df.PIPO == 0),] <- NA
      ext.PIPO <- as.data.frame(colSums(map.df.PIPO, na.rm = T))
      PIPOext.reps <- cbind(PIPOext.reps, ext.PIPO[,1]) ## binding all years
    }
    PIPOext.years<- cbind(PIPOext.years, PIPOext.reps) #binding years across reps
    PIPOext.reps <- NULL #remove previous scenario for next loop
  }
  PIPOext.maps[[Scenario]] <- PIPOext.years
  PIPOext.years <- NULL #remove previous scenario for next loop
  
}
rm(Scenario)
rm(i)
rm(k)

Historical_PIPOext <- as.data.frame(t(PIPOext.maps$Historical)) #Index out each scenario
RCP_4.5_PIPOext <- as.data.frame(t(PIPOext.maps$RCP_4.5))
RCP_8.5_PIPOext <- as.data.frame(t(PIPOext.maps$RCP_8.5))

colnames(Historical_PIPOext) <- "extent"
Historical_PIPOext$year <- as.factor(rep(c(0,90),each = 10))
Historical_PIPOext$scenario <- as.factor("Historical")
Historical_PIPOext$rep <- as.factor(1:10)

colnames(RCP_4.5_PIPOext) <- "extent"
RCP_4.5_PIPOext$year <- as.factor(rep(c(0,90),each = 10))
RCP_4.5_PIPOext$scenario <- as.factor("RCP_4.5")
RCP_4.5_PIPOext$rep <- as.factor(1:10)

colnames(RCP_8.5_PIPOext) <- "extent"
RCP_8.5_PIPOext$year <- as.factor(rep(c(0,90),each = 10))
RCP_8.5_PIPOext$scenario <- as.factor("RCP_8.5")
RCP_8.5_PIPOext$rep <- as.factor(1:10)

PIPOext_Biomass <- as.data.frame(rbind(Historical_PIPOext, RCP_4.5_PIPOext, RCP_8.5_PIPOext ))
PIPOext_Biomass$species <- as.factor("PIPO")


### ABGR ###

##setting up loop objects 
ABGRext.years <- NULL
ABGRext.reps <- NULL
ABGRext.maps <- list() #final list of all scenarios


##Looping through years, reps, and scenarios to create a list of all biomass maps (change to appropriate species code throughout)
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for(k in years){
    print(paste('years=', k))  
    for (i in 1:no_reps){
      print(paste('rep=', i))
      map.ABGR <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/", "abiegran-",k, ".img", sep=""), template=ecomap)
      map.df.ABGR <- as.data.frame(map.ABGR)
      map.df.ABGR[(map.df.ABGR > 0),] <- 4
      map.df.ABGR[(map.df.ABGR == 0),] <- NA
      ext.ABGR <- as.data.frame(colSums(map.df.ABGR, na.rm = T))
      ABGRext.reps <- cbind(ABGRext.reps, ext.ABGR[,1]) ## binding all years
    }
    ABGRext.years<- cbind(ABGRext.years, ABGRext.reps) #binding years across reps
    ABGRext.reps <- NULL #remove previous scenario for next loop
  }
  ABGRext.maps[[Scenario]] <- ABGRext.years
  ABGRext.years <- NULL #remove previous scenario for next loop
  
}
rm(Scenario)
rm(i)
rm(k)

Historical_ABGRext <- as.data.frame(t(ABGRext.maps$Historical)) #Index out each scenario
RCP_4.5_ABGRext <- as.data.frame(t(ABGRext.maps$RCP_4.5))
RCP_8.5_ABGRext <- as.data.frame(t(ABGRext.maps$RCP_8.5))

colnames(Historical_ABGRext) <- "extent"
Historical_ABGRext$year <- as.factor(rep(c(0,90),each = 10))
Historical_ABGRext$scenario <- as.factor("Historical")
Historical_ABGRext$rep <- as.factor(1:10)

colnames(RCP_4.5_ABGRext) <- "extent"
RCP_4.5_ABGRext$year <- as.factor(rep(c(0,90),each = 10))
RCP_4.5_ABGRext$scenario <- as.factor("RCP_4.5")
RCP_4.5_ABGRext$rep <- as.factor(1:10)

colnames(RCP_8.5_ABGRext) <- "extent"
RCP_8.5_ABGRext$year <- as.factor(rep(c(0,90),each = 10))
RCP_8.5_ABGRext$scenario <- as.factor("RCP_8.5")
RCP_8.5_ABGRext$rep <- as.factor(1:10)

ABGRext_Biomass <- as.data.frame(rbind(Historical_ABGRext, RCP_4.5_ABGRext, RCP_8.5_ABGRext ))
ABGRext_Biomass$species <- as.factor("ABGR")

### PICO ###

##setting up loop objects
PICOext.years <- NULL
PICOext.reps <- NULL
PICOext.maps <- list() #final list of all scenarios


##Looping through years, reps, and scenarios to create a list of all biomass maps (change to appropriate species code throughout)
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for(k in years){
    print(paste('years=', k))  
    for (i in 1:no_reps){
      print(paste('rep=', i))
      map.PICO <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/", "pinucont-",k, ".img", sep=""), template=ecomap)
      map.df.PICO <- as.data.frame(map.PICO)
      map.df.PICO[(map.df.PICO > 0),] <- 4
      map.df.PICO[(map.df.PICO == 0),] <- NA
      ext.PICO <- as.data.frame(colSums(map.df.PICO, na.rm = T))
      PICOext.reps <- cbind(PICOext.reps, ext.PICO[,1]) ## binding all years
    }
    PICOext.years<- cbind(PICOext.years, PICOext.reps) #binding years across reps
    PICOext.reps <- NULL #remove previous scenario for next loop
  }
  PICOext.maps[[Scenario]] <- PICOext.years
  PICOext.years <- NULL #remove previous scenario for next loop
  
}
rm(Scenario)
rm(i)
rm(k)

Historical_PICOext <- as.data.frame(t(PICOext.maps$Historical)) #Index out each scenario
RCP_4.5_PICOext <- as.data.frame(t(PICOext.maps$RCP_4.5))
RCP_8.5_PICOext <- as.data.frame(t(PICOext.maps$RCP_8.5))

colnames(Historical_PICOext) <- "extent"
Historical_PICOext$year <- as.factor(rep(c(0,90),each = 10))
Historical_PICOext$scenario <- as.factor("Historical")
Historical_PICOext$rep <- as.factor(1:10)

colnames(RCP_4.5_PICOext) <- "extent"
RCP_4.5_PICOext$year <- as.factor(rep(c(0,90),each = 10))
RCP_4.5_PICOext$scenario <- as.factor("RCP_4.5")
RCP_4.5_PICOext$rep <- as.factor(1:10)

colnames(RCP_8.5_PICOext) <- "extent"
RCP_8.5_PICOext$year <- as.factor(rep(c(0,90),each = 10))
RCP_8.5_PICOext$scenario <- as.factor("RCP_8.5")
RCP_8.5_PICOext$rep <- as.factor(1:10)

PICOext_Biomass <- as.data.frame(rbind(Historical_PICOext, RCP_4.5_PICOext, RCP_8.5_PICOext ))
PICOext_Biomass$species <- as.factor("PICO")

### PSME ###

##setting up loop objects
PSMEext.years <- NULL
PSMEext.reps <- NULL
PSMEext.maps <- list() #final list of all scenarios


##Looping through years, reps, and scenarios to create a list of all biomass maps (change to appropriate species code throughout)
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for(k in years){
    print(paste('years=', k))  
    for (i in 1:no_reps){
      print(paste('rep=', i))
      map.PSME <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/", "pseumenz-",k, ".img", sep=""), template=ecomap)
      map.df.PSME <- as.data.frame(map.PSME)
      map.df.PSME[(map.df.PSME > 0),] <- 4
      map.df.PSME[(map.df.PSME == 0),] <- NA
      ext.PSME <- as.data.frame(colSums(map.df.PSME, na.rm = T))
      PSMEext.reps <- cbind(PSMEext.reps, ext.PSME[,1]) ## binding all years
    }
    PSMEext.years<- cbind(PSMEext.years, PSMEext.reps) #binding years across reps
    PSMEext.reps <- NULL #remove previous scenario for next loop
  }
  PSMEext.maps[[Scenario]] <- PSMEext.years
  PSMEext.years <- NULL #remove previous scenario for next loop
  
}
rm(Scenario)
rm(i)
rm(k)

Historical_PSMEext <- as.data.frame(t(PSMEext.maps$Historical)) #Index out each scenario
RCP_4.5_PSMEext <- as.data.frame(t(PSMEext.maps$RCP_4.5))
RCP_8.5_PSMEext <- as.data.frame(t(PSMEext.maps$RCP_8.5))

colnames(Historical_PSMEext) <- "extent"
Historical_PSMEext$year <- as.factor(rep(c(0,90),each = 10))
Historical_PSMEext$scenario <- as.factor("Historical")
Historical_PSMEext$rep <- as.factor(1:10)

colnames(RCP_4.5_PSMEext) <- "extent"
RCP_4.5_PSMEext$year <- as.factor(rep(c(0,90),each = 10))
RCP_4.5_PSMEext$scenario <- as.factor("RCP_4.5")
RCP_4.5_PSMEext$rep <- as.factor(1:10)

colnames(RCP_8.5_PSMEext) <- "extent"
RCP_8.5_PSMEext$year <- as.factor(rep(c(0,90),each = 10))
RCP_8.5_PSMEext$scenario <- as.factor("RCP_8.5")
RCP_8.5_PSMEext$rep <- as.factor(1:10)

PSMEext_Biomass <- as.data.frame(rbind(Historical_PSMEext, RCP_4.5_PSMEext, RCP_8.5_PSMEext ))
PSMEext_Biomass$species <- as.factor("PSME")

### PIAL ###

##setting up loop objects
PIALext.years <- NULL
PIALext.reps <- NULL
PIALext.maps <- list() #final list of all scenarios


##Looping through years, reps, and scenarios to create a list of all biomass maps (change to appropriate species code throughout)
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for(k in years){
    print(paste('years=', k))  
    for (i in 1:no_reps){
      print(paste('rep=', i))
      map.PIAL <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/", "pinualbi-",k, ".img", sep=""), template=ecomap)
      map.df.PIAL <- as.data.frame(map.PIAL)
      map.df.PIAL[(map.df.PIAL > 0),] <- 4
      map.df.PIAL[(map.df.PIAL == 0),] <- NA
      ext.PIAL <- as.data.frame(colSums(map.df.PIAL, na.rm = T))
      PIALext.reps <- cbind(PIALext.reps, ext.PIAL[,1]) ## binding all years
    }
    PIALext.years<- cbind(PIALext.years, PIALext.reps) #binding years across reps
    PIALext.reps <- NULL #remove previous scenario for next loop
  }
  PIALext.maps[[Scenario]] <- PIALext.years
  PIALext.years <- NULL #remove previous scenario for next loop
  
}
rm(Scenario)
rm(i)
rm(k)

Historical_PIALext <- as.data.frame(t(PIALext.maps$Historical)) #Index out each scenario
RCP_4.5_PIALext <- as.data.frame(t(PIALext.maps$RCP_4.5))
RCP_8.5_PIALext <- as.data.frame(t(PIALext.maps$RCP_8.5))

colnames(Historical_PIALext) <- "extent"
Historical_PIALext$year <- as.factor(rep(c(0,90),each = 10))
Historical_PIALext$scenario <- as.factor("Historical")
Historical_PIALext$rep <- as.factor(1:10)

colnames(RCP_4.5_PIALext) <- "extent"
RCP_4.5_PIALext$year <- as.factor(rep(c(0,90),each = 10))
RCP_4.5_PIALext$scenario <- as.factor("RCP_4.5")
RCP_4.5_PIALext$rep <- as.factor(1:10)

colnames(RCP_8.5_PIALext) <- "extent"
RCP_8.5_PIALext$year <- as.factor(rep(c(0,90),each = 10))
RCP_8.5_PIALext$scenario <- as.factor("RCP_8.5")
RCP_8.5_PIALext$rep <- as.factor(1:10)

PIALext_Biomass <- as.data.frame(rbind(Historical_PIALext, RCP_4.5_PIALext, RCP_8.5_PIALext ))
PIALext_Biomass$species <- as.factor("PIAL")

### PIEN ###

##setting up loop objects
PIENext.years <- NULL
PIENext.reps <- NULL
PIENext.maps <- list() #final list of all scenarios


##Looping through years, reps, and scenarios to create a list of all biomass maps (change to appropriate species code throughout)
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for(k in years){
    print(paste('years=', k))  
    for (i in 1:no_reps){
      print(paste('rep=', i))
      map.PIEN <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/", "piceenge-",k, ".img", sep=""), template=ecomap)
      map.df.PIEN <- as.data.frame(map.PIEN)
      map.df.PIEN[(map.df.PIEN > 0),] <- 4
      map.df.PIEN[(map.df.PIEN == 0),] <- NA
      ext.PIEN <- as.data.frame(colSums(map.df.PIEN, na.rm = T))
      PIENext.reps <- cbind(PIENext.reps, ext.PIEN[,1]) ## binding all years
    }
    PIENext.years<- cbind(PIENext.years, PIENext.reps) #binding years across reps
    PIENext.reps <- NULL #remove previous scenario for next loop
  }
  PIENext.maps[[Scenario]] <- PIENext.years
  PIENext.years <- NULL #remove previous scenario for next loop
  
}
rm(Scenario)
rm(i)
rm(k)

Historical_PIENext <- as.data.frame(t(PIENext.maps$Historical)) #Index out each scenario
RCP_4.5_PIENext <- as.data.frame(t(PIENext.maps$RCP_4.5))
RCP_8.5_PIENext <- as.data.frame(t(PIENext.maps$RCP_8.5))

colnames(Historical_PIENext) <- "extent"
Historical_PIENext$year <- as.factor(rep(c(0,90),each = 10))
Historical_PIENext$scenario <- as.factor("Historical")
Historical_PIENext$rep <- as.factor(1:10)

colnames(RCP_4.5_PIENext) <- "extent"
RCP_4.5_PIENext$year <- as.factor(rep(c(0,90),each = 10))
RCP_4.5_PIENext$scenario <- as.factor("RCP_4.5")
RCP_4.5_PIENext$rep <- as.factor(1:10)

colnames(RCP_8.5_PIENext) <- "extent"
RCP_8.5_PIENext$year <- as.factor(rep(c(0,90),each = 10))
RCP_8.5_PIENext$scenario <- as.factor("RCP_8.5")
RCP_8.5_PIENext$rep <- as.factor(1:10)

PIENext_Biomass <- as.data.frame(rbind(Historical_PIENext, RCP_4.5_PIENext, RCP_8.5_PIENext ))
PIENext_Biomass$species <- as.factor("PIEN")


### ABLA ###

##setting up loop objects
ABLAext.years <- NULL
ABLAext.reps <- NULL
ABLAext.maps <- list() #final list of all scenarios


##Looping through years, reps, and scenarios to create a list of all biomass maps (change to appropriate species code throughout)
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for(k in years){
    print(paste('years=', k))  
    for (i in 1:no_reps){
      print(paste('rep=', i))
      map.ABLA <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/", "abielasi-",k, ".img", sep=""), template=ecomap)
      map.df.ABLA <- as.data.frame(map.ABLA)
      map.df.ABLA[(map.df.ABLA > 0),] <- 4
      map.df.ABLA[(map.df.ABLA == 0),] <- NA
      ext.ABLA <- as.data.frame(colSums(map.df.ABLA, na.rm = T))
      ABLAext.reps <- cbind(ABLAext.reps, ext.ABLA[,1]) ## binding all years
    }
    ABLAext.years<- cbind(ABLAext.years, ABLAext.reps) #binding years across reps
    ABLAext.reps <- NULL #remove previous scenario for next loop
  }
  ABLAext.maps[[Scenario]] <- ABLAext.years
  ABLAext.years <- NULL #remove previous scenario for next loop
  
}
rm(Scenario)
rm(i)
rm(k)

Historical_ABLAext <- as.data.frame(t(ABLAext.maps$Historical)) #Index out each scenario
RCP_4.5_ABLAext <- as.data.frame(t(ABLAext.maps$RCP_4.5))
RCP_8.5_ABLAext <- as.data.frame(t(ABLAext.maps$RCP_8.5))

colnames(Historical_ABLAext) <- "extent"
Historical_ABLAext$year <- as.factor(rep(c(0,90),each = 10))
Historical_ABLAext$scenario <- as.factor("Historical")
Historical_ABLAext$rep <- as.factor(1:10)

colnames(RCP_4.5_ABLAext) <- "extent"
RCP_4.5_ABLAext$year <- as.factor(rep(c(0,90),each = 10))
RCP_4.5_ABLAext$scenario <- as.factor("RCP_4.5")
RCP_4.5_ABLAext$rep <- as.factor(1:10)

colnames(RCP_8.5_ABLAext) <- "extent"
RCP_8.5_ABLAext$year <- as.factor(rep(c(0,90),each = 10))
RCP_8.5_ABLAext$scenario <- as.factor("RCP_8.5")
RCP_8.5_ABLAext$rep <- as.factor(1:10)

ABLAext_Biomass <- as.data.frame(rbind(Historical_ABLAext, RCP_4.5_ABLAext, RCP_8.5_ABLAext ))
ABLAext_Biomass$species <- as.factor("ABLA")

### LAOC ###

##setting up loop objects
LAOCext.years <- NULL
LAOCext.reps <- NULL
LAOCext.maps <- list() #final list of all scenarios


##Looping through years, reps, and scenarios to create a list of all biomass maps (change to appropriate species code throughout)
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for(k in years){
    print(paste('years=', k))  
    for (i in 1:no_reps){
      print(paste('rep=', i))
      map.LAOC <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/", "lariocci-",k, ".img", sep=""), template=ecomap)
      map.df.LAOC <- as.data.frame(map.LAOC)
      map.df.LAOC[(map.df.LAOC > 0),] <- 4
      map.df.LAOC[(map.df.LAOC == 0),] <- NA
      ext.LAOC <- as.data.frame(colSums(map.df.LAOC, na.rm = T))
      LAOCext.reps <- cbind(LAOCext.reps, ext.LAOC[,1]) ## binding all years
    }
    LAOCext.years<- cbind(LAOCext.years, LAOCext.reps) #binding years across reps
    LAOCext.reps <- NULL #remove previous scenario for next loop
  }
  LAOCext.maps[[Scenario]] <- LAOCext.years
  LAOCext.years <- NULL #remove previous scenario for next loop
  
}
rm(Scenario)
rm(i)
rm(k)

Historical_LAOCext <- as.data.frame(t(LAOCext.maps$Historical)) #Index out each scenario
RCP_4.5_LAOCext <- as.data.frame(t(LAOCext.maps$RCP_4.5))
RCP_8.5_LAOCext <- as.data.frame(t(LAOCext.maps$RCP_8.5))

colnames(Historical_LAOCext) <- "extent"
Historical_LAOCext$year <- as.factor(rep(c(0,90),each = 10))
Historical_LAOCext$scenario <- as.factor("Historical")
Historical_LAOCext$rep <- as.factor(1:10)

colnames(RCP_4.5_LAOCext) <- "extent"
RCP_4.5_LAOCext$year <- as.factor(rep(c(0,90),each = 10))
RCP_4.5_LAOCext$scenario <- as.factor("RCP_4.5")
RCP_4.5_LAOCext$rep <- as.factor(1:10)

colnames(RCP_8.5_LAOCext) <- "extent"
RCP_8.5_LAOCext$year <- as.factor(rep(c(0,90),each = 10))
RCP_8.5_LAOCext$scenario <- as.factor("RCP_8.5")
RCP_8.5_LAOCext$rep <- as.factor(1:10)

LAOCext_Biomass <- as.data.frame(rbind(Historical_LAOCext, RCP_4.5_LAOCext, RCP_8.5_LAOCext ))
LAOCext_Biomass$species <- as.factor("LAOC")

### JUOC ###

##setting up loop objects
JUOCext.years <- NULL
JUOCext.reps <- NULL
JUOCext.maps <- list() #final list of all scenarios


##Looping through years, reps, and scenarios to create a list of all biomass maps (change to appropriate species code throughout)
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for(k in years){
    print(paste('years=', k))  
    for (i in 1:no_reps){
      print(paste('rep=', i))
      map.JUOC <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","biomass/", "juniocci-",k, ".img", sep=""), template=ecomap)
      map.df.JUOC <- as.data.frame(map.JUOC)
      map.df.JUOC[(map.df.JUOC > 0),] <- 4
      map.df.JUOC[(map.df.JUOC == 0),] <- NA
      ext.JUOC <- as.data.frame(colSums(map.df.JUOC, na.rm = T))
      JUOCext.reps <- cbind(JUOCext.reps, ext.JUOC[,1]) ## binding all years
    }
    JUOCext.years<- cbind(JUOCext.years, JUOCext.reps) #binding years across reps
    JUOCext.reps <- NULL #remove previous scenario for next loop
  }
  JUOCext.maps[[Scenario]] <- JUOCext.years
  JUOCext.years <- NULL #remove previous scenario for next loop
  
}
rm(Scenario)
rm(i)
rm(k)

Historical_JUOCext <- as.data.frame(t(JUOCext.maps$Historical)) #Index out each scenario
RCP_4.5_JUOCext <- as.data.frame(t(JUOCext.maps$RCP_4.5))
RCP_8.5_JUOCext <- as.data.frame(t(JUOCext.maps$RCP_8.5))

colnames(Historical_JUOCext) <- "extent"
Historical_JUOCext$year <- as.factor(rep(c(0,90),each = 10))
Historical_JUOCext$scenario <- as.factor("Historical")
Historical_JUOCext$rep <- as.factor(1:10)

colnames(RCP_4.5_JUOCext) <- "extent"
RCP_4.5_JUOCext$year <- as.factor(rep(c(0,90),each = 10))
RCP_4.5_JUOCext$scenario <- as.factor("RCP_4.5")
RCP_4.5_JUOCext$rep <- as.factor(1:10)

colnames(RCP_8.5_JUOCext) <- "extent"
RCP_8.5_JUOCext$year <- as.factor(rep(c(0,90),each = 10))
RCP_8.5_JUOCext$scenario <- as.factor("RCP_8.5")
RCP_8.5_JUOCext$rep <- as.factor(1:10)

JUOCext_Biomass <- as.data.frame(rbind(Historical_JUOCext, RCP_4.5_JUOCext, RCP_8.5_JUOCext ))
JUOCext_Biomass$species <- as.factor("JUOC")


All_Biomass_ext <- as.data.frame(rbind(PIPOext_Biomass, ABGRext_Biomass, PSMEext_Biomass, PICOext_Biomass, JUOCext_Biomass, LAOCext_Biomass, PIENext_Biomass, ABLAext_Biomass, 
                                   PIALext_Biomass))


All_Biomass_ext_0 <- as.data.frame(All_Biomass_ext[(All_Biomass_ext$year == 0),])
All_Biomass_ext_90 <- as.data.frame(All_Biomass_ext[(All_Biomass_ext$year == 90),])
All_Biomass_ext_change <- as.data.frame(All_Biomass_ext_90$extent - All_Biomass_ext_0$extent)
All_Biomass_ext_change$species <- All_Biomass_ext_90$species
All_Biomass_ext_change$scenario <- All_Biomass_ext_90$scenario
colnames(All_Biomass_ext_change)[1] <- "extent"

Percent_Change_ext <- as.data.frame((All_Biomass_ext_90$extent - All_Biomass_ext_0$extent)/All_Biomass_ext_0$extent)
Percent_Change_ext$extent <- (round(Percent_Change_ext[,1],2)*100)
Percent_Change_ext$species <- All_Biomass_ext_90$species
Percent_Change_ext$scenario <- All_Biomass_ext_90$scenario


Mean_extent_across_reps <- group_by(All_Biomass_ext, scenario, species, year, add = T) #Overall mean & sd
summarise_mean_extent_across_reps <- summarise(Mean_extent_across_reps, extent = mean(extent, na.rm = T), sd = sd(extent, na.rm = T))
summarise_mean_extent_across_reps

Mean_extent_across_reps_0 <- group_by(All_Biomass_ext_0, scenario, species, add = T) #Overall mean & sd
summarise_mean_extent_across_reps_0 <- summarise(Mean_extent_across_reps_0, extent_ha = mean(extent), sd = sd(extent))
summarise_mean_extent_across_reps_0

Mean_extent_across_reps_90 <- group_by(All_Biomass_ext_90, scenario, species, add = T) #Overall mean & sd
Mean_extent_across_reps_90$prop <- Mean_extent_across_reps_90$extent/938786
summarise_mean_extent_across_reps_90 <- summarise(Mean_extent_across_reps_90, extent_ha = mean(extent), sd = sd(extent))
summarise_mean_extent_across_reps_90
summarise_mean_extent_across_reps_90$extent_prop <- summarise_mean_extent_across_reps_90$extent_ha/938786 
summarise_mean_extent_across_reps_90$sd_prop <- summarise_mean_extent_across_reps_90$sd/938786
write.csv(summarise_mean_extent_across_reps_90, "Z:/ClimateChange_Results_Oct6_2017/Biomass_Data/Extent_Year_90.csv", row.names = F)

Mean_extent_change <- group_by(All_Biomass_ext_change, scenario, species, add = T)

colors <- rep(c("blue", "orange", "red"), times = 3)
speciesNames <- c("Ponderosa Pine", "Grand Fir", "Douglas Fir", "Lodgepole Pine", "Western Juniper", "Western Larch",  "Sub-alpine Fir", "Engelmann Spruce",  "Whitebark Pine")
boxplot(prop ~ scenario + species, data = Mean_extent_across_reps_90, las=2, col = colors)



boxplot(extent ~ scenario + species, data = Mean_extent_change, las=1, col = colors, main = "Average Change in Species Extent", 
        ylab = "Extent (ha)", names = speciesNames, at =c(1,2,3, 5,6,7, 9,10,11, 13,14,15, 17,18,19, 21,22,23, 25,26,27, 29,30,31, 33,34,35))
abline(h=0)

par(mar = c(8.6, 4.1, 4.1, 2.1))
boxplot(extent ~ scenario + species, data = Percent_Change_ext, col = colors, main = "Average % Change in Species Extent", 
        ylab = "% change", font.axis = 2, at =c(1,2,3, 5,6,7, 9,10,11, 13,14,15, 17,18,19, 21,22,23, 25,26,27, 29,30,31, 33,34,35),
        xaxt = "n", yaxt = "n")
abline(h=0)
axis(1, at = c(2,6,10,14,18,22,26,30,34), labels = speciesNames, las = 2, font = 2)
axis(2, at = c(-75,-50, -25, 0, 25, 50, 75, 100, 125, 150), font = 2, ylab = "% change")
legend("bottomleft", c("Historical", "RCP 4.5", "RCP 8.5"), fill = colors )

par(mar = c(8.6, 6.1, 4.1, 2.1))
boxplot(log(extent) ~ scenario + species, data = Mean_extent_change, col = colors, main = "Average Change in Species Extent", 
        ylab = "Change in Extent (ha)", font.axis = 2, at =c(1,2,3, 5,6,7, 9,10,11, 13,14,15, 17,18,19, 21,22,23, 25,26,27, 29,30,31, 33,34,35),
        xaxt = "n")
abline(h=0)
axis(1, at = c(2,6,10,14,18,22,26,30,34), labels = speciesNames, las = 2, font = 2)
legend("bottomleft", c("Historical", "RCP 4.5", "RCP 8.5"), fill = colors )




