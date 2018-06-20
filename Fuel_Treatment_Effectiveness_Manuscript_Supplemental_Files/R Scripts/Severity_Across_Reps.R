#### Project: Optimizing placement of fuel treatments and accelerating prescribed fire 
#### ameliorates extreme weather-driven wildfires in western mixed-conifer forests by 
#### Brooke A. Cassell, Portland State University, Dissertation defended February 2018


#### This script is for calculating severity across treatment and weather scenarios (Excludes Riparian scenarios)
### Includes only forested areas of the landscape (excludes grasslands & shrublands, which nearly always burn at high severity)
#Written by Brooke Cassell January 2018

w.dir <- "Z:/Treatments_Results_Oct9_2017/"

library(raster)
library(rgdal)
library(viridis)
library(readr)

#Load initial communities map to identify forested cells
ICmap <- raster("Z:/Treatments_Results_Oct9_2017/BAU_Historical/replicate1/mapcodes4ha1.img") #Read in Initial Communities map so forested cells can be identified & overlaid
plot(ICmap)
forestmap <- ICmap
forestmap[(forestmap < 100)] <- NA #convert non-forest to NA

years <- 100
timesteps <- 1:100 # input number of timesteps (years in which fire maps are created) here
no_reps <- 10  # input nummber of replicates here

## scenario list w/out Riparian scenarios
scenario <- c("Untreated_Historical", "Untreated_Extreme", "BAU_Historical", "Opt_BAU_Historical",
              "RxFire_Historical", "Opt_RxFire_Historical", "RxFire3x_Historical", "Opt_RxFire3x_Historical", "BAU_Extreme", "Opt_BAU_Extreme", 
              "RxFire_Extreme", "Opt_RxFire_Extreme", "RxFire3x_Extreme", "Opt_RxFire3x_Extreme")


seq.cols <- cbind(1:100, 101:200, 201:300, 301:400, 401:500, 501:600, 601:700, 701:800, 801:900, 901:1000)

ecomap <- raster("Z:/Treatments_Results_Oct9_2017/BAU_Historical/replicate1/ecoregions4ha1.img") #read in the map of ecoregions to use as a mask
plot(ecomap) #plot to make sure it looks right
ecomap[(ecomap ==0)] <- NA #change cells with value = 0 to NA. Since all active cells have an ecoregion value, this will give a mask for inactive cells to fire maps

ICmap <- raster("Z:/Treatments_Results_Oct9_2017/BAU_Historical/replicate1/mapcodes4ha1.img") #Read in Initial Communities map so forested cells can be identified & overlaid
plot(ICmap)
forestmap <- ICmap
forestmap[(forestmap < 100)] <- NA

##setting up loop objects
burn.years <- matrix(ncol=100, nrow=435000)
burn.means <- NULL
burn.maps <- list() #final list of all scenarios

##Looping through years, reps, and scenarios to create a list of all fire maps
for (Scenario in scenario){
  print(paste('scenario=', Scenario))
  for (i in 1:no_reps){
    print(paste('rep=', i))
    for(k in 1:years){
      print(paste('years=', k))
      map.select <- raster(paste(w.dir, Scenario, "/", "replicate", i, "/","fire/", "severity-", k, ".img", sep=""))
      map.df <- as.data.frame((map.select)-2) #reclassify cells as -2=inactive, -1=unburned, 0=burned but no mortality, and 1-5 severities
      map.df[map.df == -1] <- NA #classify unburned cells as NA
      map.matrix <- as.matrix(map.df)
      map.all <- raster(map.matrix, template = forestmap)
      map.mask <- mask(map.all, forestmap) #convert non-forest and inactive cells to NA
      map.df <- as.data.frame(map.mask)
      burn.years[,k] <- map.df[,1] ## binding all years
    }
    rep.mean <- matrix(rowMeans(burn.years, na.rm=T)) #average across all years for the rep
    burn.means <- cbind(burn.means, rep.mean)
  }
  burn.maps[[Scenario]] <- burn.means
  
}
rm(Scenario)
rm(i)
rm(k)

Untreated_hist.severity <- burn.maps$Untreated_Historical
Untreated_ext.severity <- burn.maps$Untreated_Extreme[,11:20] 

BAU_hist.severity <- burn.maps$BAU_Historical[,21:30]
Opt_BAU_hist.severity <- burn.maps$Opt_BAU_Historical[,31:40]
BAU_ext.severity <- burn.maps$BAU_Extreme[,81:90]
Opt_BAU_ext.severity <- burn.maps$Opt_BAU_Extreme[,91:100]

RxFire_hist.severity <- burn.maps$RxFire_Historical[,41:50]
Opt_RxFire_hist.severity <- burn.maps$Opt_RxFire_Historical[,51:60]
RxFire_ext.severity <- burn.maps$RxFire_Extreme[,101:110] 
Opt_RxFire_ext.severity <- burn.maps$Opt_RxFire_Extreme[,111:120] 

RxFire3x_hist.severity <- burn.maps$RxFire3x_Historical[,61:70]
Opt_RxFire3x_hist.severity <- burn.maps$Opt_RxFire3x_Historical[,71:80]
RxFire3x_ext.severity <- burn.maps$RxFire3x_Extreme[,121:130] 
Opt_RxFire3x_ext.severity <- burn.maps$Opt_RxFire3x_Extreme[,131:140] 

rm(burn.means) #remove the loop files to free up memory
rm(burn.years)
rm(burn.maps)



Untreated_hist.by_rep <- data.frame(colMeans(Untreated_hist.severity, na.rm = T))
BAU_hist.by_rep <- data.frame(colMeans(BAU_hist.severity, na.rm = T))
Opt_BAU_hist.by_rep <- data.frame(colMeans(Opt_BAU_hist.severity, na.rm = T))
RxFire_hist.by_rep <- data.frame(colMeans(RxFire_hist.severity, na.rm = T))
Opt_RxFire_hist.by_rep <- data.frame(colMeans(Opt_RxFire_hist.severity, na.rm = T))
RxFire3x_hist.by_rep <- data.frame(colMeans(RxFire3x_hist.severity, na.rm = T))
Opt_RxFire3x_hist.by_rep <- data.frame(colMeans(Opt_RxFire3x_hist.severity, na.rm = T))

###

Untreated_ext.by_rep <- data.frame(colMeans(Untreated_ext.severity, na.rm = T))
BAU_ext.by_rep <- data.frame(colMeans(BAU_ext.severity, na.rm = T))
Opt_BAU_ext.by_rep <- data.frame(colMeans(Opt_BAU_ext.severity, na.rm = T))
RxFire_ext.by_rep <- data.frame(colMeans(RxFire_ext.severity, na.rm = T))
Opt_RxFire_ext.by_rep <- data.frame(colMeans(Opt_RxFire_ext.severity, na.rm = T))
RxFire3x_ext.by_rep <- data.frame(colMeans(RxFire3x_ext.severity, na.rm = T))
Opt_RxFire3x_ext.by_rep <- data.frame(colMeans(Opt_RxFire3x_ext.severity, na.rm = T))

colnames(Untreated_hist.by_rep)[1]<- "mean_severity"
Untreated_hist.by_rep$scenario <- "Untreated_Historical"
Untreated_hist.by_rep$rep <- as.factor(c(1:10))
Untreated_hist.by_rep$harvest <- "Untreated"
Untreated_hist.by_rep$climate <- "Historical"

colnames(BAU_hist.by_rep)[1]<- "mean_severity"
BAU_hist.by_rep$scenario <- "BAU_Historical"
BAU_hist.by_rep$rep <- as.factor(c(1:10))
BAU_hist.by_rep$harvest <- "BAU"
BAU_hist.by_rep$climate <- "Historical"

colnames(Opt_BAU_hist.by_rep)[1]<- "mean_severity"
Opt_BAU_hist.by_rep$scenario <- "Opt_BAU_Historical"
Opt_BAU_hist.by_rep$rep <- as.factor(c(1:10))
Opt_BAU_hist.by_rep$harvest <- "Opt_BAU"
Opt_BAU_hist.by_rep$climate <- "Historical"

colnames(RxFire_hist.by_rep)[1]<- "mean_severity"
RxFire_hist.by_rep$scenario <- "RxFire_Historical"
RxFire_hist.by_rep$rep <- as.factor(c(1:10))
RxFire_hist.by_rep$harvest <- "RxFire"
RxFire_hist.by_rep$climate <- "Historical"

colnames(Opt_RxFire_hist.by_rep)[1]<- "mean_severity"
Opt_RxFire_hist.by_rep$scenario <- "Opt_RxFire_Historical"
Opt_RxFire_hist.by_rep$rep <- as.factor(c(1:10))
Opt_RxFire_hist.by_rep$harvest <- "Opt_RxFire"
Opt_RxFire_hist.by_rep$climate <- "Historical"

colnames(RxFire3x_hist.by_rep)[1]<- "mean_severity"
RxFire3x_hist.by_rep$scenario <- "RxFire3x_Historical"
RxFire3x_hist.by_rep$rep <- as.factor(c(1:10))
RxFire3x_hist.by_rep$harvest <- "RxFire3x"
RxFire3x_hist.by_rep$climate <- "Historical"

colnames(Opt_RxFire3x_hist.by_rep)[1]<- "mean_severity"
Opt_RxFire3x_hist.by_rep$scenario <- "Opt_RxFire3x_Historical"
Opt_RxFire3x_hist.by_rep$rep <- as.factor(c(1:10))
Opt_RxFire3x_hist.by_rep$harvest <- "Opt_RxFire3x"
Opt_RxFire3x_hist.by_rep$climate <- "Historical"

###

colnames(Untreated_ext.by_rep)[1]<- "mean_severity"
Untreated_ext.by_rep$scenario <- "Untreated_Extreme"
Untreated_ext.by_rep$rep <- as.factor(c(1:10))
Untreated_ext.by_rep$harvest <- "Untreated"
Untreated_ext.by_rep$climate <- "Extreme"

colnames(BAU_ext.by_rep)[1]<- "mean_severity"
BAU_ext.by_rep$scenario <- "BAU_Extreme"
BAU_ext.by_rep$rep <- as.factor(c(1:10))
BAU_ext.by_rep$harvest <- "BAU"
BAU_ext.by_rep$climate <- "Extreme"

colnames(Opt_BAU_ext.by_rep)[1]<- "mean_severity"
Opt_BAU_ext.by_rep$scenario <- "Opt_BAU_Extreme"
Opt_BAU_ext.by_rep$rep <- as.factor(c(1:10))
Opt_BAU_ext.by_rep$harvest <- "Opt_BAU"
Opt_BAU_ext.by_rep$climate <- "Extreme"

colnames(RxFire_ext.by_rep)[1]<- "mean_severity"
RxFire_ext.by_rep$scenario <- "RxFire_Extreme"
RxFire_ext.by_rep$rep <- as.factor(c(1:10))
RxFire_ext.by_rep$harvest <- "RxFire"
RxFire_ext.by_rep$climate <- "Extreme"

colnames(Opt_RxFire_ext.by_rep)[1]<- "mean_severity"
Opt_RxFire_ext.by_rep$scenario <- "Opt_RxFire_Extreme"
Opt_RxFire_ext.by_rep$rep <- as.factor(c(1:10))
Opt_RxFire_ext.by_rep$harvest <- "Opt_RxFire"
Opt_RxFire_ext.by_rep$climate <- "Extreme"

colnames(RxFire3x_ext.by_rep)[1]<- "mean_severity"
RxFire3x_ext.by_rep$scenario <- "RxFire3x_Extreme"
RxFire3x_ext.by_rep$rep <- as.factor(c(1:10))
RxFire3x_ext.by_rep$harvest <- "RxFire3x"
RxFire3x_ext.by_rep$climate <- "Extreme"

colnames(Opt_RxFire3x_ext.by_rep)[1]<- "mean_severity"
Opt_RxFire3x_ext.by_rep$scenario <- "Opt_RxFire3x_Extreme"
Opt_RxFire3x_ext.by_rep$rep <- as.factor(c(1:10))
Opt_RxFire3x_ext.by_rep$harvest <- "Opt_RxFire3x"
Opt_RxFire3x_ext.by_rep$climate <- "Extreme"

###

Rep_Severity_Historical <- rbind(Untreated_hist.by_rep, BAU_hist.by_rep, Opt_BAU_hist.by_rep,
                                 RxFire_hist.by_rep, Opt_RxFire_hist.by_rep, RxFire3x_hist.by_rep, 
                                 Opt_RxFire3x_hist.by_rep)
write.csv(Rep_Severity_Historical, "Z:/Treatments_Results_Oct9_2017/Fire_Severity_Data/Severity_by_Rep_Historical_Forest.csv", row.names = F)


Rep_Severity_Extreme <- rbind(Untreated_ext.by_rep, BAU_ext.by_rep, Opt_BAU_ext.by_rep,
                                 RxFire_ext.by_rep, Opt_RxFire_ext.by_rep, RxFire3x_ext.by_rep, 
                                 Opt_RxFire3x_ext.by_rep)

write.csv(Rep_Severity_Extreme, "Z:/Treatments_Results_Oct9_2017/Fire_Severity_Data/Severity_by_Rep_Extreme_Forest.csv", row.names = F)


############ Statistical tests

#boxplots to visualize
hist_by_scenario <- group_by(Rep_Severity_Historical, harvest, add = TRUE)
hist_area_by_scenario <- summarise(hist_by_scenario, Severity = mean(mean_severity, na.rm = T))
hist_area_by_scenario <- as.data.frame(hist_area_by_scenario)

hist_by_scenario$harvest<- factor(hist_by_scenario$harvest, levels=c("Untreated", "BAU", "Opt_BAU", "RxFire", "Opt_RxFire", "RxFire3x", "Opt_RxFire3x"))

par(mfrow=c(1,1), xpd = NA, cex.axis = 0.8)
boxplot(hist_by_scenario$mean_severity ~ hist_by_scenario$harvest, las=2,
        ylab = "Severity Index")
text(x = 1, y = , 3.55, paste0(2.97))
text(x = 2, y = , 3.55, paste0(2.80))
text(x = 3, y = , 3.55, paste0(2.92))
text(x = 4, y = , 3.55, paste0(2.87))
text(x = 5, y = , 3.55, paste0(2.95))
text(x = 6, y = , 3.55, paste0(2.84))
text(x = 7, y = , 3.55, paste0(2.93))
title(main = "Fire Severity - Historical Weather", line = 2)

ext_by_scenario <- group_by(Rep_Severity_Extreme, harvest, add = TRUE)
ext_area_by_scenario <- summarise(ext_by_scenario, Severity = mean(mean_severity, na.rm = T))
ext_area_by_scenario <- as.data.frame(ext_area_by_scenario)

ext_by_scenario$harvest<- factor(ext_by_scenario$harvest, levels=c("Untreated", "BAU", "Opt_BAU", "RxFire", "Opt_RxFire", "RxFire3x", "Opt_RxFire3x"))

par(mfrow=c(1,1), xpd = NA, cex.axis = 0.8)
boxplot(ext_by_scenario$mean_severity ~ ext_by_scenario$harvest, las=2,
        ylab = "Severity Index")
text(x = 1, y = , 4.5, paste0(4.34))
text(x = 2, y = , 4.5, paste0(3.93))
text(x = 3, y = , 4.5, paste0(3.87))
text(x = 4, y = , 4.5, paste0(3.94))
text(x = 5, y = , 4.5, paste0(3.88))
text(x = 6, y = , 4.5, paste0(3.87))
text(x = 7, y = , 4.5, paste0(3.93))
title(main = "Fire Severity - Extreme Weather", line = 2)


#Test for normality of the log-transformed data
shapiro.test(hist_by_scenario$mean_severity) #W = 0.98, p = 0.39, data are normally distributed
shapiro.test(ext_by_scenario$mean_severity) #W = 0.80, p = 2.416e-08, data are NOT normally distributed

#Test for homoscedasticity of the log-transformed data
library(car)
leveneTest(hist_by_scenario$mean_severity ~ hist_by_scenario$harvest) #F = 1.31 p = 0.27, equal variances
leveneTest(ext_by_scenario$mean_severity ~ ext_by_scenario$harvest) #F = 0.69p = 0.66, equal variances

a <- aov(hist_by_scenario$mean_severity ~ hist_by_scenario$harvest)
summary(a) #F = 1.59, p = 0.165 - no significant difference

kruskal.test(ext_by_scenario$mean_severity ~ ext_by_scenario$harvest) #chi-squared = 28.91, p = 6.335e-05 - YES significant difference among treatments
dunn.test(ext_by_scenario$mean_severity, ext_by_scenario$harvest) #all treatments dif from Untreated. No other differences.
