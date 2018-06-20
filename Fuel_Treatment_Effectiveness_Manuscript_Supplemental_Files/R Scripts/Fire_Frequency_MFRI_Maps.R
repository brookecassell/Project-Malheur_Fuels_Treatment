#### Project: Optimizing placement of fuel treatments and accelerating prescribed fire 
#### ameliorates extreme weather-driven wildfires in western mixed-conifer forests by 
#### Brooke A. Cassell, Portland State University, Dissertation defended February 2018


#This script calculates frequency and probability of fire for each cell on the landscape.
#Written by Brooke Cassell October 2017

w.dir <- "W:/Treatments_Results_Oct9_2017/"

library(raster)
library(rgdal)
library(viridis)

ecomap <- raster("W:/Treatments_Results_Oct9_2017/BAU_Historical/replicate1/ecoregions4ha1.img") #read in the map of ecoregions to use as a mask
plot(ecomap) #plot to make sure it looks right
ecomap[(ecomap ==0)] <- NA #change cells with value = 0 to NA. Since all active cells have an ecoregion value, this will give a mask for inactive cells to fire maps


years <- 100
timesteps <- 1:100 # input number of timesteps (years in which fire maps are created) here
no_reps <- 10  # input nummber of replicates here
#sta <- stack()
#sta2<- stack()
scenario <- c("Untreated_Historical", "Untreated_Extreme", "BAU_Historical", "Opt_BAU_Historical",
              "RxFire_Historical", "Opt_RxFire_Historical", "RxFire3x_Historical", "Opt_RxFire3x_Historical", "BAU_Extreme", "Opt_BAU_Extreme", 
              "RxFire_Extreme", "Opt_RxFire_Extreme", "RxFire3x_Extreme", "Opt_RxFire3x_Extreme")

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
      #map.df[(map.df < 2),] <- 0 ##Reclassifying inactive and unburned as 0
      map.df[(map.df == 0),] <- -1 ##Reclassifying inactive cells as -1 (will change to NA later, can't do that in the loop)
      map.df[(map.df == 1),] <- 0 ##Reclassifying unburned cells as 0
      map.df[(map.df > 1),] <- 1 ##Reclassifying any cells w fire as 1
      burn.years[,k] <- map.df[,1] ## binding all years
    }
    burn.reps[,seq.cols[,i]] <- burn.years #binding years across reps
  }
  burn.maps[[Scenario]] <- burn.reps
  
}
rm(Scenario)
rm(i)
rm(k)

burn.maps$Untreated_Historical[burn.maps$Untreated_Historical == -1] <- NA #convert inactive cells to NA
burn.maps$BAU_Historical[burn.maps$BAU_Historical == -1] <- NA 
burn.maps$Opt_BAU_Historical[burn.maps$Opt_BAU_Historical == -1] <- NA 
burn.maps$RxFire_Historical[burn.maps$RxFire_Historical == -1] <- NA 
burn.maps$Opt_RxFire_Historical[burn.maps$Opt_RxFire_Historical == -1] <- NA 
burn.maps$RxFire3x_Historical[burn.maps$RxFire3x_Historical == -1] <- NA 
burn.maps$Opt_RxFire3x_Historical[burn.maps$Opt_RxFire3x_Historical == -1] <- NA 

burn.maps$Untreated_Extreme[burn.maps$Untreated_Extreme == -1] <- NA #convert inactive cells to NA
burn.maps$BAU_Extreme[burn.maps$BAU_Extreme == -1] <- NA 
burn.maps$Opt_BAU_Extreme[burn.maps$Opt_BAU_Extreme == -1] <- NA 
burn.maps$RxFire_Extreme[burn.maps$RxFire_Extreme == -1] <- NA 
burn.maps$Opt_RxFire_Extreme[burn.maps$Opt_RxFire_Extreme == -1] <- NA 
burn.maps$RxFire3x_Extreme[burn.maps$RxFire3x_Extreme == -1] <- NA 
burn.maps$Opt_RxFire3x_Extreme[burn.maps$Opt_RxFire3x_Extreme == -1] <- NA


###### Frequency data - Sum rows for total # of times each cell burned over all years and reps
Untreated_hist.burn <- rowSums(burn.maps$Untreated_Historical, na.rm = T)
sum(Untreated_hist.burn)
mean(Untreated_hist.burn)
table(Untreated_hist.burn) #table of cumulative burns per site
Untreated_hist.burn <- as.matrix(Untreated_hist.burn)
Untreated_hist.raster <- raster(Untreated_hist.burn, template=map.select)

BAU_hist.burn <- rowSums(burn.maps$BAU_Historical, na.rm = T)
sum(BAU_hist.burn)
table(BAU_hist.burn) #table of cumulative burns per site
BAU_hist.burn <- as.matrix(BAU_hist.burn)
BAU_hist.raster <- raster(BAU_hist.burn, template=map.select)

Opt_BAU_hist.burn <- rowSums(burn.maps$Opt_BAU_Historical, na.rm = T)
sum(Opt_BAU_hist.burn)
table(Opt_BAU_hist.burn) #table of cumulative burns per site
Opt_BAU_hist.burn <- as.matrix(Opt_BAU_hist.burn)
Opt_BAU_hist.raster <- raster(Opt_BAU_hist.burn, template=map.select)

RxFire_hist.burn <- rowSums(burn.maps$RxFire_Historical, na.rm = T)
sum(RxFire_hist.burn)
table(RxFire_hist.burn) #table of cumulative burns per site
RxFire_hist.burn <- as.matrix(RxFire_hist.burn)
RxFire_hist.raster <- raster(RxFire_hist.burn, template=map.select)

Opt_RxFire_hist.burn <- rowSums(burn.maps$Opt_RxFire_Historical, na.rm = T)
sum(Opt_RxFire_hist.burn)
table(Opt_RxFire_hist.burn) #table of cumulative burns per site
Opt_RxFire_hist.burn <- as.matrix(Opt_RxFire_hist.burn)
Opt_RxFire_hist.raster <- raster(Opt_RxFire_hist.burn, template=map.select)

RxFire3x_hist.burn <- rowSums(burn.maps$RxFire3x_Historical, na.rm = T)
sum(RxFire3x_hist.burn)
table(RxFire3x_hist.burn) #table of cumulative burns per site
RxFire3x_hist.burn <- as.matrix(RxFire3x_hist.burn)
RxFire3x_hist.raster <- raster(RxFire3x_hist.burn, template=map.select)


Opt_RxFire3x_hist.burn <- rowSums(burn.maps$Opt_RxFire3x_Historical, na.rm = T)
sum(Opt_RxFire3x_hist.burn)
table(Opt_RxFire6x_hist.burn) #table of cumulative burns per site
Opt_RxFire3x_hist.burn <- as.matrix(Opt_RxFire3x_hist.burn)
Opt_RxFire3x_hist.raster <- raster(Opt_RxFire3x_hist.burn, template=map.select)



Untreated_ext.burn <- rowSums(burn.maps$Untreated_Extreme, na.rm = T)
sum(Untreated_ext.burn)
table(Untreated_ext.burn) #table of cumulative burns per site
Untreated_ext.burn <- as.matrix(Untreated_ext.burn)
Untreated_ext.raster <- raster(Untreated_ext.burn, template=map.select)

BAU_ext.burn <- rowSums(burn.maps$BAU_Extreme, na.rm = T)
sum(BAU_ext.burn)
table(BAU_ext.burn) #table of cumulative burns per site
BAU_ext.burn <- as.matrix(BAU_ext.burn)
BAU_ext.raster <- raster(BAU_ext.burn, template=map.select)

Opt_BAU_ext.burn <- rowSums(burn.maps$Opt_BAU_Extreme, na.rm = T)
sum(Opt_BAU_ext.burn)
table(Opt_BAU_ext.burn) #table of cumulative burns per site
Opt_BAU_ext.burn <- as.matrix(Opt_BAU_ext.burn)
Opt_BAU_ext.raster <- raster(Opt_BAU_ext.burn, template=map.select)

RxFire_ext.burn <- rowSums(burn.maps$RxFire_Extreme, na.rm = T)
sum(RxFire_ext.burn)
table(RxFire_ext.burn) #table of cumulative burns per site
RxFire_ext.burn <- as.matrix(RxFire_ext.burn)
RxFire_ext.raster <- raster(RxFire_ext.burn, template=map.select)

Opt_RxFire_ext.burn <- rowSums(burn.maps$Opt_RxFire_Extreme, na.rm = T)
sum(Opt_RxFire_ext.burn)
table(Opt_RxFire_ext.burn) #table of cumulative burns per site
Opt_RxFire_ext.burn <- as.matrix(Opt_RxFire_ext.burn)
Opt_RxFire_ext.raster <- raster(Opt_RxFire_ext.burn, template=map.select)

RxFire3x_ext.burn <- rowSums(burn.maps$RxFire3x_Extreme, na.rm = T)
sum(RxFire3x_ext.burn)
table(RxFire3x_ext.burn) #table of cumulative burns per site
RxFire3x_ext.burn <- as.matrix(RxFire3x_ext.burn)
RxFire3x_ext.raster <- raster(RxFire3x_ext.burn, template=map.select)

Opt_RxFire3x_ext.burn <- rowSums(burn.maps$Opt_RxFire3x_Extreme, na.rm = T)
sum(Opt_RxFire3x_ext.burn)
table(Opt_RxFire3x_ext.burn) #table of cumulative burns per site
Opt_RxFire3x_ext.burn <- as.matrix(Opt_RxFire3x_ext.burn)
Opt_RxFire3x_ext.raster <- raster(Opt_RxFire3x_ext.burn, template=map.select)

Cells_Burned_All_Years <- cbind(Untreated_hist.burn, BAU_hist.burn, Opt_BAU_hist.burn,
                                RxFire_hist.burn, Opt_RxFire_hist.burn, 
                                RxFire6x_hist.burn, Opt_RxFire6x_hist.burn,
                                Untreated_ext.burn, BAU_ext.burn, Opt_BAU_ext.burn,
                                RxFire_ext.burn, Opt_RxFire_ext.burn) 
colnames(Cells_Burned_All_Years) <- c("Untreated_Historical", "BAU_Historical", "Opt_BAU_Historical",
                                      "RxFire_Historical", "Opt_RxFire_Historical",
                                      "RxFire3x_Historical", "Opt_RxFire3x_Historical",
                                      "Untreated_Extreme", "BAU_Extreme", "Opt_BAU_Extreme",
                                      "RxFire_Extreme", "Opt_RxFire_Extreme",
                                      "RxFire3x_Extreme", "Opt_RxFire3x_Extreme")
write.csv(Cells_Burned_All_Years,"W:/Treatments_Results_Oct9_2017/Fire_Frequency_Data/Total_Burns_Per_Cell.csv") 

###### plot fire frequency #######

par(mfrow=c(2,2), mar = c(1,1,1,1), oma = c(0,0,0,1), xpd = NA)

plot(Untreated_hist.raster, col = rev(inferno(10, begin = .1, end = .95)),zlim = c(1,145), box = F, axes = F, legend = F)
#title("Untreated_Historical", line = .1)
plot.new()


plot(BAU_hist.raster, col = rev(inferno(10, begin = .1, end = .95)),zlim = c(1,145), box = F, axes = F, legend=F)
#title("BAU_Historical", line = .1)
plot(Opt_BAU_hist.raster, col = rev(inferno(10, begin = .1, end = .95)),zlim = c(1,145), box = F, axes = F, legend=F)
#title("Opt_BAU_Historical", line = .1)

plot(RxFire_hist.raster, col = rev(inferno(10, begin = .1, end = .95)),zlim = c(1,145), box = F, axes = F, legend=F)
#title("RxFire_Historical", line = .1)
plot(Opt_RxFire_hist.raster, col = rev(inferno(10, begin = .1, end = .95)),zlim = c(1,145), box = F, axes = F, legend=F)
#title("Opt_RxFire_Historical", line = .1)

plot(RxFire6x_hist.raster, col = rev(inferno(10, begin = .1, end = .95)),zlim = c(1,145), box = F, axes = F, legend=F)
#title("RxFire3x_Historical", line = .1)
plot(Opt_RxFire6x_hist.raster, col = rev(inferno(10, begin = .1, end = .95)),zlim = c(1,145), box = F, axes = F, legend=F)
#title("Opt_RxFire3x_Historical", line = .1)

#title(main = "Fire Frequency - Historical Weather", outer = TRUE, line = -3, cex.main = 2.0)


par(mfrow=c(2,2), mar = c(1,1,1,1), oma = c(0,0,0,1), xpd = NA)

plot(Untreated_ext.raster, col = rev(inferno(10, begin = .1, end = .95)),zlim = c(1,145), box = F, axes = F, legend = F)
#title("Untreated_Extreme", line = .1)
plot.new()

plot(BAU_ext.raster, col = rev(inferno(10, begin = .1, end = .95)),zlim = c(1,145), box = F, axes = F, legend=F)
#title("BAU_Extreme", line = .1)
plot(Opt_BAU_ext.raster, col = rev(inferno(10, begin = .1, end = .95)),zlim = c(1,145), box = F, axes = F, legend = F)
#title("Opt_BAU_Extreme", line = .1)

plot(RxFire_ext.raster, col = rev(inferno(10, begin = .1, end = .95)),zlim = c(1,145), box = F, axes = F, legend=F)
#("RxFire_Extreme", line = .1)
plot(Opt_RxFire_ext.raster, col = rev(inferno(10, begin = .1, end = .95)),zlim = c(1,145), box = F, axes = F, legend = F)
#title("Opt_RxFire_Extreme", line = .1)

plot(RxFire6x_ext.raster, col = rev(inferno(10, begin = .1, end = .95)),zlim = c(1,145), box = F, axes = F, legend=F)
#title("RxFire3x_Extreme", line = .1)
plot(Opt_RxFire6x_ext.raster, col = rev(inferno(10, begin = .1, end = .95)),zlim = c(1,145), box = F, axes = F, legend = F)
#title("Opt_RxFire3x_Extreme", line = .1)

#title(main = "Fire Frequency - Extreme Weather", outer = TRUE, line = -3, cex.main = 2.0)
#plot a single raster with the legend, then export really large and crop to give one really big legend for the final graphic.
par(mfrow=c(1,1))
plot(Opt_RxFire6x_ext.raster, col = rev(inferno(10, begin = .1, end = .95)), zlim = c(1,145), box = F, axes = F, legend = T)

############ Annual Area Burned ######################

#retrieve full matrix for each scenario (1 for fire, 0 for unburned) 
untreated.burned <- burn.maps$Untreated_Historical
untreated.ext.burned <- burn.maps$Untreated_Extreme
BAU.hist.burned <- burn.maps$BAU_Historical
Opt_BAU.hist.burned <- burn.maps$Opt_BAU_Historical
BAU.ext.burned <- burn.maps$BAU_Extreme
Opt_BAU.ext.burned <- burn.maps$Opt_BAU_Extreme
RxFire.hist.burned <- burn.maps$RxFire_Historical
Opt_RxFire.hist.burned <- burn.maps$Opt_RxFire_Historical
RxFire.ext.burned <- burn.maps$RxFire_Extreme
Opt_RxFire.ext.burned <- burn.maps$Opt_RxFire_Extreme
RxFire3x.hist.burned <- burn.maps$RxFire3x_Historical
Opt_RxFire3x.hist.burned <- burn.maps$Opt_RxFire3x_Historical
RxFire3x.ext.burned <- burn.maps$RxFire3x_Extreme
Opt_RxFire3x.ext.burned <- burn.maps$Opt_RxFire3x_Extreme



untreated.burned.cell.sums <- as.data.frame(colSums(untreated.burned, na.rm = T))
#dim(untreated.burned.cell.sums) <- c(1000,1)
table(untreated.burned.cell.sums[1])
min(untreated.burned.cell.sums)
max(untreated.burned.cell.sums[,1])*4 #most ha burned in a year
colMeans(untreated.burned.cell.sums)*4 #mean ha burned/year
#rm(untreated.burned)

untreated.burned.cell.sums$scenario <- as.factor("Untreated_Historical")
untreated.burned.cell.sums$climate <- as.factor("Historical")
untreated.burned.cell.sums$harvest <- as.factor("Untreated")
untreated.burned.cell.sums$year <- as.factor(1:100) #add year numbers for paired comparisons
colnames(untreated.burned.cell.sums)[1]<- "Area Burned"

## Area Burned Metrics - UNTREATED HISTORICAL 
# Minimum - 0 cells/ha burned: 113 years out of 1000
# Maximum - 445,920 ha: 1 year out of 1000
# Years with > 100,000 acres/40,000ha burned: 66 years (6.6%)
# Mean annual area burned - 10,370ha


untreated.ext.burned.cell.sums <- as.data.frame(colSums(untreated.ext.burned, na.rm = T))
table(untreated.ext.burned.cell.sums)
min(untreated.ext.burned.cell.sums)
max(untreated.ext.burned.cell.sums)*4 #most ha burned in a year
colMeans(untreated.ext.burned.cell.sums)*4 #mean ha burned/year
#rm(untreated.ext.burned)

untreated.ext.burned.cell.sums$scenario <- as.factor("Untreated_Extreme")
untreated.ext.burned.cell.sums$climate <- as.factor("Extreme")
untreated.ext.burned.cell.sums$harvest <- as.factor("Untreated")
untreated.ext.burned.cell.sums$year <- as.factor(1:100) #add year numbers for paired comparisons
colnames(untreated.ext.burned.cell.sums)[1]<- "Area Burned"


## Area Burned Metrics - UNTREATED EXTREME
# Minimum - 0 cells/ha burned: 79 years out of 1000
# Maximum - 876,828 ha: 1 year out of 1000
# Years with > 100,000 acres/40,000ha burned: 325 years (32.5%)
# Mean annual area burned - 77,819ha


BAU.hist.burned.cell.sums <- as.data.frame(colSums(BAU.hist.burned, na.rm = T))
table(BAU.hist.burned.cell.sums)
min(BAU.hist.burned.cell.sums)
max(BAU.hist.burned.cell.sums)*4 #most ha burned in a year
colMeans(BAU.hist.burned.cell.sums)*4 #mean ha burned/year
rm(BAU.hist.burned)

BAU.hist.burned.cell.sums$scenario <- as.factor("BAU_Historical")
BAU.hist.burned.cell.sums$climate <- as.factor("Historical")
BAU.hist.burned.cell.sums$harvest <- as.factor("BAU")
BAU.hist.burned.cell.sums$year <- as.factor(1:100) #add year numbers for paired comparisons
colnames(BAU.hist.burned.cell.sums)[1]<- "Area Burned"


## Area Burned Metrics - BAU HISTORICAL
# Minimum - 0 cells/ha burned: 139 years out of 1000
# Maximum - 354,000 ha: 1 year out of 1000
# Years with > 100,000 acres/40,000ha burned: 28 years (2.8%)
# Mean annual area burned - 5,978ha

Opt_BAU.hist.burned.cell.sums <- as.data.frame(colSums(Opt_BAU.hist.burned, na.rm = T))
table(Opt_BAU.hist.burned.cell.sums)
min(Opt_BAU.hist.burned.cell.sums)
max(Opt_BAU.hist.burned.cell.sums)*4 #most ha burned in a year
colMeans(Opt_BAU.hist.burned.cell.sums)*4 #mean ha burned/year
#rm(Opt_BAU.hist.burned)

Opt_BAU.hist.burned.cell.sums$scenario <- as.factor("Opt_BAU_Historical")
Opt_BAU.hist.burned.cell.sums$climate <- as.factor("Historical")
Opt_BAU.hist.burned.cell.sums$harvest <- as.factor("Opt_BAU")
Opt_BAU.hist.burned.cell.sums$year <- as.factor(1:100) #add year numbers for paired comparisons
colnames(Opt_BAU.hist.burned.cell.sums)[1]<- "Area Burned"


## Area Burned Metrics - Optimized BAU HISTORICAL
# Minimum - 0 cells/ha burned: 160 years out of 1000
# Maximum - 298,712 ha: 1 year out of 1000
# Years with > 100,000 acres/40,000ha burned: 32 years (3.2%)
# Mean annual area burned - 5,562ha


BAU.ext.burned.cell.sums <- as.data.frame(colSums(BAU.ext.burned, na.rm = T))
table(BAU.ext.burned.cell.sums)
min(BAU.ext.burned.cell.sums)
max(BAU.ext.burned.cell.sums)*4 #most ha burned in a year
colMeans(BAU.ext.burned.cell.sums)*4 #mean ha burned/year
#rm(BAU.ext.burned)

BAU.ext.burned.cell.sums$scenario <- as.factor("BAU_Extreme")
BAU.ext.burned.cell.sums$climate <- as.factor("Extreme")
BAU.ext.burned.cell.sums$harvest <- as.factor("BAU")
BAU.ext.burned.cell.sums$year <- as.factor(1:100) #add year numbers for paired comparisons
colnames(BAU.ext.burned.cell.sums)[1]<- "Area Burned"


## Area Burned Metrics - BAU EXTREME
# Minimum - 0 cells/ha burned: 117 years out of 1000
# Maximum - 884,500 ha: 1 year out of 1000
# Years with > 100,000 acres/40,000ha burned: 256 years (25.6%)
# Mean annual area burned - 57,029ha


Opt_BAU.ext.burned.cell.sums <- as.data.frame(colSums(Opt_BAU.ext.burned, na.rm = T))
table(Opt_BAU.ext.burned.cell.sums)
min(Opt_BAU.ext.burned.cell.sums)
max(Opt_BAU.ext.burned.cell.sums)*4 #most ha burned in a year
colMeans(Opt_BAU.ext.burned.cell.sums)*4 #mean ha burned/year
#rm(Opt_BAU.ext.burned)

Opt_BAU.ext.burned.cell.sums$scenario <- as.factor("Opt_BAU_Extreme")
Opt_BAU.ext.burned.cell.sums$climate <- as.factor("Extreme")
Opt_BAU.ext.burned.cell.sums$harvest <- as.factor("Opt_BAU")
Opt_BAU.ext.burned.cell.sums$year <- as.factor(1:100) #add year numbers for paired comparisons
colnames(Opt_BAU.ext.burned.cell.sums)[1]<- "Area Burned"


## Area Burned Metrics - OPTIMIZED BAU EXTREME
# Minimum - 0 cells/ha burned: 88 years out of 1000
# Maximum - 861,560 ha: 1 year out of 1000
# Years with > 100,000 acres/40,000ha burned: 251 years (25.1%)
# Mean annual area burned - 54,472ha


RxFire.hist.burned.cell.sums <- as.data.frame(colSums(RxFire.hist.burned, na.rm = T))
table(RxFire.hist.burned.cell.sums)
min(RxFire.hist.burned.cell.sums)
max(RxFire.hist.burned.cell.sums)*4 #most ha burned in a year
colMeans(RxFire.hist.burned.cell.sums)*4 #mean ha burned/year
#rm(RxFire.hist.burned)

RxFire.hist.burned.cell.sums$scenario <- as.factor("RxFire_Historical")
RxFire.hist.burned.cell.sums$climate <- as.factor("Historical")
RxFire.hist.burned.cell.sums$harvest <- as.factor("RxFire")
RxFire.hist.burned.cell.sums$year <- as.factor(1:100) #add year numbers for paired comparisons
colnames(RxFire.hist.burned.cell.sums)[1]<- "Area Burned"


## Area Burned Metrics - RxFire HISTORICAL
# Minimum - 0 cells/ha burned: 166 years out of 1000
# Maximum - 376,636 ha: 1 year out of 1000
# Years with > 100,000 acres/40,000ha burned: 46 years (4.6%)
# Mean annual area burned - 7,397ha


Opt_RxFire.hist.burned.cell.sums <- as.data.frame(colSums(Opt_RxFire.hist.burned, na.rm = T))
table(Opt_RxFire.hist.burned.cell.sums)
min(Opt_RxFire.hist.burned.cell.sums)
max(Opt_RxFire.hist.burned.cell.sums)*4 #most ha burned in a year
colMeans(Opt_RxFire.hist.burned.cell.sums)*4 #mean ha burned/year
rm(Opt_RxFire.hist.burned)

Opt_RxFire.hist.burned.cell.sums$scenario <- as.factor("Opt_RxFire_Historical")
Opt_RxFire.hist.burned.cell.sums$climate <- as.factor("Historical")
Opt_RxFire.hist.burned.cell.sums$harvest <- as.factor("Opt_RxFire")
Opt_RxFire.hist.burned.cell.sums$year <- as.factor(1:100) #add year numbers for paired comparisons
colnames(Opt_RxFire.hist.burned.cell.sums)[1]<- "Area Burned"


## Area Burned Metrics - Optimized RxFire HISTORICAL
# Minimum - 0 cells/ha burned: 152 years out of 1000
# Maximum - 414,224 ha: 1 year out of 1000
# Years with > 100,000 acres/40,000ha burned: 42 years (4.3%)
# Mean annual area burned - 7,201ha


RxFire.ext.burned.cell.sums <- as.data.frame(colSums(RxFire.ext.burned, na.rm = T))
table(RxFire.ext.burned.cell.sums)
min(RxFire.ext.burned.cell.sums)
max(RxFire.ext.burned.cell.sums)*4 #most ha burned in a year
colMeans(RxFire.ext.burned.cell.sums)*4 #mean ha burned/year
#rm(RxFire.ext.burned)

RxFire.ext.burned.cell.sums$scenario <- as.factor("RxFire_Extreme")
RxFire.ext.burned.cell.sums$climate <- as.factor("Extreme")
RxFire.ext.burned.cell.sums$harvest <- as.factor("RxFire")
RxFire.ext.burned.cell.sums$year <- as.factor(1:100) #add year numbers for paired comparisons
colnames(RxFire.ext.burned.cell.sums)[1]<- "Area Burned"


## Area Burned Metrics - RxFire EXTREME
# Minimum - 0 cells/ha burned: 132 years out of 1000
# Maximum - 886,200 ha: 1 year out of 1000
# Years with > 100,000 acres/40,000ha burned: 247 years (24.7%)
# Mean annual area burned - 52,213ha


Opt_RxFire.ext.burned.cell.sums <- as.data.frame(colSums(Opt_RxFire.ext.burned, na.rm = T))
table(Opt_RxFire.ext.burned.cell.sums)
min(Opt_RxFire.ext.burned.cell.sums)
max(Opt_RxFire.ext.burned.cell.sums)*4 #most ha burned in a year
colMeans(Opt_RxFire.ext.burned.cell.sums)*4 #mean ha burned/year
#rm(Opt_RxFire.ext.burned)

Opt_RxFire.ext.burned.cell.sums$scenario <- as.factor("Opt_RxFire_Extreme")
Opt_RxFire.ext.burned.cell.sums$climate <- as.factor("Extreme")
Opt_RxFire.ext.burned.cell.sums$harvest <- as.factor("Opt_RxFire")
Opt_RxFire.ext.burned.cell.sums$year <- as.factor(1:100) #add year numbers for paired comparisons
colnames(Opt_RxFire.ext.burned.cell.sums)[1]<- "Area Burned"

## Area Burned Metrics - OPTIMIZED RxFIRE EXTREME
# Minimum - 0 cells/ha burned: 113 years out of 1000
# Maximum - 916,580 ha: 1 year out of 1000
# Years with > 100,000 acres/40,000ha burned: 233 years (23.3%)
# Mean annual area burned - 52,748.24ha


RxFire3x.hist.burned.cell.sums <- as.data.frame(colSums(RxFire3x.hist.burned, na.rm = T))
table(RxFire3x.hist.burned.cell.sums)
min(RxFire3x.hist.burned.cell.sums)
max(RxFire3x.hist.burned.cell.sums)*4 #most ha burned in a year
colMeans(RxFire3x.hist.burned.cell.sums)*4 #mean ha burned/year
#rm(RxFire3x.hist.burned)

RxFire3x.hist.burned.cell.sums$scenario <- as.factor("RxFire3x_Historical")
RxFire3x.hist.burned.cell.sums$climate <- as.factor("Historical")
RxFire3x.hist.burned.cell.sums$harvest <- as.factor("RxFire3x")
RxFire3x.hist.burned.cell.sums$year <- as.factor(1:100) #add year numbers for paired comparisons
colnames(RxFire3x.hist.burned.cell.sums)[1]<- "Area Burned"


## Area Burned Metrics - RxFire3x HISTORICAL
# Minimum - 0 cells/ha burned: 208 years out of 1000
# Maximum - 245,160 ha: 1 year out of 1000
# Years with > 100,000 acres/40,000ha burned: 35 years (3.5%)
# Mean annual area burned - 5,506ha


Opt_RxFire3x.hist.burned.cell.sums <- as.data.frame(colSums(Opt_RxFire3x.hist.burned, na.rm = T))
table(Opt_RxFire3x.hist.burned.cell.sums)
min(Opt_RxFire3x.hist.burned.cell.sums)
max(Opt_RxFire3x.hist.burned.cell.sums)*4 #most ha burned in a year
colMeans(Opt_RxFire3x.hist.burned.cell.sums)*4 #mean ha burned/year
#rm(Opt_RxFire3x.hist.burned)

Opt_RxFire3x.hist.burned.cell.sums$scenario <- as.factor("Opt_RxFire3x_Historical")
Opt_RxFire3x.hist.burned.cell.sums$climate <- as.factor("Historical")
Opt_RxFire3x.hist.burned.cell.sums$harvest <- as.factor("Opt_RxFire3x")
Opt_RxFire3x.hist.burned.cell.sums$year <- as.factor(1:100) #add year numbers for paired comparisons
colnames(Opt_RxFire3x.hist.burned.cell.sums)[1]<- "Area Burned"


## Area Burned Metrics - Optimized RxFire3x HISTORICAL
# Minimum - 0 cells/ha burned: 179 years out of 1000
# Maximum - 221,184 ha: 1 year out of 1000
# Years with > 100,000 acres/40,000ha burned: 37 years (3.7%)
# Mean annual area burned - 5,972ha


RxFire3x.ext.burned.cell.sums <- as.data.frame(colSums(RxFire3x.ext.burned, na.rm = T))
table(RxFire6x.ext.burned.cell.sums[1])
min(RxFire3x.ext.burned.cell.sums)
max(RxFire3x.ext.burned.cell.sums)*4 #most ha burned in a year
colMeans(RxFire3x.ext.burned.cell.sums)*4 #mean ha burned/year
#rm(RxFire3x.ext.burned)

RxFire3x.ext.burned.cell.sums$scenario <- as.factor("RxFire3x_Extreme")
RxFire3x.ext.burned.cell.sums$climate <- as.factor("Extreme")
RxFire3x.ext.burned.cell.sums$harvest <- as.factor("RxFire3x")
RxFire3x.ext.burned.cell.sums$year <- as.factor(1:100) #add year numbers for paired comparisons
colnames(RxFire3x.ext.burned.cell.sums)[1]<- "Area Burned"


## Area Burned Metrics - RxFire3x EXTREME
# Minimum - 0 cells/ha burned: 172 years out of 1000
# Maximum - 834,272 ha: 1 year out of 1000
# Years with > 100,000 acres/40,000ha burned: 224 years (22.4%)
# Mean annual area burned - 49,373ha


Opt_RxFire3x.ext.burned.cell.sums <- as.data.frame(colSums(Opt_RxFire3x.ext.burned, na.rm = T))
table(Opt_RxFire3x.ext.burned.cell.sums)
min(Opt_RxFire3x.ext.burned.cell.sums)
max(Opt_RxFire3x.ext.burned.cell.sums)*4 #most ha burned in a year
colMeans(Opt_RxFire3x.ext.burned.cell.sums)*4 #mean ha burned/year
#rm(Opt_RxFire3x.ext.burned)

Opt_RxFire3x.ext.burned.cell.sums$scenario <- as.factor("Opt_RxFire3x_Extreme")
Opt_RxFire3x.ext.burned.cell.sums$climate <- as.factor("Extreme")
Opt_RxFire3x.ext.burned.cell.sums$harvest <- as.factor("Opt_RxFire3x")
Opt_RxFire3x.ext.burned.cell.sums$year <- as.factor(1:100) #add year numbers for paired comparisons
colnames(Opt_RxFire3x.ext.burned.cell.sums)[1]<- "Area Burned"

## Area Burned Metrics - OPTIMIZED RxFire3x EXTREME
# Minimum - 0 cells/ha burned: 115 years out of 1000
# Maximum - 845,416 ha: 1 year out of 1000
# Years with > 100,000 acres/40,000ha burned: 252 years (25.2%)
# Mean annual area burned - 51,354ha


##Combine into one dataframe

Area_Burned_Historical <- rbind(untreated.burned.cell.sums, BAU.hist.burned.cell.sums, Opt_BAU.hist.burned.cell.sums,
                                RxFire.hist.burned.cell.sums, Opt_RxFire.hist.burned.cell.sums, 
                                RxFire3x.hist.burned.cell.sums, Opt_RxFire3x.hist.burned.cell.sums)

colnames(Area_Burned_Historical)[1] <- "Area_Burned"
Area_Burned_Historical$Area_Burned <- Area_Burned_Historical$Area_Burned*4
Area_Burned_Historical$Log_Area <- log(Area_Burned_Historical$Area_Burned+1)
write.csv(Area_Burned_Historical, "W:/Treatments_Results_Oct9_2017/Area_Burned_Data/Area_Burned_Historical.csv")


dev.off()
par(mfrow=c(1,1), xpd = NA, cex.axis = 0.8)
boxplot(Area_Burned_Historical$Log_Area ~ Area_Burned_Historical$harvest, ylab = "log area burned (ha)", las = 2)        
text(x = 1, y = , 14.5, paste(round(mean(log((untreated.burned.cell.sums[,1]*4)+1)),2)))
text(x = 2, y = , 14.5, paste(round(mean(log((BAU.hist.burned.cell.sums[,1]*4)+1)),2)))
text(x = 3, y = , 14.5, paste(round(mean(log((Opt_BAU.hist.burned.cell.sums[,1]*4)+1)),2)))
text(x = 4, y = , 14.5, paste(round(mean(log((RxFire.hist.burned.cell.sums[,1]*4)+1)),2)))
text(x = 5, y = , 14.5, paste(round(mean(log((Opt_RxFire.hist.burned.cell.sums[,1]*4)+1)),2)))
text(x = 6, y = , 14.5, paste(round(mean(log((RxFire3x.hist.burned.cell.sums[,1]*4)+1)),2)))
text(x = 7, y = , 14.5, paste(round(mean(log((Opt_RxFire3x.hist.burned.cell.sums[,1]*4)+1)),2)))
title(main = "Annual Area Burned - Historical Weather", line = 2)



Area_Burned_Extreme <- rbind(untreated.ext.burned.cell.sums, BAU.ext.burned.cell.sums, Opt_BAU.ext.burned.cell.sums,
                             RxFire.ext.burned.cell.sums, Opt_RxFire.ext.burned.cell.sums, 
                             RxFire3x.ext.burned.cell.sums, Opt_RxFire3x.ext.burned.cell.sums)

colnames(Area_Burned_Extreme)[1] <- "Area_Burned"
Area_Burned_Extreme$Area_Burned <- Area_Burned_Extreme$Area_Burned*4
Area_Burned_Extreme$Log_Area <- log(Area_Burned_Extreme$Area_Burned+1)
write.csv(Area_Burned_Extreme, "W:/Treatments_Results_Oct9_2017/Area_Burned_Data/Area_Burned_Extreme.csv")


par(mfrow=c(1,1), xpd = NA, cex.axis = 0.8)
boxplot(Area_Burned_Extreme$Log_Area ~ Area_Burned_Extreme$harvest, ylab = "log area burned (ha)", las = 2)        
text(x = 1, y = , 15.5, paste(round(mean(log((untreated.ext.burned.cell.sums[,1]*4)+1)),2)))
text(x = 2, y = , 15.5, paste(round(mean(log((BAU.ext.burned.cell.sums[,1]*4)+1)),2)))
text(x = 3, y = , 15.5, paste(round(mean(log((Opt_BAU.ext.burned.cell.sums[,1]*4)+1)),2)))
text(x = 4, y = , 15.5, paste(round(mean(log((RxFire.ext.burned.cell.sums[,1]*4)+1)),2)))
text(x = 5, y = , 15.5, paste(round(mean(log((Opt_RxFire.ext.burned.cell.sums[,1]*4)+1)),2)))
text(x = 6, y = , 15.5, paste(round(mean(log((RxFire3x.ext.burned.cell.sums[,1]*4)+1)),2)))
text(x = 7, y = , 15.5, paste(round(mean(log((Opt_RxFire3x.ext.burned.cell.sums[,1]*4)+1)),2)))
title(main = "Annual Area Burned - Extreme Weather", line = 2)

###Statistical Tests###

#Test for equal variances among groups

library(car)
leveneTest(Area_Burned_Historical$Log_Area ~ Area_Burned_Historical$harvest) #p=0.09, equal variances
leveneTest(Area_Burned_Extreme$Log_Area ~ Area_Burned_Extreme$harvest) #p = 0.000001, so unequal variances

#Data violate the assumption of independence, I'll use the Kruskal Wallace 

#Nonparametric test just to be sure & multiple comparisons with conservative correction:
kruskal.test(Area_Burned_Historical$Log_Area ~ Area_Burned_Historical$harvest) #still different (p < 2.2e-16)
dunn.test(x = Area_Burned_Historical$Log_Area, g = Area_Burned_Historical$harvest, method="bonferroni") #Multiple comparisons using Bonferroni's adjustment because samples are NOT independent.


#Nonparametric test just to be sure & multiple comparisons with conservative correction:
kruskal.test(Area_Burned_Extreme$Log_Area ~ Area_Burned_Extreme$harvest) #still different (p < 2.2e-16)
dunn.test(Area_Burned_Extreme$Log_Area, Area_Burned_Extreme$harvest, method="bonferroni") #Multiple comparisons using Bonferroni's adjustment because samples are NOT independent.

