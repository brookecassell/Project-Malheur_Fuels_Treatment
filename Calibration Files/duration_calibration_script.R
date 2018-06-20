#### Project: Optimizing placement of fuel treatments and accelerating prescribed fire 
#### ameliorates extreme weather-driven wildfires in western mixed-conifer forests by 
#### Brooke A. Cassell, Portland State University, Dissertation defended February 2018

### This script can be used to quickly compare the log of annual area burned from calibration runs in LANDIS-II with
### observed data.

##### Calibrating fire - annual area burned - with a loop to read in multiple fire summary logs ####

library(dplyr)

wd <- "R:/Cassell/Malheur Project Files/DynamicFire/DFFSCalibration/Fire_Calibration_092017" #designate working directory location
scen_list <- list.files(wd) #read the list of files within that folder - each file includes reps of calibration runs
scen_list #double check

#scen_list <- "7_1.4__0.4" #for testing only

reps <- 4 #number of reps in each calibration scenario

all_area_burned <- NULL #create an empty object

for (Scenario in scen_list){   #loop in all of the fire summary logs and combine them into one dataframe
  print(paste('scenario=', Scenario, sep = ""))
  for (Rep in 1:reps){
    print(paste('rep=', Rep, sep = ""))
    fire_log_file <- paste(wd, Scenario, "/", "dynamic-fire-summary-log", Rep, ".csv", sep = "")
    fire_log <- read.csv(fire_log_file, strip.white = T, header = T)
    p <- ncol(fire_log)
    fire_log_trunc <- fire_log[,1:(p-1)] #remove mysteriously added column of NAs
    fire_annual_sums <- with(fire_log_trunc, aggregate(fire_log_trunc[,3:4], data.frame(Time), sum))
    fire_annual_sums$HaBurned <- round(fire_annual_sums$TotalBurnedSites*4,2)  #Calculate the ha burned (4ha per cell)
    fire_annual_sums$LnSize <- round(log(fire_annual_sums$HaBurned),2) #calculate the natural log of annual area burned
    fire_annual_sums$LnSize[fire_annual_sums$LnSize == "-Inf"] <- 0 #change logs of 0 to 0 for the purpose the calculations
    replicate <- rep(Rep, nrow(fire_annual_sums))   # create a vector of the length of your input file to record the replicate using the rep (replicate) function
    reps_data <- cbind(fire_annual_sums, replicate)     # bind the replicate number to the data
    scen_data <- cbind(reps_data, Scenario)     # bind the scenario name to the data
    scen_rep <- paste(Scenario,"-replicate", replicate, sep="") #create a vector with scenario name and replicate in one
    scen_data_rep <- cbind(scen_data, scen_rep) # bind scenario name + rep to the data
    all_area_burned <- rbind(all_area_burned, scen_data_rep)         # append the replicates together
  }
}

rm(Scenario)
rm(Rep)


hist_data <- c("Historical", 13201.38, 614.93, 24480.90, 81010.84)  #these are the historical mean, median, sd and max
hist_ln_data <- c("Historical", 5.73, 6.39, 4.18, 11.30)  #these are the historical natural log mean, median, sd and max

by_scenario <- group_by(all_area_burned, Scenario)  #use dplyr to group the data by scenario

size_dist <- by_scenario %>% summarise(MeanHa = mean(HaBurned), MedianHa = median(HaBurned), StDevHa = sd(HaBurned), MaxHa = max(HaBurned))
levels(size_dist$Scenario) <- c(levels(size_dist$Scenario),"Historical") #add the factor "Hist" so that the historical summary stats can be added to the dataframe
size_dist <- rbind(size_dist, hist_data) #bind historical data to calibration scenarios
size_dist <- transform(size_dist, MeanHa = as.numeric(MeanHa), MedianHa = as.numeric(MedianHa), StDevHa = as.numeric(StDevHa), MaxHa = as.numeric(MaxHa))



log_dist <- by_scenario %>% summarise(MeanLn = mean(LnSize), MedianLn = median(LnSize), StDevLn = sd(LnSize), MaxLn = max(LnSize))
levels(log_dist$Scenario) <- c(levels(log_dist$Scenario),"Historical") #add the factor "Hist" so that the historical summary stats can be added to the dataframe
log_dist <- rbind(log_dist, hist_ln_data)
log_dist <- transform(log_dist, MeanLn = as.numeric(MeanLn), MedianLn = as.numeric(MedianLn), StDevLn = as.numeric(StDevLn),  MaxLn = as.numeric( MaxLn))

group <- log_dist$Scenario  #specify groups for plotting

#Boxplot
lnboxplot <- ggplot(log_dist, aes(x = as.factor(group))) +
  geom_boxplot(aes(
    lower = MeanLn - StDevLn,
    upper = MeanLn + StDevLn,
    middle = MedianLn,
    ymin = 0, 
    ymax = MaxLn),
    stat = "identity")

lnboxplot + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(aes(yintercept=12.2), colour = "blue", linetype="dashed") + 
  geom_hline(aes(yintercept=6.39), colour="#990000", linetype="dashed") +
  labs(x = "Scenario", y = "Ln of Area (ha)") + ggtitle("Fire Duration-Size Calibration")
 

####  The best fire input scenario is 7_1.4_18000_0.4. Plot this one against Historical ##

log_dist_select <- log_dist[ which(log_dist$Scenario=='Historical'
                                            | log_dist$Scenario =='7_1.4_18000_0.4'), ]
group <- log_dist_select$Scenario
lnboxplot <- ggplot(log_dist_select, aes(x = as.factor(group))) +
  geom_boxplot(aes(
    lower = MeanLn - StDevLn,
    upper = MeanLn + StDevLn,
    middle = MedianLn,
    ymin = 0, 
    ymax = MaxLn),
    stat = "identity")

lnboxplot + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(aes(yintercept=12.2), colour = "blue", linetype="dashed") + 
  labs(x = "Scenario", y = "Ln of Area (ha)") + ggtitle(label = "Fire Duration-Size Calibration", 
                                                        subtitle = "Dynamic Fire Inputs: mu = 7, sigma = 1.4, max = 18000 min, ignitions = 0.4") +
  scale_x_discrete(labels=c("7_1.4_18000_0.4" = "Modeled", "Historical" = "Observed"))


