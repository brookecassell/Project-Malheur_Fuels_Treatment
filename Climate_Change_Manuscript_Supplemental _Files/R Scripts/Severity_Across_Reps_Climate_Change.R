#### Project: More widespread and severe wildfires under climate change lead to dramatic declines in 
#### high-elevation species in the dry mixed conifer forests of the inland western U.S. by 
#### Brooke A. Cassell, Portland State University, Dissertation defended February 2018


#### This script is for calculating fire severity across climate scenarios
#Script written by Brooke A. Cassell, December 2017

w.dir <- "Z:/ClimateChange_Results_Oct6_2017/"

library(raster)
library(rgdal)
library(viridis)
library(readr)
library(dplry)


years <- 90
timesteps <- 1:90 # input number of timesteps (years in which fire maps are created) here (if comparing just the last 30 years, use 31:90)
no_reps <- 10  # input nummber of replicates here

## scenario list w/out Riparian scenarios
scenario <- c("Historical", "RCP_4.5", "RCP_8.5")


seq.cols <- cbind(1:90, 91:180, 181:270, 271:360, 361:450, 451:540, 541:630, 631:720, 721:810, 811:900) #create sequence to fill columns for each replicate. Each set should be equal to the number of years in your simulation. 


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
      map.df[map.df == -2]<- NA #classify inactive cells as NA
      map.df[map.df == -1] <- NA #classify unburned cells as NA
      map.df <- as.data.frame(map.df)
      burn.years[,k] <- map.df[,1] ## binding all years
    }
    rep.mean <- matrix(rowMeans(burn.years, na.rm=T)) #average across all years for the rep
    burn.means <- cbind(burn.means, rep.mean) #bind reps
    burn.years <- NULL #empty the matrix for the next rep
  }
  burn.maps[[Scenario]] <- burn.means
  burn.means <- NULL #empty the matrix for the next scenario
  
}
rm(Scenario)
rm(i)
rm(k)

Historical.severity <- burn.maps$Historical
RCP_4.5.severity <- burn.maps$RCP_4.5
RCP_8.5.severity <- burn.maps$RCP_8.5


rm(burn.means) #remove the loop files to free up memory
rm(burn.years)
rm(burn.maps)


#create dataframes with landscape means for each rep and add columns for scenario and rep
Historical.by_rep <- data.frame(colMeans(Historical.severity, na.rm = T))
RCP_4.5.by_rep <- data.frame(colMeans(RCP_4.5.severity, na.rm = T))
RCP_8.5.by_rep <- data.frame(colMeans(RCP_8.5.severity, na.rm = T))

colnames(Historical.by_rep)[1]<- "mean_severity"
Historical.by_rep$scenario <- "Historical"
Historical.by_rep$rep <- as.factor(c(1:10))

colnames(RCP_4.5.by_rep)[1]<- "mean_severity"
RCP_4.5.by_rep$scenario <- "RCP_4.5"
RCP_4.5.by_rep$rep <- as.factor(c(1:10))

colnames(RCP_8.5.by_rep)[1]<- "mean_severity"
RCP_8.5.by_rep$scenario <- "RCP_8.5"
RCP_8.5.by_rep$rep <- as.factor(c(1:10))


#combine into one dataframe
Rep_Severity<- rbind(Historical.by_rep,RCP_4.5.by_rep, RCP_8.5.by_rep)
write.csv(Rep_Severity, "Z:/ClimateChange_Results_Oct6_2017/Fire_Severity_Data/Severity_by_Rep.csv", row.names = F)



############ Statistical tests

#boxplots to visualize
hist_by_scenario <- group_by(Rep_Severity, scenario)
hist_area_by_scenario <- summarise(hist_by_scenario, Severity = mean(mean_severity, na.rm = T), sd = sd(mean_severity, na.rm = T))

par(mfrow=c(1,1), xpd = NA, cex.axis = 0.8)
boxplot(hist_by_scenario$mean_severity ~ hist_by_scenario$scenario, las=2,
        ylab = "Severity Index")
#text(x = 1, y = , 3.7, paste0(3.22))
#text(x = 2, y = , 3.7, paste0(3.40))
#text(x = 3, y = , 3.7, paste0(3.39))

title(main = "Fire Severity", line = 2)



#Test for normality 
shapiro.test(hist_by_scenario$mean_severity) #W = 0.95, p = 0.18, data are normally distributed

#Test for homoscedasticity of the log-transformed data
library(car)
leveneTest(hist_by_scenario$mean_severity ~ hist_by_scenario$scenario) #F = 0.44 p = 0.65, equal variances

#Since data are normally distributed with equal variance, use ANOVA
a <- aov(hist_by_scenario$mean_severity ~ hist_by_scenario$scenario)
summary(a) #F = 5.32, p = 0.0113 - YES significant difference

TukeyHSD(a) #No difference between 4.5 & 8.5 (diff = -0.118, p = 0.98)
#Difference between Historical & 4.5: diff = 0.18, p = 0.019; Historical & 8.5: diff = 0.17, p = 0.029)


