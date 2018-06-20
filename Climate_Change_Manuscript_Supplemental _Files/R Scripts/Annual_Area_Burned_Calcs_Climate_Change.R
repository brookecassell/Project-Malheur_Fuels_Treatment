#### Project: More widespread and severe wildfires under climate change lead to dramatic declines in 
#### high-elevation species in the dry mixed conifer forests of the inland western U.S. by 
#### Brooke A. Cassell, Portland State University, Dissertation defended February 2018

## This script is for calculating annual area burned across climate scenarios and replicates. Includes plots.
# Written by Brooke A. Cassell in December 2017


w.dir <- "Z:/ClimateChange_Results_Oct6_2017/"

library(raster)
library(rgdal)
library(viridis)
library(readr)


years <- 90
timesteps <- 1:90 # input start and end timesteps (years in which fire maps are created) here
no_reps <- 10  # input nummber of replicates here

## scenario list w/out Riparian scenarios
scenario <- c("Historical", "RCP_4.5", "RCP_8.5")


seq.cols <- cbind(1:90, 91:180, 181:270, 271:360, 361:450, 451:540, 541:630, 631:720, 721:810, 811:900) #create sequence to fill columns for each replicate. Each set should be equal to the number of years in your simulation. 

ecomap <- raster("Z:/ClimateChange_Results_Oct6_2017/Historical/replicate1/ecoregions4ha1.img") #read in the map of ecoregions to use as a mask
plot(ecomap) #plot to make sure it looks right
ecomap[(ecomap ==0)] <- NA #change cells with value = 0 to NA. Since all active cells have an ecoregion value, this will give a mask for inactive cells to fire maps


##setting up loop objects
burn.years <- matrix(ncol=90, nrow=435000)
burn.reps <- matrix(ncol=900, nrow=435000)
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
      map.df[map.df == 1] <- 0 #unburned cells
      map.df[map.df > 1] <- 1 #burned cells
      burn.years[,k] <- map.df[,1] ## binding all years
    }
    burn.reps[,seq.cols[,i]] <- burn.years #binding years across reps
  }
  burn.maps[[Scenario]] <- burn.reps
  
}
rm(Scenario)
rm(i)
rm(k)

#Index out each scenario
hist.burned <- burn.maps$Historical
RCP_4.5.burned <- burn.maps$RCP_4.5
RCP_8.5.burned <- burn.maps$RCP_8.5 

#Sum across columns and convert to hectares (4 hectares per cell)
hist.burned.cell.sums <- as.data.frame(colSums(hist.burned, na.rm = T)*4) #add down columns for annual area burned & multiply by 4 for ha
RCP_4.5.burned.cell.sums <- as.data.frame(colSums(RCP_4.5.burned, na.rm = T)*4)
RCP_8.5.burned.cell.sums <- as.data.frame(colSums(RCP_8.5.burned, na.rm = T)*4)

#Add columns with log area, scenario, year, and rep
hist.burned.cell.sums$log_area <-log1p(hist.burned.cell.sums[,1])
hist.burned.cell.sums$scenario <- as.factor("Historical")
hist.burned.cell.sums$year <- as.factor(1:90) #add year numbers 
hist.burned.cell.sums$rep <- as.factor(rep(1:10, each=90))
colnames(hist.burned.cell.sums)[1] <- "area(ha)"


RCP_4.5.burned.cell.sums$log_area <-log1p(RCP_4.5.burned.cell.sums[,1])
RCP_4.5.burned.cell.sums$scenario <- as.factor("RCP_4.5")
RCP_4.5.burned.cell.sums$year <- as.factor(1:90) #add year numbers
RCP_4.5.burned.cell.sums$rep <- as.factor(rep(1:10, each=90))
colnames(RCP_4.5.burned.cell.sums)[1]<- "area(ha)"


RCP_8.5.burned.cell.sums$log_area <-log1p(RCP_8.5.burned.cell.sums[,1])
RCP_8.5.burned.cell.sums$scenario <- as.factor("RCP_8.5")
RCP_8.5.burned.cell.sums$year <- as.factor(1:90) #add year numbers for paired comparisons
RCP_8.5.burned.cell.sums$rep <- as.factor(rep(1:10, each=90))
colnames(RCP_8.5.burned.cell.sums)[1]<- "area(ha)"


#Combine into one dataframe
Log_Area_Burned <- rbind(hist.burned.cell.sums,RCP_4.5.burned.cell.sums, RCP_8.5.burned.cell.sums)
colnames(Log_Area_Burned)[1] <- "area_ha"
write.csv(Log_Area_Burned, "Z:/ClimateChange_Results_Oct6_2017/Area_Burned_Data/Area_Burned_by_Rep.csv", row.names = F)

###
library(dplyr)

by_scenario <- group_by(Log_Area_Burned, scenario, rep, add = TRUE)
Mean_across_reps <- group_by(Log_Area_Burned, scenario)
summarise_mean_across_reps <- summarise(Mean_across_reps, log_area_burned = mean(area_ha), sd = sd(area_ha))
area_by_scenario <- summarise(by_scenario, log_area_burned = mean(log_area), sd = sd(log_area))
area_by_scenario <- as.data.frame(area_by_scenario)

par(mfrow=c(1,1), xpd = NA, cex.axis = 0.8)
boxplot(area_by_scenario$log_area_burned ~ area_by_scenario$scenario, las=2,
        ylab = "Log Area Burned (ha)", ylim = c(5,7))
#text(x = 1, y = , 7.2, paste0(5.56))
#text(x = 2, y = , 7.2, paste0(5.87))
#text(x = 3, y = , 7.2, paste0(6.03))
#text(x = 1, y = 6.8, paste0("a"))
#text(x = 2, y = 6.8, paste0("a,b"))
#text(x = 3, y = 6.8, paste0("b"))
title(main = "Annual Area Burned", line = 2)



###### Statistical Tests #######

#Test for normality of the log-transformed data
shapiro.test(area_by_scenario$log_area_burned) #W = 0.98, p = 0.89, data are normally distributed

#Test for homoscedasticity of the log-transformed data
bartlett.test(area_by_scenario$log_area_burned ~ area_by_scenario$scenario) K^2 = 0.312 #p = 0.86, equal variances

#Run an ANOVA
a <- aov(area_by_scenario$log_area_burned ~ area_by_scenario$scenario) #df = 2, F = 4.98, p = 0.0145 - reject H0
summary(a)



### Repeat for only the last 30 years of the simulation - use a vector of the last 30 obs for each rep and subset

last30 <- c(61:90, 151:180, 241:270, 331:360, 421:450, 511:540, 601:630, 691:720, 781:810, 871:900)
hist.burned.cell.sums30 <- as.data.frame(hist.burned.cell.sums[last30,])
RCP_4.5.burned.cell.sums30 <- as.data.frame(RCP_4.5.burned.cell.sums[last30,])
RCP_8.5.burned.cell.sums30 <- as.data.frame(RCP_8.5.burned.cell.sums[last30,])

Log_Area_Burned30 <- rbind(hist.burned.cell.sums30, RCP_4.5.burned.cell.sums30, RCP_8.5.burned.cell.sums30 )
colnames(Log_Area_Burned30)[1] <- "area_ha"
write.csv(Log_Area_Burned30, "Z:/ClimateChange_Results_Oct6_2017/Area_Burned_Data/Area_Burned_by_Rep_Last_30_Years.csv", row.names = F)


by_scenario30 <- group_by(Log_Area_Burned30, scenario, rep, add = TRUE)
Mean_across_reps30 <- group_by(Log_Area_Burned30, scenario)
summarise_mean_across_reps30 <- summarise(Mean_across_reps30, log_area_burned = mean(log_area), sd = sd(log_area))
area_by_scenario30 <- summarise(by_scenario30, log_area_burned = mean(log_area), sd = sd(log_area))
area_by_scenario30 <- as.data.frame(area_by_scenario30)

par(mfrow=c(1,1), xpd = NA, cex.axis = 0.8)
boxplot(area_by_scenario30$log_area_burned ~ area_by_scenario30$scenario, las=2,
        ylab = "Log Area Burned (ha)")
#text(x = 1, y = , 7, paste0(5.24))
#text(x = 2, y = , 7, paste0(5.75))
#text(x = 3, y = , 7, paste0(5.79))
#text(x = 1, y = 6.6, paste0("a"))
#text(x = 2, y = 6.6, paste0("a,b"))
#text(x = 3, y = 6.6, paste0("b"))

title(main = "Annual Area Burned", line = 2)


#Test for normality of the log-transformed data
shapiro.test(area_by_scenario30$log_area_burned) #W = 0.0.96, p = 0.24, data are normally distributed

#Test for homoscedasticity of the log-transformed data
bartlett.test(area_by_scenario30$log_area_burned ~ area_by_scenario30$scenario) #K^0.79 p= 0.68 #p = 0.86, equal variances

#Run an ANOVA
b <- aov(area_by_scenario30$log_area_burned ~ area_by_scenario30$scenario) #df = 2, F = 3.82, p = 0.035 - reject H0
summary(b)


#Another plotting option - density plot
##For last 30 years only

library(sm)
Log_Area_Burned_nona30 <- na.omit(Log_Area_Burned30) #delete rows with NA, since sm.density.compare can't have them
#dev.off() #if needed to clear previous plot settings
sm.density.compare(Log_Area_Burned_nona30$log_area, Log_Area_Burned_nona30$scenario, xlab="Log of Annual Area Burned (ha)", ylab = "Proportion",
                   col = c("dodgerblue2", "darkorange2", "firebrick3"), lwd = c(2,2,2))
title(main="Mean Annual Area Burned")
legend("topleft", levels(Log_Area_Burned_nona30$scenario), col = c("dodgerblue2","darkorange2", "firebrick3"), lwd = c(2,2,2), lty = c(1,2,3), bty = "n")
#abline(v = 5.24, col = "dodgerblue2", lty = 1)
#abline(v = 5.75, col = "darkorange2", lty = 2)
#abline(v = 5.47, col = "firebrick3", lty = 3)


### All years for comparison

Log_Area_Burned_nona <- na.omit(Log_Area_Burned) #delete rows with NA, since sm.density.compare can't have them
#dev.off() #if needed to clear previous plot settings
sm.density.compare(Log_Area_Burned_nona$log_area, Log_Area_Burned_nona$scenario, xlab="Log of Annual Area Burned (ha)", ylab = "Proportion",
                   col = c("dodgerblue2", "darkorange2", "firebrick3"), lwd = c(2,2,2))
title(main="Mean Annual Area Burned")
legend("topleft", levels(Log_Area_Burned_nona$scenario), col = c("dodgerblue2","darkorange2", "firebrick3"), lwd = c(2,2,2), lty = c(1,2,3), bty = "n")
#abline(v = 5.56, col = "dodgerblue2", lty = 1)
#abline(v = 5.87, col = "darkorange2", lty = 2)
#abline(v = 6.03, col = "firebrick3", lty = 3)



######## Annual Area Burned through time

Log_Area_Burned$year <- as.factor(rep(2011:2100, times = 30)) #replace year # with simulation years


library(dplyr)
library(ggplot2)

#Use group_by and summarise to calculate mean and 5th/95th quantiles of area burned for each year across reps

Area_Grouped <- group_by(Log_Area_Burned, scenario, year, add = TRUE)
Area_Summary <- as.data.frame(summarise(Area_Grouped, mean_area = mean(area_ha), ninetyfifth = quantile(area_ha, probs=0.95), fifth = quantile(area_ha, probs=0.05)))



par(mfrow = c(1,1))

a <- ggplot(Area_Summary, aes(x = year, y = mean_area, colour = scenario, group = scenario)) + 
  geom_line(size = 2) +
  geom_ribbon(aes(ymin = fifth, ymax = ninetyfifth, fill = scenario), alpha = 0.3) +
  theme_classic() +
  scale_color_manual(values = c("blue", "orange", "red")) +
  scale_fill_manual(values = c("blue", "orange", "red")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(breaks=seq(2010, 2100, 10)) + ylab("Mean Area Burned (ha)")

a


##Plot the log-transformed data

Area_Summary_Log <- as.data.frame(summarise(Area_Grouped, mean_area = mean(log_area), ninetyfifth = quantile(log_area, probs=0.95), fifth = quantile(log_area, probs=0.05)))


b <- ggplot(Area_Summary_Log, aes(x = year, y = mean_area, colour = scenario, group = scenario)) + 
  geom_line(size = 2) +
  geom_ribbon(aes(ymin = fifth, ymax = ninetyfifth, fill = scenario), alpha = 0.3) +
  theme_classic() +
  scale_color_manual(values = c("blue", "orange", "red")) +
  scale_fill_manual(values = c("blue", "orange", "red")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(breaks=seq(2010, 2100, 10)) + ylab("Mean Area Burned (ha)")

b

#Plot z-scores
Area_Summary_Log$scaled_mean <- scale(Area_Summary_Log$mean_area)
Area_Summary_Log$scaled_lwr <- scale(Area_Summary_Log$fifth)
Area_Summary_Log$scaled_upr <- scale(Area_Summary_Log$ninetyfifth)

c <- ggplot(Area_Summary_Log, aes(x = year, y = scaled_mean, colour = scenario, group = scenario)) + 
  geom_line(size = 2) +
  geom_ribbon(aes(ymin = scaled_lwr, ymax = scaled_upr, fill = scenario), alpha = 0.3) +
  theme_classic() +
  scale_color_manual(values = c("blue", "orange", "red")) +
  scale_fill_manual(values = c("blue", "orange", "red")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(breaks=seq(2010, 2100, 10)) + ylab("Mean Area Burned (ha)")

c

#Plot with loess smoothing line (log-transformed)
d <- ggplot(Area_Summary_Log, aes(x = year, y = mean_area, colour = scenario, group = scenario)) + 
  #geom_line(size = .5, lty=1, alpha = .5) +
  #geom_ribbon(aes(ymin = fifth, ymax = ninetyfifth, fill = scenario), alpha = 0.3) +
  stat_smooth(aes(fill=scenario), method = "loess", alpha = 0.2)+
  theme_classic() +
  scale_color_manual(values = c("blue", "orange", "red")) +
  scale_fill_manual(values = c("blue", "orange", "red")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(breaks=seq(2010, 2100, 10)) + ylab("Mean Area Burned (ha)")+ xlab("Year")

d

#Plot with loess smoothing line (raw data)
e <- ggplot(Area_Summary, aes(x = year, y = mean_area, colour = scenario, group = scenario)) + 
  #geom_line(size = 2) +
  #geom_ribbon(aes(ymin = fifth, ymax = ninetyfifth, fill = scenario), alpha = 0.3) +
  stat_smooth(aes(fill=scenario), method = "loess", alpha = 0.2)+
  theme_classic() +
  scale_color_manual(values = c("blue", "orange", "red")) +
  scale_fill_manual(values = c("blue", "orange", "red")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(breaks=seq(2010, 2100, 10)) + ylab("Mean Area Burned (ha)") + xlab("Year")

e
