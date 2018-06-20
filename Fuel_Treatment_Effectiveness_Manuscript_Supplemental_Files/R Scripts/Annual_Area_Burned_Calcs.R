#### Project: Optimizing placement of fuel treatments and accelerating prescribed fire 
#### ameliorates extreme weather-driven wildfires in western mixed-conifer forests by 
#### Brooke A. Cassell, Portland State University, Dissertation defended February 2018


#This script calculates annual area burned across replicates.
#Written by Brooke Cassell October 2017


library(dplyr)
library(ggplot2)

Historical_Area_Burned <- read.csv("W:/Treatments_Results_Oct9_2017/Area_Burned_Data/Area_Burned_Historical.csv", strip.white = T, header = T)
Historical_Area_Burned$rep <- as.factor(rep(1:10, each=100))

Extreme_Area_Burned <- read.csv("W:/Treatments_Results_Oct9_2017/Area_Burned_Data/Area_Burned_Extreme.csv", strip.white = T, header = T)
Extreme_Area_Burned$rep <- as.factor(rep(1:10, each=100))


hist_by_scenario <- group_by(Historical_Area_Burned, harvest, rep, add = TRUE)
hist_area_by_scenario <- summarise(hist_by_scenario, area_burned = mean(Log_Area))
hist_area_by_scenario <- as.data.frame(hist_area_by_scenario)

hist_area_by_scenario$harvest<- factor(hist_area_by_scenario$harvest, levels=c("Untreated", "BAU", "Opt_BAU", "RxFire", "Opt_RxFire", "RxFire3x", "Opt_RxFire3x"))

par(mfrow=c(1,1), xpd = NA, cex.axis = 0.8)
boxplot(hist_area_by_scenario$area_burned ~ hist_area_by_scenario$harvest, las=2,
        ylab = "Log Area Burned (ha)")
text(x = 1, y = , 7, paste0(6.13))
text(x = 2, y = , 7, paste0(5.59))
text(x = 3, y = , 7, paste0(5.47))
text(x = 4, y = , 7, paste0(5.27))
text(x = 5, y = , 7, paste0(5.39))
text(x = 6, y = , 7, paste0(5.00))
text(x = 7, y = , 7, paste0(5.29))
title(main = "Annual Area Burned - Historical Weather", line = 2)

###

ext_by_scenario <- group_by(Extreme_Area_Burned, harvest, rep, add = TRUE)
ext_area_by_scenario <- summarise(ext_by_scenario, area_burned = mean(Log_Area))
ext_area_by_scenario <- as.data.frame(ext_area_by_scenario)

ext_area_by_scenario$harvest<- factor(ext_area_by_scenario$harvest, levels=c("Untreated", "BAU", "Opt_BAU", "RxFire", "Opt_RxFire", "RxFire3x", "Opt_RxFire3x"))

par(mfrow=c(1,1), xpd = NA, cex.axis = 0.8)
boxplot(ext_area_by_scenario$area_burned ~ ext_area_by_scenario$harvest, las=2,
        ylab = "Log Area Burned (ha)")
text(x = 1, y = , 9.2, paste0(8.47))
text(x = 2, y = , 9.2, paste0(7.74))
text(x = 3, y = , 9.2, paste0(7.91))
text(x = 4, y = , 9.2, paste0(7.46))
text(x = 5, y = , 9.2, paste0(7.64))
text(x = 6, y = , 9.2, paste0(6.99))
text(x = 7, y = , 9.2, paste0(7.67))
title(main = "Annual Area Burned - Extreme Weather", line = 2)

###### Statistical Tests #######

#Test for normality of the log-transformed data
shapiro.test(hist_area_by_scenario$area_burned) #W = 0.98, p = 0.36, data are normally distributed
shapiro.test(ext_area_by_scenario$area_burned) #W = 0.99, p = 0.74, data are normally distributed

#Test for homoscedasticity of the log-transformed data
bartlett.test(hist_area_by_scenario$area_burned ~ hist_area_by_scenario$harvest) #p = 0.88, equal variances
bartlett.test(ext_area_by_scenario$area_burned ~ ext_area_by_scenario$harvest) #p = 0.11, equal variances

#Run an ANOVA
a <- aov(hist_area_by_scenario$area_burned ~ hist_area_by_scenario$harvest) #df = 6, F = 10.38, p = 5.64e-80 - reject H0
summary(a)

#Post-Hoc Tests
TukeyHSD(a)

###
b <- aov(ext_area_by_scenario$area_burned ~ ext_area_by_scenario$harvest) #df = 6, F = 13.81, p = 6.01e-10 - reject H0
summary(b)

#Post-Hoc Tests
TukeyHSD(b)
