debug(utils:::unpackPkgZip)
############################################################################################
###################     LANDIS-II Biomass Validation using GNN/TREE_LIVE data    ######################
###################              Last Updated - April 28, 2017                   ######################

#This script was written by Brooke Cassell and Melissa Lucash
#This script allows a comparison of LANDIS-II year-zero biomass by species at the landscape level 
#with GNN initial biomass. 

### Subsequent sections provide plots of the data in various ways (e.g., total biomass by climate and soil regions, species biomass by climate and soil regions)


#Set Working directory

setwd("I:/Cassell/Malheur Project Files/R_Scripts/Biomass_Validation_LANDIS_GNN/LANDIS-II_GNN_Biomass_Validation_4ha/")

#install and load packages

library(rgdal)
library(raster)
library(plotrix)
library(ggplot2)
library(gdata)

##################   GNN Input Data Biomass Calculations   ##################

# Export entire TREE_LIVE spreadsheet from the GNN database as a .csv file and then read in the entire table.

TREE_LIVE <- read.csv("TREE_LIVE.csv") #This spreadsheet contains all FCIDs and tree information from associated FIA plots

TREE_LIVE_Biomass <- subset(TREE_LIVE [,c(2,4,5,36)]) #Subset to keep only FCID, SPP_SYMBOL, SCIENTIFIC_NAME, and BIOMPH_CRM_FC
#Be aware that the TREE_LIVE spreadsheet only includes data for FCIDs with trees. Potential forest and non-forest are
#not included.

#Import the crosswalk of GNN and LANDIS-II species codes for species in the study area - you will need to create this in the same format as the example file.
species_list <- read.csv("species_list.csv", header = T)

#Keep only TREE_LIVE data for species of interest in the study area
TREE_LIVE_Biomass_Spp_All <- subset(TREE_LIVE_Biomass, SPP_SYMBOL %in% species_list$Species)
TREE_LIVE_Biomass_Spp <- drop.levels(TREE_LIVE_Biomass_Spp_All) #drop unused factor levels, i.e., unused species names
str(TREE_LIVE_Biomass_Spp) #check to be sure that your SPP_SYMBOL levels matches your species list, and that you have the correct columns

#Create a vector of the tree species included in the study using the SPP_SYMBOL included in the TREE_LIVE database
Species <- unique(TREE_LIVE_Biomass_Spp$SPP_SYMBOL)

#Include the total number of hectares in the active cells of your study area. If you are working at 1 ha resolution, then just use the number of active cells.
#If not, then calculate your total hectares and use that value.
Active_cells_LANDIS<-234692 # Number of active cells in your landscape. Example file has 938306 active cells (at 1 Ha resolution)
                            # This includes active but non-forested cells, which allows the total and per species average values
                            # to be calculated across eco-types and compared with LANDIS-II output maps, which will include
                            # biomass in all active cells
Ha_Per_Cell <- 4 #Enter the resolution you're working at (e.g., 4ha/cell = 4). Example file is at a 1-ha resolution
Landscape_Area <- Active_cells_LANDIS*Ha_Per_Cell

# Export the GNN Attribute Table from ArcMap for your study area. Make sure it is saved as a .csv file. The GNN raster layer
# automatically includes a column called "FCID" and another column called "COUNT".
# Add a column called "Hectares". Fill this column by multiplying the "COUNT" column by the number of ha/cell in your landscape (e.g., if you are working at a 1 ha/cell resolution, just copy the values from the "COUNT" column. If you are If your resolution is >1 ha/cell, then you'll need to add a column called "Hectares", which is your COUNT * #ha/cell (e.g., if 4 ha cells, this column is COUNT*4)

GNN_Att_Table <- read.csv("GNN_Attribute_Table_4ha.csv", header = T)
GNN_Att_Table$Hectares <- GNN_Att_Table$COUNT*Ha_Per_Cell
FCID_all <- (GNN_Att_Table[,"FCID"]) #Create a vector of FCID codes in the study area
FCID <- subset(FCID_all, FCID_all >0) #Subset to remove negative FCIDs (these represent "potential" forest and non-forest with no intial tree biomass)

#Testing only - use a few FCIDs that occur on your landscape to allow the loop to run quickly for testing
#FCID<- c(2154:2155)

#Create a loop to keep only FCIDs that occur in the study area and sum the biomass for each species within each plot

plot_table<-NULL #this will become the plot matrix, combining individual trees by species within plots
for (i in 1:length(FCID)){   #For every unique plot number
  for_each_FCID<-(FCID[i])   #fia plot number/FCID
  plot_data<-subset(TREE_LIVE_Biomass_Spp,TREE_LIVE_Biomass_Spp[,"FCID"]==for_each_FCID) #subset all the data for each unique plot
      
  for (j in 1:length(Species)){ #For every species
      for_each_species<-(Species[j]) #For every unique species
      spp_plot_data<-subset(plot_data,plot_data[,"SPP_SYMBOL"]==for_each_species) #all the data for each unique species in each plot
      biomass_spp_plot_data<-sum(spp_plot_data[,"BIOMPH_CRM_FC"]) #sums the biomass (in kg/ha) for each species in each plot
      for_each_row<-cbind.data.frame(for_each_FCID, for_each_species, biomass_spp_plot_data)
      plot_table<-rbind(plot_table, for_each_row) #table of summed biomass for all species in each plot
}
}
print(head(plot_table))

#Merge GNN_Att_Table with the plot_table by FCID so that COUNT (the # of occurrences) is added by FCID.

GNN_plot_table_all <- merge(plot_table, GNN_Att_Table, by.x = "for_each_FCID", by.y = "FCID")

print(head(GNN_plot_table_all))

#Next, average the biomass for each species across the landscape, accounting for the area each plot occurs in

# This loop will multiply each species in each plot by the area, convert kg/ha to g/m^2, and average
# the biomass across the landscape.

### Note ### This script is already modified to accomodate cell sizes of >1ha

GNN_spp_table<-NULL
for (k in 1:length(Species)){  #For every species
  for_each_species_again<-(Species[k])  #for each unique species
  spp_data<-subset(GNN_plot_table_all,GNN_plot_table_all[,"for_each_species"]==for_each_species_again) #all data for each species
  spp_data_present<- spp_data[which(spp_data$biomass_spp_plot_data > 0),] #remove any plots where the species does not occur
  spp_biomass<-(spp_data_present[,"biomass_spp_plot_data"])  #summed biomass within each plot for each species
  species_area<-(spp_data_present[,"Hectares"]) #area in hectares covered by the species
  Total_Hectares<- sum(species_area) #Sum up the total area where the species occurs
  biomass_per_spp_kgha<-spp_biomass*species_area #Multiply species biomass within FCID by ha of that FCID
  biomass_per_spp_kgha_landscape <- sum(biomass_per_spp_kgha) #sum up that species across the whole landscape 
  biomass_per_spp_kgha_average <- biomass_per_spp_kgha_landscape/Total_Hectares #divide by the land area occupied by the species
  biomass_per_spp_gm2<- (biomass_per_spp_kgha_average*1000)/10000  #Then convert the final value to g m-2 (LANDIS units)
  GNN_spp_biomass_SE <- std.error(spp_biomass*1000)/10000 #Calculate standard error in g m-2
  for_each_row_again<-cbind.data.frame(for_each_species_again, biomass_per_spp_gm2, GNN_spp_biomass_SE)
  GNN_spp_table<-rbind(GNN_spp_table, for_each_row_again)
}
print(head(GNN_spp_table))

colnames(GNN_spp_table) <- c("GNN_Spp", "GNN_Biomass", "GNN_SE")

write.csv(GNN_spp_table,("Biomass_Output/GNN_Biomass_landscape.csv"),row.names=F)

##################   LANDIS-II Year-Zero Biomass Calculations   ##################

####Read in the ecoregion map (or landscape map), the same one used for LANDIS-II simulations.
#NOTE: Make sure resolution corresponds to output.  This one is a 4ha ecoregion map.
ecoregion_map_sims<-raster("ecoregion_map/ecoregions4ha1.img")
plot(ecoregion_map_sims) #plot the ecoregion map
ecoregion_DF_sims<-as.data.frame(ecoregion_map_sims) #make a dataframe
colnames(ecoregion_DF_sims)<-'Ecoregion_L' #name column
ecoregion<-unique(ecoregion_DF_sims) #create vector of unique ecoregion names
U_ecoregion<-sort(unique(ecoregion$Ecoregion_L)) #sort ecoregions numerically
U_ecoregion[is.na(U_ecoregion)]<-0  #make NA values(if any) equal to 0

#Read in all the names of the LANDIS maps of biomass at time=0.  

Landis_dir<-("D:/LANDIS-II Runs/Climate_Change_Runs_July26_2017/Historical/replicate1/biomass")
files_all<-list.files(Landis_dir)
files_img<-grep("-0.img",files_all, value=T)
L_files_nob<-grep("TotalBiomass",files_img, value=T, invert=T) #Need to exclude the TotalBiomass maps.  Want only the species maps.
L_files<-grep(".xml",L_files_nob, value=T, invert=T)
unique(L_files)

#Create a vector of LANDIS-II species codes from the lookup table that crosswalks the GNN SPP_SYMBOL with LANDIS-II species codes.
unique_spp_L<-species_list[,"LANDIS_Species"]

#Now read in all the actual LANDIS maps.  For species looping, use the species name (8 letters) that match the filenames in LANDIS.
L_landscape_matrix<-NULL
for (l in 1:length(unique_spp_L)){#for each species...
  spp<-(unique_spp_L[l])
  spp_LANDIS<-as.data.frame(raster(paste(Landis_dir,"/",spp,"-0.img",sep="")))#LANDIS unique spp biomass.
  spp_LANDIS <- na.omit(spp_LANDIS) #remove any rows with na (there probably aren't any)
  spp_LANDIS <- as.data.frame(spp_LANDIS[which(spp_LANDIS > 0),]) #remove all cells where the species does not occur
  LANDIS_species_area <- nrow(spp_LANDIS) #number of cells in which the species occurs
  colnames(spp_LANDIS)<-c("LANDIS")
  sum_biomass <-sum(as.numeric(spp_LANDIS$LANDIS)) #sum species biomass across the landscape
  avg_biomass<-(sum_biomass/LANDIS_species_area) #divide by the number of cells in which the species occurs
  SE_biomass <-std.error(spp_LANDIS$LANDIS)  #Note - this SE isn't really comparable to the GNN biomass SE, which looks at variation across FCIDs. This is landscape-level, and after spin-up, cells are no longer associated with FCIDs.
  landscape_row<-cbind.data.frame(spp, avg_biomass, SE_biomass)
  L_landscape_matrix<-rbind(L_landscape_matrix,landscape_row)
  
} #closes species loop
colnames(L_landscape_matrix)<-c("species", "LANDIS_Biomass_gm2", "SE_Biomass")
print(head(L_landscape_matrix))
write.csv(L_landscape_matrix,"Biomass_Output/LANDIS_Biomass_Landscape.csv", row.names=FALSE)  

#Merge the output files for GNN and LANDIS-II

GNN_SPP_species <- merge(GNN_spp_table,species_list[,], by.x = "GNN_Spp", by.y = "Species" )
GNN_LANDIS_merge <- merge(GNN_SPP_species, L_landscape_matrix, by.x = "LANDIS_Species", by.y = "species")
GNN_LANDIS_Landscape_Biomass <- subset(GNN_LANDIS_merge[,c(1,3:6)])
colnames(GNN_LANDIS_Landscape_Biomass) <- c("Species", "GNN_Biomass_gm2", "GNN_SE_Biomass", "LANDIS_Biomass_gm2", "LANDIS_SE_Biomass")


print(head(GNN_LANDIS_Landscape_Biomass))
write.csv(GNN_LANDIS_Landscape_Biomass,"Biomass_Output/GNN_LANDIS_Biomass_Landscape.csv", row.names=FALSE)

install.packages("RColorBrewer") #install an alternate color palette so more than 8 unique colors can be displayed
library(RColorBrewer)
palette(brewer.pal(n = 11, name = "Spectral"))

par(mfrow=c(1,1))
plot(GNN_LANDIS_Landscape_Biomass$GNN_Biomass_gm2,GNN_LANDIS_Landscape_Biomass$LANDIS_Biomass_gm2, main = "Species Biomass - Landscape Level", col = GNN_LANDIS_Landscape_Biomass$Species,pch = 17, cex = 1.5, xlim=c(0,3700), ylim = c(0,3700), xlab = "GNN Biomass (g m2)", ylab = "LANDIS-II Biomass (g m2)")
abline(0,1)
legend(0,3500,unique(GNN_LANDIS_Landscape_Biomass$Species),col=1:length(GNN_LANDIS_Landscape_Biomass$Species),pch=17)
legend(3200,350, bty="n", legend = paste("r=", format(round(cor(GNN_LANDIS_Landscape_Biomass$GNN_Biomass_gm2,GNN_LANDIS_Landscape_Biomass$LANDIS_Biomass_gm2),2))))
cor.test(GNN_LANDIS_Landscape_Biomass$GNN_Biomass_gm2,GNN_LANDIS_Landscape_Biomass$LANDIS_Biomass_gm2)


############### Plot the data in a bar graph ##################

#Install the reshape2 package for reshaping data

library(reshape2)


#Reorder data into the format needed by ggplot
Biomass_String_all <- melt(data = GNN_LANDIS_Landscape_Biomass, id.vars = "Species") 
SE <- Biomass_String_all[which(Biomass_String_all$variable == "GNN_SE_Biomass" | Biomass_String_all$variable == "LANDIS_SE_Biomass"),3]
Biomass <- Biomass_String_all[which(Biomass_String_all$variable == "GNN_Biomass_gm2" | Biomass_String_all$variable == "LANDIS_Biomass_gm2"),]
Biomass_String_SE <- cbind(Biomass,SE)  #This new dataframe separates GNN and LANDIS biomass into separate groupings with each species SE with each group


barplot <- ggplot(data = Biomass_String_SE, aes(x = Species, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = value-SE, ymax = value+SE), 
                width = .5, position = position_dodge(.9)) +
  ggtitle("GNN and LANDIS-II Landscape Biomass") +
  ylab("Biomass (g -m2)") +
  scale_fill_discrete(name = "Source"); barplot

ggsave("Biomass_Output/barplot.png", width = 10, height = 5)



########### Plot landscape biomass (all species) FIA data vs. LANDIS-II data as scatterplot

library(car)

#Read FIA Biomass map as raster

fia_biomass <- raster("FIA_AGB_gm2/fia_agb_gm2.img")
#plot(fia_biomass)
fia_biomass_df <- as.data.frame(fia_biomass) #convert to dataframe
colnames(fia_biomass_df) <- "FIA_biomass"
head(fia_biomass_df)

#Read LANDIS total biomass map for year 0 as raster

LANDIS_landscape_biomass_all <- raster("biomass/TotalBiomass-0.img") 
#plot(LANDIS_landscape_biomass)
LANDIS_landscape_biomass_all_df<- as.data.frame(LANDIS_landscape_biomass_all) #convert to dataframe
colnames(LANDIS_landscape_biomass_all_df) <- "LANDIS_biomass"
summary(LANDIS_landscape_biomass_all_df)

#remove shrubs and grass biomass
intoresp_raster <- raster("biomass/intoresp-0.img")
toleresp_raster <- raster("biomass/toleresp-0.img")
nonnseed_raster <- raster("biomass/nonnseed-0.img")
fixnresp_raster <- raster("biomass/fixnresp-0.img")
natvgrss_raster <- raster("biomass/natvgrss-0.img")
invsgrss_raster <- raster("biomass/invsgrss-0.img")

intoresp_df <- as.data.frame(intoresp_raster)
toleresp_df <- as.data.frame(toleresp_raster)
nonnseed_df <- as.data.frame(nonnseed_raster)
fixnresp_df <- as.data.frame(fixnresp_raster)
natvgrss_df <- as.data.frame(natvgrss_raster)
invsgrss_df <- as.data.frame(invsgrss_raster)

non_tree_biomass <- as.data.frame(intoresp_df + toleresp_df + nonnseed_df + fixnresp_df + natvgrss_df + invsgrss_df)

LANDIS_landscape_biomass_df <- as.data.frame(LANDIS_landscape_biomass_all_df - non_tree_biomass)
summary(LANDIS_landscape_biomass_all_df)

FIA_LANDIS_comparison_all <- as.data.frame(cbind(fia_biomass_df$FIA_biomass, LANDIS_landscape_biomass_df$LANDIS_biomass))
colnames(FIA_LANDIS_comparison_all) <- c("FIA_biomass", "LANDIS_biomass")
head(FIA_LANDIS_comparison_all)


FIA_LANDIS_comparison_nona <- na.omit(FIA_LANDIS_comparison_all) #remove na
FIA_LANDIS_comparison <- FIA_LANDIS_comparison_nona[which(FIA_LANDIS_comparison_nona$FIA_biomass >0),] #remove FIA plots w/ 0 biomass
FIA_LANDIS_comparison <- FIA_LANDIS_comparison[which(FIA_LANDIS_comparison$LANDIS_biomass >0),] #remove FIA plots w/ 0 biomass

#Scatterplot 
#plot(FIA_LANDIS_comparison$FIA_biomass, FIA_LANDIS_comparison$LANDIS_biomass)
#abline(0,1) #1 to 1 line
#cor(FIA_LANDIS_comparison$FIA_biomass, FIA_LANDIS_comparison$LANDIS_biomass)



############Plot landscape biomass (all species) GNN data vs. LANDIS-II data as scatterplot

#To get biomass for each FCID, use the plot_table created above

head(plot_table)
plot_table$for_each_FCID <- as.factor(plot_table$for_each_FCID)
str(plot_table)

#Sum biomass for all species within FCIDs to get total biomass in each cell type (FCID)
 
detach(plyr)
library(dplyr) #need to make sure the plyr is not installed, or else this won't work

GNN_FCID_Biomass_Totals <- plot_table %>% 
  group_by(for_each_FCID) %>% 
  summarise(biomass_spp_plot_data = sum(biomass_spp_plot_data))
summary(GNN_FCID_Biomass_Totals)
head(GNN_FCID_Biomass_Totals)  


GNN_FCID_Biomass_Totals$Biomass_gm2 <- GNN_FCID_Biomass_Totals$biomass_spp_plot_data*0.1 #multiply to convert kg/ha to g/m2
summary(GNN_FCID_Biomass_Totals)


#Merge this table with the GNN_Att_Table on FCID to attach Mapcodes ("VALUE") with biomass
Mapcode_FCID_Biomass_merge <- merge(GNN_FCID_Biomass_Totals, GNN_Att_Table, by.x = "for_each_FCID", by.y = "FCID"  )

Mapcode_Biomass <- subset(Mapcode_FCID_Biomass_merge[,c(4,3)])
Mapcode_Biomass$Biomass_gm2 <- as.integer(Mapcode_Biomass$Biomass_gm2)
colnames(Mapcode_Biomass)[1] <- "Mapcode"
head(Mapcode_Biomass)
summary(Mapcode_Biomass)

write.csv(Mapcode_Biomass, ("Mapcode_Biomass_Lookup.csv"), row.names=F)


#import map of mapcodes at 4ha resolution w/ na values for inactive cells. Raster has 435,000 cells

mapcodes_raster <- raster("mapcodes_na.img")
#plot(mapcodes_raster)
mapcodes_raster_df <- as.data.frame(mapcodes_raster) #create a data frame
head(mapcodes_raster_df)
mapcodes <- subset(mapcodes_raster_df,!(is.na(mapcodes_raster_df))) #remove na cells. Leaves 234,692 cells (equal to the FIA and LANDIS maps from the previous section)
head(mapcodes)
colnames(mapcodes) <- "Mapcode"
str(mapcodes$Mapcode)
summary(mapcodes)

#Join the mapcodes map to the Mapcode_Biomass lookup table to populate the input GNN map with the corresponding biomass for each cell

detach(dplry)
library(plyr)
GNN_biomass_join <- join(mapcodes, Mapcode_Biomass, by = "Mapcode") #Using join instead of merge, because join preserves the order
head(GNN_biomass_join)
summary(GNN_biomass_join)
GNN_biomass_join$Biomass_gm2[is.na(GNN_biomass_join$Biomass_gm2)] <- 0 #because there are no input biomass values for non-forest mapcodes, they automatically classifed as "na" in the join. Convert these to 0 biomass
GNN_LANDIS_Total_Biomass <- as.data.frame(cbind(GNN_biomass_join$Biomass_gm2, FIA_LANDIS_comparison_nona$LANDIS_biomass))
colnames(GNN_LANDIS_Total_Biomass) <- c("GNN_Biomass", "LANDIS_Biomass")
head(GNN_LANDIS_Total_Biomass)
summary(GNN_LANDIS_Total_Biomass)



#plot(GNN_biomass_join$Biomass_gm2, FIA_LANDIS_comparison_nona$FIA_biomass )
#cor(GNN_biomass_join$Biomass_gm2, FIA_LANDIS_comparison_nona$FIA_biomass)
#plot(GNN_biomass_join$Biomass_gm2, FIA_LANDIS_comparison_nona$FIA_biomass)
#abline(0,1)

#Scatter plot of input GNN biomass for each cell compared with its corresponding output LANDIS-II Year 0 Total Biomass
par(mfrow=c(1,1))
plot(GNN_LANDIS_Total_Biomass$GNN_Biomass, GNN_LANDIS_Total_Biomass$LANDIS_Biomass)
abline(0,1, col = "blue")
legend("topright", bty="n", legend = paste("r=", format(round(cor(GNN_LANDIS_Total_Biomass$GNN_Biomass, GNN_LANDIS_Total_Biomass$LANDIS_Biomass),2))))
text(x=43000, y=100, labels="1-to-1 line _____", col = "blue")
cor(GNN_LANDIS_Total_Biomass$GNN_Biomass, GNN_LANDIS_Total_Biomass$LANDIS_Biomass)

GNN_LANDIS_Total_Biomass_10000 <- as.data.frame(GNN_LANDIS_Total_Biomass[which(GNN_LANDIS_Total_Biomass$GNN_Biomass<10000),])
GNN_LANDIS_Total_Biomass_no0 <- as.data.frame(GNN_LANDIS_Total_Biomass_10000[which(GNN_LANDIS_Total_Biomass_10000$GNN_Biomass>0),])
qqplot(GNN_LANDIS_Total_Biomass_no0$GNN_Biomass, GNN_LANDIS_Total_Biomass_no0$LANDIS_Biomass)
abline(0,1)

xsq <- chisq.test(GNN_LANDIS_Total_Biomass_no0$GNN_Biomass, GNN_LANDIS_Total_Biomass_no0$LANDIS_Biomass, correct = F)
xsq

ks <- ks.test(GNN_LANDIS_Total_Biomass_no0$GNN_Biomass, GNN_LANDIS_Total_Biomass_no0$LANDIS_Biomass)
ks
######Look at data at the ecoregion level##########

#Merge ecoregions with GNN/LANDIS data
ecoregion_DF_sims_nona <- na.omit(ecoregion_DF_sims)
ecoregion_DF_sims_no0 <- as.data.frame(ecoregion_DF_sims_nona[which(ecoregion_DF_sims_nona$Ecoregion_L>0),])
colnames(ecoregion_DF_sims_no0) <- "Ecoregion"
ecoregion_DF_sims_no0$ClimateRegion <- as.factor(substr(ecoregion_DF_sims_no0$Ecoregion, 0, 1)) #create column of just climate regions (1st digit of the ecoregions column)
ecoregion_DF_sims_no0$SoilRegion <- as.factor(substr(ecoregion_DF_sims_no0$Ecoregion, 3, 3)) #create column of just soil regions (3rd digit of the ecoregions column


GNN_LANDIS_ecoregion <- as.data.frame(cbind(GNN_LANDIS_Total_Biomass,ecoregion_DF_sims_no0))
head(GNN_LANDIS_ecoregion)


#Remove all non-forest sites/sites where GNN has 0 biomass
gnn_landis_ecoregion_No0 <- GNN_LANDIS_ecoregion[which(GNN_LANDIS_ecoregion$GNN_Biomass >0),]

#Plot GNN_biomass x LANDIS_Biomass by ecoregion
#plot(gnn_landis_ecoregion_No0$GNN_Biomass, gnn_landis_ecoregion_No0$LANDIS_Biomass, col = gnn_landis_ecoregion_No0$Ecoregion)
#abline(0,1)
#legend(40000,12000,unique(gnn_landis_ecoregion_No0$Ecoregion),col=1:length(gnn_landis_ecoregion_No0$Ecoregion), pch=1)

par(mfrow=c(1,2))

#Plot GNN_biomass x LANDIS_Biomass by Climate Region
plot(gnn_landis_ecoregion_No0$GNN_Biomass, gnn_landis_ecoregion_No0$LANDIS_Biomass, col = gnn_landis_ecoregion_No0$ClimateRegion, xlim = c(0,15000), ylim= c(0,15000), main = "GNN by LANDIS Biomss by Climate Region")
abline(0,1)
legend(0,15000,unique(gnn_landis_ecoregion_No0$ClimateRegion),col=1:length(gnn_landis_ecoregion_No0$ClimateRegion), pch=1)


#Plot GNN_biomass x LANDIS_Biomass by Soil Region
plot(gnn_landis_ecoregion_No0$GNN_Biomass, gnn_landis_ecoregion_No0$LANDIS_Biomass, col = gnn_landis_ecoregion_No0$SoilRegion, xlim = c(0,15000), ylim= c(0,15000), main = "GNN by LANDIS Biomss by Soil Region")
abline(0,1)
legend(0,15000,unique(gnn_landis_ecoregion_No0$SoilRegion),col=1:length(gnn_landis_ecoregion_No0$SoilRegion), pch=1)


#Boxplots

par(mfrow=c(1,2))
boxplot(gnn_landis_ecoregion_No0$GNN_Biomass~gnn_landis_ecoregion_No0$ClimateRegion, main = "GNN Biomass by Climate Region")

boxplot(gnn_landis_ecoregion_No0$LANDIS_Biomass~gnn_landis_ecoregion_No0$ClimateRegion, ylim = c(0,55000), main = "LANDIS-II Biomass by Climate Region")

boxplot(gnn_landis_ecoregion_No0$GNN_Biomass~gnn_landis_ecoregion_No0$SoilRegion, ylim = c(0,55000), main = "GNN Biomass by Soil Region")

boxplot(gnn_landis_ecoregion_No0$LANDIS_Biomass~gnn_landis_ecoregion_No0$SoilRegion, ylim = c(0,55000), main = "GNN Biomass by Soil Region")


################### Plot average biomass for ecoregions #######################

GNN_bio_by_climate <- tapply(gnn_landis_ecoregion_No0$GNN_Biomass, gnn_landis_ecoregion_No0$ClimateRegion, mean)
LANDIS_bio_by_climate <- tapply(gnn_landis_ecoregion_No0$LANDIS_Biomass, gnn_landis_ecoregion_No0$ClimateRegion, mean)
Climate_Region_Biomass <- as.data.frame(cbind(GNN_bio_by_climate, LANDIS_bio_by_climate))
Climate_Region_Biomass$ClimateRegion <- c(1,2,3,4,5)


plot(Climate_Region_Biomass$GNN_bio_by_climate,Climate_Region_Biomass$LANDIS_bio_by_climate, col = Climate_Region_Biomass$ClimateRegion, pch = 17, cex = 2, xlim = c(0,10000), ylim= c(0,10000), main = "Biomass by Climate Region", xlab = "GNN Biomass (g m2)", ylab = "LANDIS-II Biomass (g m2)")
abline(0,1)
legend("topleft",bty="n",legend= unique(Climate_Region_Biomass$ClimateRegion),col=1:length(Climate_Region_Biomass$ClimateRegion), pch=17)
legend("bottomright", bty="n", legend = paste("r=", format(round(cor(Climate_Region_Biomass$GNN_bio_by_climate,Climate_Region_Biomass$LANDIS_bio_by_climate),2))))



GNN_bio_by_soil <- tapply(gnn_landis_ecoregion_No0$GNN_Biomass, gnn_landis_ecoregion_No0$SoilRegion, mean)
LANDIS_bio_by_soil <- tapply(gnn_landis_ecoregion_No0$LANDIS_Biomass, gnn_landis_ecoregion_No0$SoilRegion, mean)
Soil_Region_Biomass <- as.data.frame(cbind(GNN_bio_by_soil,LANDIS_bio_by_soil))
Soil_Region_Biomass$SoilRegion <- c(1,2,3,4,5)


plot(Soil_Region_Biomass$GNN_bio_by_soil,Soil_Region_Biomass$LANDIS_bio_by_soil, col = Soil_Region_Biomass$SoilRegion, xlim = c(0,10000), ylim= c(0,10000), pch = 17, cex = 2, main = "Biomass by Soil Region", xlab = "GNN Biomass (g m2)", ylab = "LANDIS-II Biomass (g m2)")
abline(0,1)
legend("topleft",bty="n",legend= unique(Soil_Region_Biomass$SoilRegion),col=1:length(Soil_Region_Biomass$SoilRegion), pch=17)
legend("bottomright", bty="n",legend = paste("r=", format(round(cor(Soil_Region_Biomass$GNN_bio_by_soil,Soil_Region_Biomass$LANDIS_bio_by_soil),2))))

cor.test(Climate_Region_Biomass$GNN_bio_by_climate,Climate_Region_Biomass$LANDIS_bio_by_climate)













############ Plot GNN vs. LANDIS-II biomass by species as scatterplots ##################

PIPO_plot_table <- subset(plot_table,plot_table[,"for_each_species"]=="PIPO")
head(PIPO_plot_table)
write.csv(PIPO_plot_table, "PIPO_plot_table.csv", row.names = F)

gnn_PIPO_biomass<-raster("gnn_pipo_bio.img")
plot(gnn_PIPO_biomass) #plot the map
gnn_PIPO_biomass_df<-as.data.frame(gnn_PIPO_biomass) #make a dataframe
head(gnn_PIPO_biomass_df)
summary(gnn_PIPO_biomass_df)
gnn_PIPO_biomass_df[is.na(gnn_PIPO_biomass_df)]<-0  #make NA values(if any) equal to 0
colnames(gnn_PIPO_biomass_df) <- "GNN_biomass"

landis_PIPO_biomass <- raster("biomass/pinupond-0.img")
plot(landis_PIPO_biomass)
landis_PIPO_biomass_df <- as.data.frame(landis_PIPO_biomass)
colnames(landis_PIPO_biomass_df) <- "LANDIS_biomass"


plot(landis_PIPO_biomass_df$LANDIS_biomass, gnn_PIPO_biomass_df$GNN_biomass)
str(landis_PIPO_biomass_df)
summary(gnn_PIPO_biomass_df)
PIPO_Biomass_comparison <- as.data.frame(cbind(gnn_PIPO_biomass_df$GNN_biomass,landis_PIPO_biomass_df$LANDIS_biomass))
head(PIPO_Biomass_comparison)
colnames(PIPO_Biomass_comparison) <- c("GNN_biomass", "LANDIS_biomass")
PIPO_Biomass_comparison_no0 <- PIPO_Biomass_comparison[which(PIPO_Biomass_comparison$GNN_biomass>0),]
PIPO_Biomass_comparison_10000 <- PIPO_Biomass_comparison_no0[which(PIPO_Biomass_comparison_no0$GNN_biomass<10000),]
head(PIPO_Biomass_comparison_10000)
plot(PIPO_Biomass_comparison_no0$GNN_biomass, PIPO_Biomass_comparison_no0$LANDIS_biomass)

cor(PIPO_Biomass_comparison_no0$GNN_biomass, PIPO_Biomass_comparison_no0$LANDIS_biomass)


PIPO_Biomass_comparison_10000 <- PIPO_Biomass_comparison[which(PIPO_Biomass_comparison$GNN_biomass<10000),]
plot(PIPO_Biomass_comparison_10000$GNN_biomass, PIPO_Biomass_comparison_10000$LANDIS_biomass)
abline(0,1)

cor(PIPO_Biomass_comparison_10000$GNN_biomass, PIPO_Biomass_comparison_10000$LANDIS_biomass)
qqplot(PIPO_Biomass_comparison$GNN_biomass, PIPO_Biomass_comparison$LANDIS_biomass)
qqplot(PIPO_Biomass_comparison_10000$GNN_biomass, PIPO_Biomass_comparison_10000$LANDIS_biomass)


