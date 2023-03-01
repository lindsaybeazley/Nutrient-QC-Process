#Nutrient Bottle Data Quality Control Procedures for AZMP/AZOMP

#Written by: Lindsay Beazley

#Last Updated: February 28, 2023



#Load packages:

library(tidyr)
library(dplyr)
library(readxl)
library(data.table)
library(tidyverse)
library(ggplot2)


#Set working directory and load data:

setwd("C:/AZMP/7_Data Management/1_ODIS_Data_Submissions/2022/BBMP/Nutrients") #set your working directory



# Step 1: Load Nutrient Data ------------------------------------

Nuts <- read_excel("NUTRIENT_FILENAME_GOES_HERE.xlsx", skip=4, sheet=1, col_types='text')[, 1:12] #Skip the first 4 rows containing the header

#Note: The col_types argument is set to read all columns in as text. This is to allow you to read
##in those columns that have cells with NAs.

#The [ , 1:12] appended at the end reads in only the first 12 columns. The BIO rating guide and BioChem flag scheme are skipped.



# Step 2: Fill in Sample ID & Format ------------------------------------

# Fill in Sample ID:
Nuts2 <- Nuts  %>% 
  mutate(SAMPLE_ID = na_if(SAMPLE_ID, "")) %>%
  fill(SAMPLE_ID)
head(Nuts2)


##Add repeating Rep ID field after SAMPLE_ID field:
Nuts2 <- Nuts2 %>%
  add_column(RepID = (Nuts2$RepID <- sequence(rle(as.character(Nuts2$SAMPLE_ID))$lengths)), .after="SAMPLE_ID")


###Remove rows with reference material:
Nuts3 <- Nuts2[- grep("CH", Nuts2$SAMPLE_ID),]


####Change DL entries to NA (if applicable):
#Nuts3[Nuts3 == "DL"] <- NA


#####Omit NAs (if applicable):
#Nuts4 <- na.omit(Nuts3)



# Step 3: Evaluate Differences Between Replicates ------------------------------------


#Change to dataframe (because I like working with dataframe):
Nutsdf<- as.data.frame(Nuts3, na.rm=TRUE)


##onvert NA entries to blanks:
Nutsdf[is.na(Nutsdf)] <- "" 


###Change Nutrient fields to numeric field:

Nutsdf$NITRATE <- as.numeric(Nutsdf$NITRATE)
Nutsdf$NITRITE <- as.numeric(Nutsdf$NITRITE)
Nutsdf$PHOSPHATE <- as.numeric(Nutsdf$PHOSPHATE)
Nutsdf$SILICATE <- as.numeric(Nutsdf$SILICATE)
Nutsdf$AMMONIUM <- as.numeric(Nutsdf$AMMONIUM)
Nutsdf$RepID <- as.character(Nutsdf$RepID)


####View and check sample ID column for any glaringly bad sample IDs:
View(Nutsdf) 


#####Create columns that calculate the difference between rep 1 and rep 2:

Diff <- Nutsdf %>%
  group_by(SAMPLE_ID) %>%
  mutate(diff_NITRATE = NITRATE - lag(NITRATE, default = first(NITRATE))) %>%
  mutate(diff_NITRITE = NITRITE - lag(NITRITE, default = first(NITRITE))) %>%
  mutate(diff_PHOSPHATE = PHOSPHATE - lag(PHOSPHATE, default = first(PHOSPHATE))) %>%
  mutate(diff_SILICATE = SILICATE - lag(SILICATE, default = first(SILICATE))) %>%
  mutate(diff_AMMONIUM = AMMONIUM - lag(AMMONIUM, default = first(AMMONIUM)))
head(Diff)


write.csv(Diff, file="Differences_Replicates_MISSION_NAME_GOES_HERE.csv")


#The BIO datashop has been using these tolerated differences:

#NO3+NO2           3.5
#NO2               0.1
#PO4               0.5
#SiO               4.0
#NH4               0.1

#Peter Thamer (nutrient analyzer at BIO) tends to flag lower differences as the replicates can and should be much closer in value: 

#NO3+NO2           0.5
#NO2              0.05
#PO4              0.05
#SiO               1.0
#NH4               0.1




# Step 4: Plot Data by Environmental Factor ------------------------------------


#Load QAT data using one of the following 3 options, to get depth, temp, etc. and merge it with nutrient data. 
#QAT files are comma delimited = CSV:

#Option 1: Directly load individual QAT files (make sure path leads to calibrated QATs, if available):
CombinedQATs <- list.files(path = "path\\Step_2_Apply_Calibrations\\QAT\\",
                           pattern = "*.QAT$", full.names = TRUE) %>% 
  lapply(read_csv, col_types= cols(.default = col_character())) %>% # Store all files in list
  bind_rows                                                                               
CombinedQATs

CombinedQATs <- as.data.frame(CombinedQATs)

CombinedQATs <- CombinedQATs[, -17:-40] #Remove unneeded columns

colnames(CombinedQATs)[6] <- "SAMPLE_ID"



#Option 2: Load collated QAT xlsx or csv file (if CTD data have been post-processed):
setwd("path\\Step_2_Apply_Calibrations\\QAT\\")

CombinedQATs <- read_excel("MISSIONID_QAT_Corrected.xlsx") 

CombinedQATs <-as.data.frame(CombinedQATs)

colnames(CombinedQATs)[7] <- "SAMPLE_ID"

#Extract only the needed columns
CombinedQATs <- CombinedQATs[, -17:-39]



#Option 3: Load IML bottle report from ANDES (if sGSL mission and CTD data have not been post-processed):
Bottlerep <- read_excel("path/ANDES_Reports/IML bottle report CAR-2022-025 (2022-10-01).xlsx", sheet=1)

Bottlerep_df <- as.data.frame(Bottlerep)

#rename Bottle UID to SAMPLE_ID
names(Bottlerep_df)[1] <- "SAMPLE_ID"


#########

##Merge Nutrient and QAT Data Together:

jointNuts <- merge(Nutsdf, CombinedQATs, by = 'SAMPLE_ID')
nrow(Nutsdf)
nrow(jointNuts) #do they match? 


jointNuts$NITRATE <- as.numeric(jointNuts$NITRATE)
jointNuts$NITRITE <- as.numeric(jointNuts$NITRITE)
jointNuts$PHOSPHATE <- as.numeric(jointNuts$PHOSPHATE)
jointNuts$SILICATE <- as.numeric(jointNuts$SILICATE)
jointNuts$AMMONIUM <- as.numeric(jointNuts$AMMONIUM)
jointNuts$PrDM <- as.numeric(jointNuts$PrDM)
jointNuts$event <- as.numeric(jointNuts$event)
jointNuts$RepID <- as.character(jointNuts$RepID)


###Exploratory plots to illustrate pattern with depth:

plot(jointNuts$NITRATE ~ jointNuts$PrDM, xlab = "Pressure (db)", ylab= "NITRATE (µg/l)")
plot(jointNuts$NITRITE ~ jointNuts$PrDM, xlab = "Pressure (db)", ylab= "NITRITE (µg/l)")
plot(jointNuts$PHOSPHATE ~ jointNuts$PrDM, xlab = "Pressure (db)", ylab= "PHOSPHATE (µg/l)")
plot(jointNuts$SILICATE ~ jointNuts$PrDM, xlab = "Pressure (db)", ylab= "SILICATE (µg/l)")
plot(jointNuts$AMMONIUM ~ jointNuts$PrDM, xlab = "Pressure (db)", ylab= "AMMONIUM (µg/l)")


###By temperature:

plot(jointNuts$NITRATE ~ jointNuts$T090C, xlab = "Temperature (C)", ylab= "NITRATE (µg/l)")
plot(jointNuts$NITRITE ~ jointNuts$T090C, xlab = "Temperature (C)", ylab= "NITRITE (µg/l)")
plot(jointNuts$PHOSPHATE ~ jointNuts$T090C, xlab = "Temperature (C)", ylab= "PHOSPHATE (µg/l)")
plot(jointNuts$SILICATE ~ jointNuts$T090C, xlab = "Temperature (C)", ylab= "SILICATE (µg/l)")
plot(jointNuts$AMMONIUM ~ jointNuts$T090C, xlab = "Temperature (C)", ylab= "AMMONIUM (µg/l)")


###By salinity:

plot(jointNuts$NITRATE ~ jointNuts$Sal00, xlab = "Salinity", ylab= "NITRATE (µg/l)")
plot(jointNuts$NITRITE ~ jointNuts$Sal00, xlab = "Salinity", ylab= "NITRITE (µg/l)")
plot(jointNuts$PHOSPHATE ~ jointNuts$Sal00, xlab = "Salinity", ylab= "PHOSPHATE (µg/l)")
plot(jointNuts$SILICATE ~ jointNuts$Sal00, xlab = "Salinity", ylab= "SILICATE (µg/l)")
plot(jointNuts$AMMONIUM ~ jointNuts$Sal00, xlab = "Salinity", ylab= "AMMONIUM (µg/l)")



# Step 5: Plot Data Across Time/Year (mostly for fixed stations) --------


ggplot(jointNuts, aes(x=SAMPLE_ID, y=NITRATE, colour=RepID)) +
  geom_point(aes(y=NITRATE, colour=RepID)) +
  scale_color_identity(guide = "legend", name="Rep ID") +
  theme(legend.position = "top") +
  theme(axis.text.x = element_blank())

ggplot(jointNuts, aes(x=SAMPLE_ID, y=NITRITE, colour=RepID)) +
  geom_point(aes(y=NITRITE, colour=RepID)) +
  scale_color_identity(guide = "legend", name="Rep ID") +
  theme(legend.position = "top") +
  theme(axis.text.x = element_blank())

ggplot(jointNuts, aes(x=SAMPLE_ID, y=PHOSPHATE, colour=RepID)) +
  geom_point(aes(y=PHOSPHATE, colour=RepID)) +
  scale_color_identity(guide = "legend", name="Rep ID") +
  theme(legend.position = "top") +
  theme(axis.text.x = element_blank())

ggplot(jointNuts, aes(x=SAMPLE_ID, y=SILICATE, colour=RepID)) +
  geom_point(aes(y=SILICATE, colour=RepID)) +
  scale_color_identity(guide = "legend", name="Rep ID") +
  theme(legend.position = "top") +
  theme(axis.text.x = element_blank())

ggplot(jointNuts, aes(x=SAMPLE_ID, y=AMMONIUM, colour=RepID)) +
  geom_point(aes(y=AMMONIUM, colour=RepID)) +
  scale_color_identity(guide = "legend", name="Rep ID") +
  theme(legend.position = "top") +
  theme(axis.text.x = element_blank())

#Plotting by event is also useful. May wish to plot data by depth layer. If so, move to step 6.
#If not, move to Step 7.


# Step 6: Extract Data by Depth Stratum and Plot (mostly for fixed stations)  --------


#Extract surface data and plot nutrients consecutively:

Samples_surf <- jointNuts %>% 
  filter(PrDM<4)
Samples_surf 

tiff("path//NITRATE_Surface_MissionID.tiff", 
     units="in", width=20, height=6, res=100)

ggplot(Samples_surf, aes(x=SAMPLE_ID, y=NITRATE, colour=RepID)) +
  geom_point(aes(y=NITRATE, colour=RepID)) +
  scale_color_identity(guide = "legend", name="Rep ID") +
  theme(legend.position = "top")
dev.off()


#Extract 5 m data:

Samples_5m <- jointNuts %>% 
  filter(PrDM>4 & PrDM<7)
Samples_5m 

tiff("path//NITRATE_5m_MISSIONID.tiff", 
     units="in", width=20, height=6, res=100)

ggplot(Samples_5m, aes(x=SAMPLE_ID, y=NITRATE, colour=RepID)) +
  geom_point(aes(y=NITRATE, colour=RepID)) +
  scale_color_identity(guide = "legend", name="Rep ID") +
  theme(legend.position = "top") 
dev.off()


#Extract 10 m data:

Samples_10m <- jointNuts %>% 
  filter(PrDM>8 & PrDM<12)
Samples_10m 

tiff("path//NITRATE_10m_MISSIONID.tiff", 
     units="in", width=20, height=6, res=100)

ggplot(Samples_10m, aes(x=SAMPLE_ID, y=NITRATE, colour=RepID)) +
  geom_point(aes(y=NITRATE, colour=RepID)) +
  scale_color_identity(guide = "legend", name="Rep ID") +
  theme(legend.position = "top") 
dev.off()


#Extract 60 m

Samples_60m <- jointNuts %>% 
  filter(PrDM>40)
Samples_60m 

tiff("path//AMMONIUM_60m_MISSIONID.tiff", 
     units="in", width=24, height=6, res=100)

ggplot(Samples_60m, aes(x=SAMPLE_ID, y=NITRATE, colour=RepID)) +
  geom_point(aes(y=NITRATE, colour=RepID)) +
  scale_color_identity(guide = "legend", name="Rep ID") +
  theme(legend.position = "top") 
dev.off()



# Step 7: Plot Nutrient Vertical Profiles by Sampling Event ----------------------

##This code is most useful for shelf-wide data, and helps to a) detect inversion, b) evaluate vertical structure, and
##c) examine divergence between replicates and eliminate data if applicable. The plots and the data are examined outside
##R, and the data modified in the lab spreadsheet if applicable.
  
#First, create the empty folders in your working directory to store the plots.


# NITRATE

out_dir = "path/Nutrients_By_Event/NITRATE"
Events = unique(jointNuts$event)


for (i in seq(1, length(Events), 1)){
  
  Nuts_subset <- (jointNuts[jointNuts$event %in% Events[i],])
  
  plot <- ggplot(Nuts_subset, aes(x=PrDM)) +
    geom_hline(yintercept = seq(0, 25, by=0.5), linetype='dotted', col = 'darkgrey') + #Add in lines to denote 0.05 threshold 
    geom_point(aes(y=NITRATE, colour=RepID), size=2.5) +
    guides(colour = guide_legend(label.theme = element_text(size=15))) +
    scale_color_identity(guide = "legend", name="Rep ID") +
    coord_flip() +
    scale_x_reverse() +
    theme_bw() +
    ylab("NITRATE (µg/l)")+
    xlab("Pressure (dbar)") +
    theme(axis.text.y=element_text(size=12)) +
    theme(axis.text.x=element_text(size=12)) +
    theme(axis.title.y=element_text(size=15)) +
    theme(axis.title.x=element_text(size=15)) +
    scale_y_continuous(limits = c(0, 25)) + #examine data and select max limit
    
    facet_wrap(~event, scales="free_y")+
    theme(strip.text.x = element_text(size=17, face="bold", vjust=1))+
    theme(strip.text.y = element_text(size=17, face="bold", vjust=1))+
    theme(legend.position="top") 
  #To make filename
  eventout <- ifelse(Events[i] < 10, paste0('00', Events[i]),
                     ifelse(Events[i]<100, paste0('0', Events[i]),
                            Events[i]))
  
  png(paste(out_dir, paste0("MISSIONID_NITRATE_Event_", eventout, '.png'), sep='/'), width = 10, height = 8, units = 'in', res = 250)
  print(plot)
  
  dev.off()
  
}


# PHOSPHATE

out_dir = "path/Nutrients_By_Event/PHOSPHATE"
Events = unique(jointNuts$event)


for (i in seq(1, length(Events), 1)){
  
  Nuts_subset <- (jointNuts[jointNuts$event %in% Events[i],])
  
  plot <- ggplot(Nuts_subset, aes(x=PrDM)) +
    geom_hline(yintercept = seq(0, 12, by=0.05), linetype='dotted', col = 'darkgrey') + #Add in lines to denote 0.05 threshold 
    geom_point(aes(y=PHOSPHATE, colour=RepID), size=2.5) +
    guides(colour = guide_legend(label.theme = element_text(size=15))) +
    scale_color_identity(guide = "legend", name="Rep ID") +
    coord_flip() +
    scale_x_reverse() +
    theme_bw()+
    ylab("PHOSPHATE (µg/l)")+
    xlab("Pressure (dbar)") +
    theme(axis.text.y=element_text(size=12)) +
    theme(axis.text.x=element_text(size=12)) +
    theme(axis.title.y=element_text(size=15)) +
    theme(axis.title.x=element_text(size=15)) +
    scale_y_continuous(limits = c(0, 12)) +
    
    facet_wrap(~event, scales="free_y")+
    theme(strip.text.x = element_text(size=17, face="bold", vjust=1))+
    theme(strip.text.y = element_text(size=17, face="bold", vjust=1))+
    theme(legend.position="top") 
  #To make filename
  eventout <- ifelse(Events[i] < 10, paste0('00', Events[i]),
                     ifelse(Events[i]<100, paste0('0', Events[i]),
                            Events[i]))
  
  png(paste(out_dir, paste0("MISSIONID_PHOSPHATE_Event_", eventout, '.png'), sep='/'), width = 10, height = 8, units = 'in', res = 250)
  print(plot)
  
  dev.off()
  
}

# SILICATE

out_dir = "path/Nutrients_By_Event/SILICATE"
Events = unique(jointNuts$event)


for (i in seq(1, length(Events), 1)){
  
  Nuts_subset <- (jointNuts[jointNuts$event %in% Events[i],])
  
  plot <- ggplot(Nuts_subset, aes(x=PrDM)) +
    geom_hline(yintercept = seq(0, 50, by=1), linetype='dotted', col = 'darkgrey') + #Add in lines to denote 0.05 threshold 
    geom_point(aes(y=SILICATE, colour=RepID), size=2.5) +
    guides(colour = guide_legend(label.theme = element_text(size=15))) +
    scale_color_identity(guide = "legend", name="Rep ID") +
    coord_flip() +
    scale_x_reverse() +
    theme_bw()+
    ylab("SILICATE (µg/l)")+
    xlab("Pressure (dbar)") +
    theme(axis.text.y=element_text(size=12)) +
    theme(axis.text.x=element_text(size=12)) +
    theme(axis.title.y=element_text(size=15)) +
    theme(axis.title.x=element_text(size=15)) +
    scale_y_continuous(limits = c(0, 50)) + #examine data and select max limit
    
    facet_wrap(~event, scales="free_y")+
    theme(strip.text.x = element_text(size=17, face="bold", vjust=1))+
    theme(strip.text.y = element_text(size=17, face="bold", vjust=1))+
    theme(legend.position="top") 
  #To make filename
  eventout <- ifelse(Events[i] < 10, paste0('00', Events[i]),
                     ifelse(Events[i]<100, paste0('0', Events[i]),
                            Events[i]))
  
  png(paste(out_dir, paste0("MISSIONID_SILICATE_Event_", eventout, '.png'), sep='/'), width = 10, height = 8, units = 'in', res = 250)
  print(plot)
  
  dev.off()
  
}


# NITRITE

out_dir = "path/Nutrients_By_Event/NITRITE"
Events = unique(jointNuts$event)


for (i in seq(1, length(Events), 1)){
  
  Nuts_subset <- (jointNuts[jointNuts$event %in% Events[i],])
  
  plot <- ggplot(Nuts_subset, aes(x=PrDM)) +
    geom_hline(yintercept = seq(0, 0.8, by=0.05), linetype='dotted', col = 'darkgrey') + #Add in lines to denote 0.05 threshold 
    geom_point(aes(y=NITRITE, colour=RepID), size=2.5) +
    guides(colour = guide_legend(label.theme = element_text(size=15))) +
    scale_color_identity(guide = "legend", name="Rep ID") +
    coord_flip() +
    scale_x_reverse() +
    theme_bw()+
    ylab("NITRITE (µg/l)")+
    xlab("Pressure (dbar)") +
    theme(axis.text.y=element_text(size=12)) +
    theme(axis.text.x=element_text(size=12)) +
    theme(axis.title.y=element_text(size=15)) +
    theme(axis.title.x=element_text(size=15)) +
    scale_y_continuous(limits = c(0, 0.8)) + #examine data and select max limit
    
    facet_wrap(~event, scales="free_y")+
    theme(strip.text.x = element_text(size=17, face="bold", vjust=1))+
    theme(strip.text.y = element_text(size=17, face="bold", vjust=1))+
    theme(legend.position="top") 
  #To make filename
  eventout <- ifelse(Events[i] < 10, paste0('00', Events[i]),
                     ifelse(Events[i]<100, paste0('0', Events[i]),
                            Events[i]))
  
  png(paste(out_dir, paste0("MISSIONID_NITRITE_Event_", eventout, '.png'), sep='/'), width = 10, height = 8, units = 'in', res = 250)
  print(plot)
  
  dev.off()
  
}


# AMMONIUM

out_dir = "path/Nutrients_By_Event/AMMONIUM"
Events = unique(jointNuts$event)


for (i in seq(1, length(Events), 1)){
  
  Nuts_subset <- (jointNuts[jointNuts$event %in% Events[i],])
  
  plot <- ggplot(Nuts_subset, aes(x=PrDM)) +
    geom_hline(yintercept = seq(0, 17, by=0.1), linetype='dotted', col = 'darkgrey') + #Add in lines to denote 0.05 threshold 
    geom_point(aes(y=AMMONIUM, colour=RepID), size=2.5) +
    guides(colour = guide_legend(label.theme = element_text(size=15))) +
    scale_color_identity(guide = "legend", name="Rep ID") +
    coord_flip() +
    scale_x_reverse() +
    theme_bw()+
    ylab("AMMONIUM (µg/l)")+
    xlab("Pressure (dbar)") +
    theme(axis.text.y=element_text(size=12)) +
    theme(axis.text.x=element_text(size=12)) +
    theme(axis.title.y=element_text(size=15)) +
    theme(axis.title.x=element_text(size=15)) +
    scale_y_continuous(limits = c(0, 17)) + #examine data and select max limit
    
    facet_wrap(~event, scales="free_y")+
    theme(strip.text.x = element_text(size=17, face="bold", vjust=1))+
    theme(strip.text.y = element_text(size=17, face="bold", vjust=1))+
    theme(legend.position="top") 
  #To make filename
  eventout <- ifelse(Events[i] < 10, paste0('00', Events[i]),
                     ifelse(Events[i]<100, paste0('0', Events[i]),
                            Events[i]))
  
  png(paste(out_dir, paste0("MISSIONID_AMMONIUM_Event_", eventout, '.png'), sep='/'), width = 10, height = 8, units = 'in', res = 250)
  print(plot)
  
  dev.off()
  
}



#To view subsets:

subset43 <- subset(jointNuts, jointNuts$event == "43")
View(subset43)



# Step 8: Convert Final Lab Flags to BioChem Flags ------------------------------------

#Read in modified laboratory spreadsheet after replicates are reviewed:

Nutsfinal <- read_excel("Nutrients_FILENAME.xlsx", skip=4, sheet=1, col_types='text')[, 1:14]

Nutsfinal$NITRATE <- as.numeric(Nutsfinal$NITRATE)
Nutsfinal$NITRITE <- as.numeric(Nutsfinal$NITRITE)
Nutsfinal$PHOSPHATE <- as.numeric(Nutsfinal$PHOSPHATE)
Nutsfinal$SILICATE <- as.numeric(Nutsfinal$SILICATE)
Nutsfinal$AMMONIUM <- as.numeric(Nutsfinal$AMMONIUM)



##Fill in Sample ID:
NutsF2 <- Nutsfinal  %>% 
  mutate(SAMPLE_ID = na_if(SAMPLE_ID, "")) %>%
  fill(SAMPLE_ID)
head(NutsF2)


###Add repeating Rep ID field after SAMPLE_ID field:
NutsF2 <- NutsF2 %>%
  add_column(RepID = (NutsF2$RepID <- sequence(rle(as.character(NutsF2$SAMPLE_ID))$lengths)), .after="SAMPLE_ID")


####Add empty QC columns. Must be after the nutrient data column, to facilitate upload:
NutsF3 <- NutsF2 %>%
  add_column(NITRATE_QC = "", .after="NITRATE") %>%
  add_column(NITRITE_QC = "", .after="NITRITE") %>%
  add_column(PHOSPHATE_QC = "", .after="PHOSPHATE") %>%
  add_column(SILICATE_QC = "", .after="SILICATE") %>%
  add_column(AMMONIUM_QC = "", .after="AMMONIUM")
head(NutsF3)

Nutsdf<- as.data.frame(NutsF3, na.rm=TRUE)

#####Convert NA entries to blanks
Nutsdf[is.na(Nutsdf)] <- "" 

######Populate QC flag columns using ifelse statements:
Nutsdf$NITRATE_QC <- ifelse(Nutsdf$NITRATE_FLAG == 'A'|Nutsdf$NITRATE_FLAG == 'B', '3',
                            ifelse(Nutsdf$NITRATE_FLAG == 'C'|Nutsdf$NITRATE_FLAG == 'G', '4',
                              ifelse(Nutsdf$NITRATE_FLAG == 'D'|Nutsdf$NITRATE_FLAG == 'E', '6',
                                   ifelse(Nutsdf$NITRATE_FLAG == 'F', '9',
                                          ifelse(grepl("^CH", Nutsdf$SAMPLE_ID), '0', '0')))))  

Nutsdf$NITRITE_QC <- ifelse(Nutsdf$NITRITE_FLAG == 'A'|Nutsdf$NITRITE_FLAG == 'B', '3',
                            ifelse(Nutsdf$NITRITE_FLAG == 'C'|Nutsdf$NITRITE_FLAG == 'G', '4',
                                   ifelse(Nutsdf$NITRITE_FLAG == 'D'|Nutsdf$NITRITE_FLAG == 'E', '6',
                                          ifelse(Nutsdf$NITRITE_FLAG == 'F', '9',
                                                 ifelse(grepl("^CH", Nutsdf$SAMPLE_ID), '0', '0')))))

Nutsdf$PHOSPHATE_QC <- ifelse(Nutsdf$PHOSPHATE_FLAG == 'A'|Nutsdf$PHOSPHATE_FLAG == 'B', '3',
                              ifelse(Nutsdf$PHOSPHATE_FLAG == 'C'|Nutsdf$PHOSPHATE_FLAG == 'G', '4',
                                     ifelse(Nutsdf$PHOSPHATE_FLAG == 'D'|Nutsdf$PHOSPHATE_FLAG == 'E', '6',
                                            ifelse(Nutsdf$PHOSPHATE_FLAG == 'F', '9',
                                                   ifelse(grepl("^CH", Nutsdf$SAMPLE_ID), '0', '0')))))

Nutsdf$SILICATE_QC <- ifelse(Nutsdf$SILICATE_FLAG == 'A'|Nutsdf$SILICATE_FLAG == 'B', '3',
                             ifelse(Nutsdf$SILICATE_FLAG == 'C'|Nutsdf$SILICATE_FLAG == 'G', '4',
                                    ifelse(Nutsdf$SILICATE_FLAG == 'D'|Nutsdf$SILICATE_FLAG == 'E', '6',
                                           ifelse(Nutsdf$SILICATE_FLAG == 'F', '9',
                                                  ifelse(grepl("^CH", Nutsdf$SAMPLE_ID), '0', '0')))))

Nutsdf$AMMONIUM_QC <- ifelse(Nutsdf$AMMONIUM_FLAG == 'A'|Nutsdf$AMMONIUM_FLAG == 'B', '3',
                             ifelse(Nutsdf$AMMONIUM_FLAG == 'C'|Nutsdf$AMMONIUM_FLAG == 'G', '4',
                                    ifelse(Nutsdf$AMMONIUM_FLAG == 'D'|Nutsdf$AMMONIUM_FLAG == 'E', '6',
                                           ifelse(Nutsdf$AMMONIUM_FLAG == 'F', '9',
                                                  ifelse(grepl("^CH", Nutsdf$SAMPLE_ID), '0', '0')))))
#Legend to ifelse statements:

# BIO Lab Flag A and B = BioChem QC flag 3
# BIO Lab Flag C and G = BioChem QC flag 4
# BIO Lab Flag D and E = BioChem QC flag 6
# BIO Lab Flag F = BioChem QC flag 9
# Any instance of reference material (CH-xxxx), and all blank cells = 0. Even those values that appear correct
# Must be assigned a BioChem Flag of 0. This will help those applying the IML QC script know whether the data
# Have or have not been QC'd yet.

head(Nutsdf)

write.csv(Nutsdf, file="Nuts_MISSIONID_FinalLabQC.csv", row.names=FALSE)


