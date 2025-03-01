#Nutrient Bottle Data Quality Control Procedures for AZMP/AZOMP

#Written by: Lindsay Beazley
#Last Updated: March 1, 2025


# Step 0: Housekeeping ------------------------------------

#Load packages:

library(tidyr)
library(dplyr)
library(readxl)
library(data.table)
library(tidyverse)
library(ggplot2)


# Save this .R file in your mission folder and nutrients subfolder (e.g., DY18402/Nutrients)


#Create a folder where you can store and load the nutrients dataset, and export other datasets:
data_dir <- './data'
if(!dir.exists(data_dir)){
  dir.create(data_dir, recursive = TRUE)
}



# Step 1: Load Nutrient Data ------------------------------------

Nuts <- read_excel(file.path(data_dir, "YOURFILENAME.xlsx"), skip=4, sheet=1, col_types='text')[, 1:13] #Skip the first 4 rows containing the header

#Note: The col_types argument is set to read all columns in as text. This is to allow you to read in those columns that have cells with NAs.

#The [ , 1:13] appended at the end reads in only the first 13 columns. The BIO rating guide and BioChem flag scheme are skipped.



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


##Convert NA entries to blanks:
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


write.csv(Diff, file=paste(data_dir, "Differences_Replicates_MISSION_NAME_GOES_HERE.csv", row.names=F, sep='/'))


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



# Step 4: Load QAT Data to get Environmental Factors ------------------------------------


#Load QAT data using one of the following 3 options, to get depth, temp, etc. and merge it with nutrient data. QAT files are comma delimited = CSV.

#Change your QAT path according to one of the options below:

qat_path <- '//ent.dfo-mpo.ca/ATLShares/Science/BIODataSvc/SRC/2020s/MISSIONNAME/LOCATION' #, where:

# Option 1 is path to individual calibrated or uncalibrated QAT files from mission folder (//ent.dfo-mpo.ca/ATLShares/Science/BIODataSvc/SRC/2020s/2024/CAR2024010/CTD/DATASHOP_PROCESSING/Step_2_Apply_Calibrations/QAT'
# Option 2 is path to collated QAT file produced by BIO Data Shop (e.g., )
# Option 3 is path to IML bottle report in ANDES folder (if individual QAT files are not available) (e.g., ANDES_Reports/IML bottle report CAR-2022-025 (2022-10-01).xlsx)



#Option 1: Directly load individual QAT files (make sure path leads to calibrated QATs, if available):
CombinedQATs <- list.files(path = qat_path,
                           pattern = "*.QAT$", full.names = TRUE) %>% 
  lapply(read_csv, col_types= cols(.default = col_character())) %>% # Store all files in list
  bind_rows                                                                               
CombinedQATs

CombinedQATs <- as.data.frame(CombinedQATs)

CombinedQATs <- CombinedQATs[, -17:-40] #Remove unneeded columns

colnames(CombinedQATs)[6] <- "SAMPLE_ID"



#Option 2: Load collated QAT xlsx or csv file (if CTD data have been post-processed):
CombinedQATs <- read_excel(file.path(qat_path, "CAR2024010_QAT_Corrected.xlsx")) 

CombinedQATs <-as.data.frame(CombinedQATs)

colnames(CombinedQATs)[7] <- "SAMPLE_ID"

#Extract only the needed columns
CombinedQATs <- CombinedQATs[, -17:-39]



#Option 3: Load IML bottle report from ANDES (if sGSL mission and CTD data have not been post-processed):
Bottlerep <- read_excel(file.path(qat_path, "IML bottle report.xlsx"), sheet=1)

Bottlerep_df <- as.data.frame(Bottlerep)

#rename Bottle UID to SAMPLE_ID
names(Bottlerep_df)[1] <- "SAMPLE_ID"


#########

##Merge nutrient and QAT data together:

jointNuts <- merge(Nutsdf, CombinedQATs, by = 'SAMPLE_ID')
nrow(Nutsdf)
nrow(jointNuts) #do they match? 



###Convert from wide to long format to facilitate plotting:

jointNuts2 <- reshape2::melt(jointNuts, id=c("SAMPLE_ID", "event", "RepID", "PrDM", "T090C", "Sal00"), 
                             measure.vars= c("NITRATE", "NITRITE", "PHOSPHATE", "SILICATE", "AMMONIUM"))

jointNuts2 <- jointNuts2 %>% mutate_at(c('PrDM', 'T090C', 'Sal00'), as.numeric)



# Step 5: Plot Data by Environment ----------------------------------------

#First, create the empty folders in your working directory to store the plots.

#Check and see if directory/folder for plots has been created. If not, create one:
plots_enviro_dir <- './Nutrients_By_Environment'
if(!dir.exists(plots_enviro_dir)){
  dir.create(plots_enviro_dir, recursive = TRUE)
}
nutrients <- unique(jointNuts2$variable)
mission='xxxYEARyyy' #where xxx = abbreviated vessel name, and yyy = unique station name or mission ID, e.g., BCD2024666 or CAR2024010


for(i in seq(1, length(nutrients), 1)){
  sub <- (jointNuts2[jointNuts2$variable %in% nutrients[i],])
  for(m in colnames(jointNuts2)[4:6]){ #This loops through the different environmental variables
    plot <- ggplot(data = sub, aes(x = .data[[m]], y = value))+
      geom_point(aes(colour = nutrients[i]), shape=1, size=3, colour="navy")+
      ylab(paste0(nutrients[i]))+
      theme_bw()+
      theme(axis.text.y=element_text(size=10)) +
      theme(axis.text.x=element_text(size=10)) +
      theme(legend.position="none")
    
    png(paste(plots_enviro_dir, paste0(mission, '_', nutrients[i], '_', m, '.png'), sep='/'), width = 16, height = 8, units = 'in', res = 250)
    print(plot)
    
    dev.off()
    
  }     
}



# Step 6: Plot Data Across Time/Year --------

#Check and see if directory/folder for plots has been created. If not, create one:
plots_time_dir <- './Nutrients_By_Time'
if(!dir.exists(plots_time_dir)){
  dir.create(plots_time_dir, recursive = TRUE)
}

for(i in seq(1, length(nutrients), 1)){
  sub <- (jointNuts2[jointNuts2$variable %in% nutrients[i],])
  plot <- ggplot(data = sub, aes(x = event, y = value))+
    geom_point(aes(colour = RepID), size=2)+
    ylab(paste0(nutrients[i]))+
    scale_color_identity(guide = "legend", name="Rep ID") +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    theme(axis.text.y=element_text(size=10)) +
    theme(axis.text.x=element_text(size=10))
  
  png(paste(plots_time_dir, paste0(mission, '_', nutrients[i], '_By_Time', '.png'), sep='/'), width = 16, height = 8, units = 'in', res = 250)
  print(plot)
  
  dev.off()
  
}


#The following code is useful for fixed stations, but not for missions. This plots the mean concentration of each sample by collected
#within each cast/event, per depth

for(i in seq(1, length(nutrients), 1)){
  sub <- (jointNuts2[jointNuts2$variable %in% nutrients[i],])
  plot <- sub %>%
    group_by(SAMPLE_ID) %>%
    mutate(mean_conc = mean(value)) %>%
    ungroup() %>%
    ggplot(aes(x = event, y = PrDM, colour=mean_conc)) +
    geom_point(size=3) +
    scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", 
                                      "red")) +
    scale_y_reverse()+
    ylab("PrDM")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    theme(axis.text.y=element_text(size=10)) +
    theme(axis.text.x=element_text(size=10)) 
  
  png(paste(plots_time_dir, paste0(mission, '_', nutrients[i], '_By_Time_VerticalProfiles', '.png'), sep='/'), width = 16, height = 8, units = 'in', res = 250)
  print(plot)
  
  dev.off()
  
}


# Step 7: Extract Data by Depth Stratum and Plot  --------

#Check and see if directory/folder for plots has been created. If not, create one:
plots_depth_dir <- './Nutrients_By_Depth'
if(!dir.exists(plots_depth_dir)){
  dir.create(plots_depth_dir, recursive = TRUE)
}

nom_depths <- c(0, 5, 10, 20, 30, 40, 50, 75, 100, 140) #These may change depending on the station, mission, etc.
bin <- c(3, 5, 5, 5, 5, 5, 5, 5, 5, 5) #3 was chosen for the surface bin

for(i in seq(1, length(nom_depths), 1)){
  
  depth_l <-(nom_depths[i] - bin[i]) # the data -5 m of nominal depth (lower depth)
  depth_h <-(nom_depths[i] + bin[i]) # the data +5 m of the nominal depth (higher depth)
  
  #subset the data so that -/+ 5 m nominal depth are included. #So for the 10 m slice, data from 5 to <15 m are included in the average.
  if (depth_l < max(jointNuts2$PrDM)) sub1 <- subset(jointNuts2, jointNuts2$PrDM >= depth_l & jointNuts2$PrDM < depth_h) 
  
      for(s in seq(1, length(nutrients), 1)){

         sub2 <- (sub1[sub1$variable %in% nutrients[s],]) 
            plot <- ggplot(data = sub2, aes(x = SAMPLE_ID, y=value, colour=RepID)) +
            geom_point(aes(y=value, colour=RepID), size=3) +
            scale_color_identity(guide = "legend", name="RepID") +
            ylab(paste0(nutrients[s]))+
            theme(axis.text.x=element_text(angle=90)) +
            theme(legend.position = "top")
            #Set a consistent scale for each variable, based on known ranges or nutrient min/max:
            if(nutrients[s] == "NITRATE"){  
              plot <- plot + scale_y_continuous(limits = c(0, 25))
            } else if(nutrients[s] == "PHOSPHATE"){
              plot <- plot + scale_y_continuous(limits = c(0, 2))
            } else if(nutrients[s] == "SILICATE"){
              plot <- plot + scale_y_continuous(limits = c(0, 35)) 
            } else if(nutrients[s] == "NITRITE"){
              plot <- plot + scale_y_continuous(limits = c(0, 1))
            } else if (nutrients[s] == "AMMONIUM"){
              plot <- plot + scale_y_continuous(limits = c(0, 2))
            }
  
            png(paste(plots_depth_dir, paste0(mission,'_',nutrients[s],'_',nom_depths[i],'m', '.png'), sep='/'), width = 16, height = 8, units = 'in', res = 250)
            print(plot)
  
            dev.off()
  
      }
  
}



# Step 8: Plot Nutrient Vertical Profiles by Sampling Event ----------------------

##This code is most useful for shelf-wide data, and helps to a) detect inversion, b) evaluate vertical structure, and
##c) examine divergence between replicates and eliminate data if applicable. The plots and the data are examined outside
##R, and the data modified in the lab spreadsheet if applicable.
  

#Check and see if directory/folder for plots has been created. If not, create one:
plots_event_dir <- './Nutrients_By_Event'
if(!dir.exists(plots_event_dir)){
  dir.create(plots_event_dir, recursive = TRUE)
}

Events = unique(jointNuts2$event)
mission='xxxYEARyyy' #where xxx = abbreviated vessel name, and yyy = unique station name or mission ID, e.g., BCD2024666 or CAR2024010

for (m in seq(1, length(Events), 1)){
  
  Event_subset <- (jointNuts2[jointNuts2$event %in% Events[m],])
  
  for(i in seq(1, length(nutrients), 1)){
    sub <- (Event_subset[Event_subset$variable %in% nutrients[i],])
    
    plot <- ggplot(sub, aes(x=PrDM)) +
      geom_point(aes(y=value, colour=RepID), size=2.5) +
      guides(colour = guide_legend(label.theme = element_text(size=15))) +
      scale_color_identity(guide = "legend", name="Rep ID") +
      coord_flip() +
      scale_x_reverse() +
      theme_bw() +
      ylab(paste0(nutrients[i]))+
      xlab("Pressure (dbar)") +
      theme(axis.text.y=element_text(size=12)) +
      theme(axis.text.x=element_text(size=12)) +
      theme(axis.title.y=element_text(size=15)) +
      theme(axis.title.x=element_text(size=15)) +
      
      facet_wrap(~event, scales="free_y")+
      theme(strip.text.x = element_text(size=17, face="bold", vjust=1))+
      theme(strip.text.y = element_text(size=17, face="bold", vjust=1))+
      theme(legend.position="top")
    
    if(nutrients[i] == "NITRATE"){
      plot <- plot + geom_hline(yintercept = seq(0, 25, by=0.5), linetype='dotted', col = 'darkgrey') + 
        geom_line(aes(y=T090C/1.0), linewidth=0.5) +
        scale_y_continuous(limits = c(0, 25), sec.axis = sec_axis(~.*1.0, name = "Temperature"))
    } else if(nutrients[i] == "PHOSPHATE"){
      plot <- plot + geom_hline(yintercept = seq(0, 2, by=0.05), linetype='dotted', col = 'darkgrey') + 
        geom_line(aes(y=T090C/6.0), linewidth=0.5) +
        scale_y_continuous(limits = c(0, 2), sec.axis = sec_axis(~.*6.0, name = "Temperature")) 
    } else if(nutrients[i] == "SILICATE"){
      plot <- plot + geom_hline(yintercept = seq(0, 35, by=1), linetype='dotted', col = 'darkgrey') +
        geom_line(aes(y=T090C/1), linewidth=0.5) +
        scale_y_continuous(limits = c(0, 35), sec.axis = sec_axis(~.*1.0, name = "Temperature")) 
    } else if(nutrients[i] == "NITRITE"){
      plot <- plot + geom_hline(yintercept = seq(0, 1, by=0.05), linetype='dotted', col = 'darkgrey') +
        geom_line(aes(y=T090C/10.0), linewidth=0.5) +
        scale_y_continuous(limits = c(0, 1), sec.axis = sec_axis(~.*10.0, name = "Temperature")) 
    } else if (nutrients[i] == "AMMONIUM"){
      plot <- plot + geom_hline(yintercept = seq(0, 2, by=0.1), linetype='dotted', col = 'darkgrey') + 
        geom_line(aes(y=T090C/10.0), linewidth=0.5) +
        scale_y_continuous(limits = c(0, 2), sec.axis = sec_axis(~.*10.0, name = "Temperature")) 
    }
    
    png(paste(plots_event_dir, paste0(mission, '_', nutrients[i], '_Event_', Events[m], '.png'), sep='/'), width = 10, height = 8, units = 'in', res = 250)
    print(plot)
    
    dev.off()
    
  }
}


#To subset and view data for individual events:

subset43 <- subset(jointNuts, jointNuts$event == "43")
View(subset43)


### Make a copy of Peter Thamer's original laboratory spreadsheet, and called it "Nutrients_FILENAME_FLAGGED.xlsx", and add your flags directly to this file based on your evaluation of the plots from the above exercises. 



# Step 9: Add Columns with Detection Limits per Sequence and BioChem flags ------------------------------------

#Read in modified laboratory spreadsheet after replicates are reviewed and flagged:

Nutsfinal <- read_excel(file.path(data_dir, "Nutrients_FILENAME_FLAGGED.xlsx"), skip=4, sheet=1, col_types='text')[, 1:14]

##Fill in Sample ID:
Nutsfinal2 <- Nutsfinal  %>% 
  mutate(SAMPLE_ID = na_if(SAMPLE_ID, "")) %>%
  fill(SAMPLE_ID)
head(Nutsfinal2)


###Add repeating Rep ID field after SAMPLE_ID field:

Nutsfinal3 <- Nutsfinal2 %>%
  add_column(RepID = (Nutsfinal2$RepID <- sequence(rle(as.character(Nutsfinal2$SAMPLE_ID))$lengths)), .after="SAMPLE_ID")


#Add in Detection Limits: 

DL <- read_excel(file.path(data_dir, "Nutrients_FILENAME_FLAGGED.xlsx"), skip=7, sheet=2, cell_rows(8:11))[, 9:14]

colnames(DL)[1] <- "NITRATE_DL"
colnames(DL)[2] <- "NITRITE_DL"
colnames(DL)[3] <- "PHOSPHATE_DL"
colnames(DL)[4] <- "SILICATE_DL"
colnames(DL)[5] <- "AMMONIUM_DL"
colnames(DL)[6] <- "SEQUENCE"


#Populate empty DL columns with DL values per Sequence:

Nutsfinal4 <- Nutsfinal3 %>%
  merge(DL, by = "SEQUENCE") %>%
  transmute(SAMPLE_ID, RepID, SAMPLE_STATE, NITRATE, NITRATE_FLAG, NITRATE_DL = coalesce(as.character(NITRATE_DL)),
            NITRITE, NITRITE_FLAG, NITRITE_DL = coalesce(as.character(NITRITE_DL)), PHOSPHATE, PHOSPHATE_FLAG, 
            PHOSPHATE_DL = coalesce(as.character(PHOSPHATE_DL)), SILICATE, SILICATE_FLAG, SILICATE_DL = coalesce(as.character(SILICATE_DL)),
            AMMONIUM, AMMONIUM_FLAG, AMMONIUM_DL = coalesce(as.character(AMMONIUM_DL)), SEQUENCE, COMMENT)


#This is an IMPORTANT QC check: make sure that records from Nutsfinal3 were not lost when joining the detection limits. If they were, the Sequence information is likely missing from the QC tab:
nrow(Nutsfinal3)
nrow(Nutsfinal4) #Do they match?


#Convert NAs to Blank Cells for a cleaner dataframe:

Nutsfinal5 <- Nutsfinal4 %>%
  mutate(NITRATE_FLAG = ifelse(is.na(NITRATE_FLAG), "", NITRATE_FLAG),
         NITRITE_FLAG = ifelse(is.na(NITRITE_FLAG), "", NITRITE_FLAG),
         PHOSPHATE_FLAG = ifelse(is.na(PHOSPHATE_FLAG), "", PHOSPHATE_FLAG),
         SILICATE_FLAG = ifelse(is.na(SILICATE_FLAG), "", SILICATE_FLAG),
         AMMONIUM_FLAG = ifelse(is.na(AMMONIUM_FLAG), "", AMMONIUM_FLAG),
         COMMENT = ifelse(is.na(COMMENT), "", COMMENT))


####Add empty QC columns. Must be after the nutrient data column, to facilitate upload:

Nutsfinal6 <- Nutsfinal5 %>%
  add_column(NITRATE_QC = "", .after="NITRATE") %>%
  add_column(NITRITE_QC = "", .after="NITRITE") %>%
  add_column(PHOSPHATE_QC = "", .after="PHOSPHATE") %>%
  add_column(SILICATE_QC = "", .after="SILICATE") %>%
  add_column(AMMONIUM_QC = "", .after="AMMONIUM")
head(Nutsfinal6)

Nutsdf <- as.data.frame(Nutsfinal6, na.rm=TRUE)



######Populate QC flag columns using ifelse statements:
Nutsdf$NITRATE_QC <- ifelse(Nutsdf$NITRATE_FLAG == 'A'|Nutsdf$NITRATE_FLAG == 'B', '3',
                            ifelse(Nutsdf$NITRATE_FLAG == 'C'|Nutsdf$NITRATE_FLAG == 'G', '4',
                              ifelse(Nutsdf$NITRATE_FLAG == 'D'|Nutsdf$NITRATE_FLAG == 'E', '0',
                                   ifelse(Nutsdf$NITRATE_FLAG == 'F', '9',
                                          ifelse(grepl("^CH", Nutsdf$SAMPLE_ID), '0', '0')))))  

Nutsdf$NITRITE_QC <- ifelse(Nutsdf$NITRITE_FLAG == 'A'|Nutsdf$NITRITE_FLAG == 'B', '3',
                            ifelse(Nutsdf$NITRITE_FLAG == 'C'|Nutsdf$NITRITE_FLAG == 'G', '4',
                                   ifelse(Nutsdf$NITRITE_FLAG == 'D'|Nutsdf$NITRITE_FLAG == 'E', '0',
                                          ifelse(Nutsdf$NITRITE_FLAG == 'F', '9',
                                                 ifelse(grepl("^CH", Nutsdf$SAMPLE_ID), '0', '0')))))

Nutsdf$PHOSPHATE_QC <- ifelse(Nutsdf$PHOSPHATE_FLAG == 'A'|Nutsdf$PHOSPHATE_FLAG == 'B', '3',
                              ifelse(Nutsdf$PHOSPHATE_FLAG == 'C'|Nutsdf$PHOSPHATE_FLAG == 'G', '4',
                                     ifelse(Nutsdf$PHOSPHATE_FLAG == 'D'|Nutsdf$PHOSPHATE_FLAG == 'E', '0',
                                            ifelse(Nutsdf$PHOSPHATE_FLAG == 'F', '9',
                                                   ifelse(grepl("^CH", Nutsdf$SAMPLE_ID), '0', '0')))))

Nutsdf$SILICATE_QC <- ifelse(Nutsdf$SILICATE_FLAG == 'A'|Nutsdf$SILICATE_FLAG == 'B', '3',
                             ifelse(Nutsdf$SILICATE_FLAG == 'C'|Nutsdf$SILICATE_FLAG == 'G', '4',
                                    ifelse(Nutsdf$SILICATE_FLAG == 'D'|Nutsdf$SILICATE_FLAG == 'E', '0',
                                           ifelse(Nutsdf$SILICATE_FLAG == 'F', '9',
                                                  ifelse(grepl("^CH", Nutsdf$SAMPLE_ID), '0', '0')))))

Nutsdf$AMMONIUM_QC <- ifelse(Nutsdf$AMMONIUM_FLAG == 'A'|Nutsdf$AMMONIUM_FLAG == 'B', '3',
                             ifelse(Nutsdf$AMMONIUM_FLAG == 'C'|Nutsdf$AMMONIUM_FLAG == 'G', '4',
                                    ifelse(Nutsdf$AMMONIUM_FLAG == 'D'|Nutsdf$AMMONIUM_FLAG == 'E', '0',
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
str(Nutsdf)

Nutsdf$NITRATE <- as.numeric(Nutsdf$NITRATE)
Nutsdf$NITRITE <- as.numeric(Nutsdf$NITRITE)
Nutsdf$PHOSPHATE <- as.numeric(Nutsdf$PHOSPHATE)
Nutsdf$SILICATE <- as.numeric(Nutsdf$SILICATE)
Nutsdf$AMMONIUM <- as.numeric(Nutsdf$AMMONIUM)


#Convert negative DL values to zero:

Nutsdf[Nutsdf < 0] <- 0


#Export your final QC'd dataset:

write.csv(Nutsdf, file=paste(data_dir, "Nuts_MISSIONID_FinalLabQC.csv", row.names=FALSE, sep='/'))


