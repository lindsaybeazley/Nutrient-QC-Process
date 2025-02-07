# Nutrient-QC-Process
This repository holds the R script used to apply quality control procedures to the bottle nutrient data collected by the DFO Maritimes Region AZMP and AZOMP programs. 

The main features of this process include:

1) Evaluating differences between replicate samples against a maximum difference threshold for each nutrient, and flagging the data according to the laboratory flagging scheme outlined in the 'Nutrient_LaboratoryFlaggingScheme_Feb2025.xlsx' file
2) Extracting BIO's QAT data (the CTD data related to each CTD bottle fire) and matching it against the nutrient sample IDs for evaluation of nutrient data in relation to their corresponding environmental attributes
3) Creating plots using base R to evaluate patterns by depth, temperature, and salinity
4) Using ggplot2 to evaluate patterns in nutrients by event and sample ID 
5) Using ggplot2 to produce plots of nutrients by depth profile to evaluate their vertical structure and differences between replicates
6) Using ifelse statements to match the character laboratory flags to their corresponding BioChem numerical flags

This code has been applied consistently to each AZMP dataset (fixed station, ecosystem trawl survey, and spring/fall survey) starting in 2023, and was also applied to certain missions conducted in 2022. 

The laboratory flagging scheme that illustrates the different laboratory flags applied after sample analysis with the AA3 Autoanalyzer, and their corresponding BioChem numerical flags, can be found in the 'Nutrient_LaboratoryFlaggingScheme_Feb2025.xlsx' and 'Nutrient Lab Template_Revised_Feb2025.xlsx' files included in this repository.

In February 2025, this repository was updated to 1) improve the ggplot2 workflows used to generate all exploratory figures; 2) add new columns and populate them with the detection limits for each nutrient and sequence; 3) re-code the laboratory flag D to a BioChem flag of 0 and eliminate BioChem flag 6; and 4) recode all BioChem flag 1s to 0s, which will then be updated to 1s after the IML QC procedure is applied during the second QC process.
