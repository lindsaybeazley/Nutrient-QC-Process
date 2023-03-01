# Nutrient-QC-Process
This repository holds the R script used to apply quality control procedures to the bottle nutrient data collected by the AZMP and AZOMP programs. 

The main features of this process include:

1) Evaluating differences between replicate samples against a maximum difference threshold for each nutrient
2) Extracting BIO's QAT data (the CTD data related to each CTD bottle fire) and matching it against the nutrient sample IDs for evaluation of nutrient data in relation to their corresponding environmental attributes
3) Creating plots using base R to evaluate patterns by depth, temperature, and salinity
4) Using ggplot2 to evaluate patterns in nutrients by event and sample ID 
5) Using ggplot 2 to produce plots of nutrients by depth profile to evaluate vertical structure of nutrients and differences between replicates

This code has been applied consistently to each AZMP dataset (fixed station, ecosystem trawl survey, and spring/fall survey) starting in 2023, and was also applied to certain missions conducted in 2022. 
