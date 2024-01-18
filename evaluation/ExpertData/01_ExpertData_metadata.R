#This script looks at HawkEars performance on the expert dataset.

#PREAMBLE############

#1. Load libraries----
library(tidyverse)
library(tuneR)
library(hardRain)

#2. Set root file path---
root <- "G:/Shared drives/ABMI_Recognizers/HawkEars"

#3. Get evaluation dataset----
eval <- read.csv(file.path(root, "Data", "Evaluation", "ExpertData.csv")) %>% 
  dplyr::filter(!is.na(recording_url))