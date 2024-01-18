#This script wrangles raw hawkears and birdnet detections for the expert dataset.

#PREAMBLE############

#1. Load libraries----
library(tidyverse)

#2. Set root file path---
root <- "G:/Shared drives/ABMI_Recognizers/HawkEars"

#3. Get evaluation dataset----
eval <- read.csv(file.path(root, "Data", "Evaluation", "ExpertData.csv")) %>% 
  dplyr::filter(!is.na(recording_url))

#HAWKEARS##########

#1. Get list of raw files----
files.he <- data.frame(path = list.files(file.path(root, "Results", "ExpertData", "HawkEars"), full.names = TRUE),
                       file = list.files(file.path(root, "Results", "ExpertData", "HawkEars"))) %>% 
  separate(file, into=c("recording_id", "classifier", "filetype")) %>% 
  dplyr::select(path, recording_id)

#2. Read them in----
list.he <- list()
for(i in 1:nrow(files.he)){
  list.he[[i]] <- read_table(files.he$path[i], col_names = c("start", "end", "species_score"), show_col_types=FALSE)  %>% 
    separate(species_score, into=c("species", "score"), sep=";") %>% 
    mutate(score = as.numeric(score),
           recording_id = files.he$recording_id[i],
           classifier = "HawkEars")
}

#BIRDNET#########

#1. Get list of raw files----
files.bn <- data.frame(path = list.files(file.path(root, "Results", "ExpertData", "BirdNET"), full.names = TRUE),
                       file = list.files(file.path(root, "Results", "ExpertData", "BirdNET")),
                       file.size = file.size(file.path(root, "Results", "ExpertData", "BirdNET"))) %>% 
  separate(file, into=c("recording_id", "classifier", "results", "filetype")) %>% 
  dplyr::select(path, recording_id)

#2. Read them in----
list.bn <- list()
for(i in 1:nrow(files.bn)){
  list.bn[[i]] <- read_table(files.bn$path[i], col_names = c("start", "end", "species_score"), show_col_types=FALSE)
    separate(species_score, into=c("species", "score"), sep=";") %>% 
    mutate(score = as.numeric(score),
           recording_id = files.bn$recording_id[i],
           classifier = "BirdNET")
}

#PUT IT TOGETHER#########

#1. Raw data----
raw <- do.call(rbind, list.he) %>% 
  rbind(do.call(rbind, list.bn))

write.csv(raw, file.path(root, "Results", "ExpertData", "ExpertData_HawkEarsBirdNET_raw.csv"), row.names = FALSE)

#2. Summarize to minute----
min <- raw %>% 
  mutate(minute = ceiling(start/60),
         minute = ifelse(minute==0, 1, minute)) %>% 
  dplyr::select(classifier, recording_id, minute, species) %>% 
  unique()

#3. Put together with the expert data----
dat <- 
