#This script wrangles raw hawkears and birdnet detections for the expert dataset.

#PREAMBLE############

#1. Load libraries----
library(tidyverse)

#2. Set root file path---
root <- "G:/Shared drives/ABMI_Recognizers/HawkEars"

#3. Get evaluation dataset----
eval <- read.csv(file.path(root, "Data", "Evaluation", "ExpertData.csv")) %>% 
  dplyr::filter(!is.na(recording_url))

#4. Get covariate dataset----
covs <- read.csv(file.path(root, "Results", "ExpertData", "ExpertData_RecordingCovariates.csv"))

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
                       file = list.files(file.path(root, "Results", "ExpertData", "BirdNET"))) %>% 
  separate(file, into=c("recording_id", "classifier", "results", "filetype")) %>% 
  rowwise() %>% 
  mutate(filesize = file.size(path)) %>% 
  ungroup() %>% 
  dplyr::filter(filesize > 0) %>% 
  dplyr::select(path, recording_id) 

#2. Read them in----
list.bn <- list()
for(i in 1:nrow(files.bn)){
  list.bn[[i]] <- read_table(files.bn$path[i], col_names = c("start", "end", "species_score"), show_col_types=FALSE) %>% 
    separate(species_score, into=c("species", "score"), sep=";") %>% 
    mutate(score = as.numeric(score),
           recording_id = files.bn$recording_id[i],
           classifier = "BirdNET")
}

#PUT IT TOGETHER#########

#TO DO: FILTER OUT EXTRANEOUS TASKS FOR RECORDINGS PROCESSED TWICE####

#1. Raw data----
raw <- do.call(rbind, list.he) %>% 
  rbind(do.call(rbind, list.bn))

write.csv(raw, file.path(root, "Results", "ExpertData", "ExpertData_HawkEarsBirdNET_raw.csv"), row.names = FALSE)

#2. Summarize to minute----
min <- raw %>% 
  mutate(minute = ceiling(start/60),
         minute = ifelse(minute==0, 1, minute)) %>% 
  group_by(classifier, recording_id, minute, species) %>% 
  summarize(score = max(score)) %>% 
  pivot_wider(names_from=classifier, values_from=score, values_fill = 0)

#3. Put together with the expert data----
dat <- eval %>% 
  dplyr::select(recording_url, minute, tasks, observer_id, ALFL:YRWA) %>% 
  pivot_longer(ALFL:YRWA, values_to="count", names_to="species") %>% 
  dplyr::filter(count>0) %>% 
  left_join(covs %>% 
              dplyr::select(recording_id, tasks, minute), multiple="all") %>% 
  mutate(expert = 0) %>% 
  full_join(min %>% 
              mutate(recording_id = as.integer(recording_id)))

#4. Randomly sample one task per recording----
set.seed(123)
use <- dat %>% 
  dplyr::select(recording_id, observer_id) %>% 
  unique() %>% 
  group_by(recording_id) %>% 
  summarize(n=n()) %>% 
  ungroup()
  
  group_by(recording_id) %>% 
  sample_n(1) %>% 
  ungroup() %>% 
  left_join(dat, multiple="all")
  
write.csv(use, file.path(root, "Results", "ExpertData", "ExpertData_ByMinute.csv"), row.names = FALSE)
