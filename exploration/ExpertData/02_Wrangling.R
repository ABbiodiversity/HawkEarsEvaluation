#This script wrangles raw hawkears and birdnet detections for the expert dataset.

#PREAMBLE############

#1. Load libraries----
library(tidyverse)

#2. Set root file path---
root <- "G:/Shared drives/ABMI_Recognizers/HawkEars"

#3. Get evaluation dataset----
eval <- read.csv(file.path(root, "Data", "Evaluation", "ExpertData.csv")) %>% 
  dplyr::filter(!is.na(recording_url)) %>% 
  separate(recording_url, into=c("f1", "f2", "f3", "f4", "recfile"), sep="/") %>% 
  separate(recfile, into=c("recording_id", "filetype")) %>% 
  dplyr::select(-c(f1, f2, f3, f4, filetype)) %>% 
  mutate(recording_id = as.integer(recording_id),
         CAJA = ifelse(CAJA==0, GRAJ, CAJA)) %>% 
  dplyr::select(-GRAJ, -CHIK)

#4. Get extra tags----
extra <- read.csv(file.path(root, "Data", "Evaluation", "ExpertData_extra_tags.csv")) %>% 
  mutate(count = 1,
         observer_id = 99) %>% 
  rename(recording_id=recording, species = code)

#5. Get covariate dataset----
covs <- read.csv(file.path(root, "Results", "ExpertData", "ExpertData_RecordingCovariates.csv"))

#HAWKEARS##########

#1. Get list of raw files----
files.he <- data.frame(path = list.files(file.path(root, "Results", "ExpertData", "HawkEars", "tags"), full.names = TRUE),
                       file = list.files(file.path(root, "Results", "ExpertData", "HawkEars", "tags"))) %>% 
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
files.bn <- data.frame(path = list.files(file.path(root, "Results", "ExpertData", "BirdNET", "tags"), full.names = TRUE),
                       file = list.files(file.path(root, "Results", "ExpertData", "BirdNET", "tags"))) %>% 
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

#1. Raw data----
raw <- do.call(rbind, list.he) %>% 
  rbind(do.call(rbind, list.bn))

write.csv(raw, file.path(root, "Results", "ExpertData", "ExpertData_HawkEarsBirdNET_raw.csv"), row.names = FALSE)

raw <- read.csv(file.path(root, "Results", "ExpertData", "ExpertData_HawkEarsBirdNET_raw.csv"))

#2. Summarize to minute----
min <- raw %>% 
  mutate(minute = ceiling(start/60),
         minute = ifelse(minute==0, 1, minute)) %>% 
  group_by(classifier, recording_id, minute, species) %>% 
  summarize(score = max(score)) %>% 
  ungroup() |> 
  pivot_wider(names_from=classifier, values_from=score) %>% 
  dplyr::filter(minute <=3) %>% 
  mutate(recording_id=as.integer(recording_id))

#3. Put everything together----
dat <- eval %>%
  dplyr::select(recording_id, minute, observer_id, ALFL:YRWA) %>% 
  pivot_longer(ALFL:YRWA, values_to="count", names_to="species") %>% 
  dplyr::filter(count > 0,
                minute <= 3) %>% 
  rbind(extra) |> 
  # left_join(covs %>% 
  #             dplyr::select(recording_url, recording_id, minute), multiple="all") %>% 
  full_join(min) 
  dplyr::select(observer_id, recording_id, minute, species, count, BirdNET, HawkEars)

write.csv(dat, file.path(root, "Results", "ExpertData", "ExpertData_ByMinute.csv"), row.names = FALSE)
