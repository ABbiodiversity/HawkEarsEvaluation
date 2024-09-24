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
#filter to just the species in the original data
extra <- read.csv(file.path(root, "Data", "Evaluation", "ExpertData_extra_tags.csv")) %>% 
  mutate(count = 1,
         observer_id = 99) %>% 
  rename(recording_id=recording, species = code) |> 
  dplyr::filter(species %in% colnames(eval))

#HAWKEARS##########

#1. Get list of raw files----
files.he <- data.frame(path = list.files(file.path(root, "Results", "ExpertData", "HawkEars-2024-09-12-filters", "tags"), full.names = TRUE),
                       file = list.files(file.path(root, "Results", "ExpertData", "HawkEars-2024-09-12-filters", "tags"))) %>% 
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
files.bn <- data.frame(path = list.files(file.path(root, "Results", "ExpertData", "BirdNET-2024-06-07", "tags"), full.names = TRUE),
                       file = list.files(file.path(root, "Results", "ExpertData", "BirdNET-2024-06-07", "tags"))) %>% 
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

#PERCH##########

#1. Get list of raw fiels----
files.pr <- data.frame(path = list.files(file.path(root, "Results", "ExpertData", "Perch-2024-06-07", "tags"), full.names = TRUE),
                       file = list.files(file.path(root, "Results", "ExpertData", "Perch-2024-06-07", "tags"))) %>% 
  separate(file, into=c("recording_id", "classifier", "results", "filetype")) %>% 
  rowwise() %>% 
  mutate(filesize = file.size(path)) %>% 
  ungroup() %>% 
  dplyr::filter(filesize > 0) %>% 
  dplyr::select(path, recording_id) 

#2. Read them in----
list.pr <- list()
for(i in 1:nrow(files.pr)){
  list.pr[[i]] <- read_table(files.pr$path[i], col_names = c("start", "end", "species_score"), show_col_types=FALSE) %>% 
    separate(species_score, into=c("species", "score"), sep=";") %>% 
    mutate(score = as.numeric(score),
           recording_id = files.pr$recording_id[i],
           classifier = "Perch")
}

#PUT IT TOGETHER#########

#1. Raw data----
#filter to three minutes
raw <- do.call(rbind, list.he) %>% 
  rbind(do.call(rbind, list.bn)) |> 
  rbind(do.call(rbind, list.pr))

write.csv(raw, file.path(root, "Results", "ExpertData", "ExpertData_HawkEarsBirdNETPerch_raw.csv"), row.names = FALSE)

raw <- read.csv(file.path(root, "Results", "ExpertData", "ExpertData_HawkEarsBirdNETPerch_raw.csv"))

#2. Summarize to minute----
round <- raw %>% 
  mutate(startmin = ceiling((start+0.000001)/60),
         startmin = ifelse(startmin==0, 1, startmin),
         endmin = ceiling((end)/60))

#3. Identify the extra minutes----
extras <- round |> 
  dplyr::filter(startmin!=endmin) |> 
  mutate(minute = startmin+1)

#4. Add in the overlap minutes and get max score per minute----
min <- round |> 
  mutate(minute = startmin) |> 
  rbind(extras) |> 
  group_by(classifier, recording_id, minute, species) %>% 
  summarize(score = max(score)) %>% 
  ungroup() |> 
  pivot_wider(names_from=classifier, values_from=score) %>% 
  dplyr::filter(minute <=3) %>% 
  mutate(recording_id=as.integer(recording_id))

#5. Wrangle evaluation data to intervals----
dat <- eval %>%
  dplyr::select(recording_id, minute, observer_id, ALFL:YRWA) %>% 
  pivot_longer(ALFL:YRWA, values_to="count", names_to="species") %>% 
  rbind(extra) |> 
  dplyr::filter(count>0)

table(dat$recording_id, dat$minute)

write.csv(dat, file.path(root, "Data", "Evaluation", "ExpertData_All.csv"), row.names = FALSE)

#6. Get list of evaluation data intervals----
intervals <- dat |> 
  dplyr::select(recording_id, minute) |> 
  unique()
  
#7. Get list of HawkEars species----
spp <- raw |> 
  dplyr::filter(classifier=="HawkEars") |> 
  dplyr::select(species) |> 
  unique()

#8. Put everything together----
out <- full_join(min, dat, multiple="all") |> 
  inner_join(intervals) |> 
  inner_join(spp) |> 
  mutate(detection = ifelse(!is.na(count), 1, 0))

write.csv(out, file.path(root, "Results", "ExpertData", "ExpertData_ByMinute.csv"), row.names = FALSE)
