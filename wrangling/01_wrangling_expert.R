#This script wrangles raw hawkears and birdnet detections for the expert dataset.

#PREAMBLE############

#1. Load libraries----
library(tidyverse)
library(fuzzyjoin)

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

#PERCH##########

files.pr <- data.frame(path = list.files(file.path(root, "Results", "ExpertData", "Perch", "tags"), full.names = TRUE),
                       file = list.files(file.path(root, "Results", "ExpertData", "Perch", "tags"))) %>% 
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
  rbind(do.call(rbind, list.pr)) |> 
  dplyr::filter(start <= 180)

write.csv(raw, file.path(root, "Results", "ExpertData", "ExpertData_HawkEarsBirdNETPerch_raw.csv"), row.names = FALSE)

raw <- read.csv(file.path(root, "Results", "ExpertData", "ExpertData_HawkEarsBirdNETPerch_raw.csv"))

#3. Wrangle evaluation data to intervals----
dat <- eval %>%
  dplyr::select(recording_id, minute, observer_id, ALFL:YRWA) %>% 
  pivot_longer(ALFL:YRWA, values_to="count", names_to="species") %>% 
  dplyr::filter(count>0) %>% 
  rbind(extra) %>% 
  mutate(start = 60*(minute-1),
         end = 60*minute)

#4. Get list of HawkEars species----
spp <- raw |> 
  dplyr::filter(classifier=="HawkEars") |> 
  dplyr::select(species) |> 
  unique()

#4. List of species and recordings----
loop <- dplyr::select(raw, recording_id, species) %>% 
  unique() %>% 
  rbind(dplyr::select(dat, recording_id, species) %>% 
          unique()) %>% 
  unique() %>% 
  dplyr::filter(species %in% spp$species) |> 
  arrange(species, recording_id)

#THERE'S GOTTA BE A FASTER WAY OF DOING THIS

#5. Set up loop----
out.list <- list()
for(i in 1:nrow(loop)){
  
  #6. Interval join evaluation and raw data----
  out.list[[i]] <- dplyr::filter(dat, recording_id==loop$recording_id[i], species==loop$species[i]) %>%
    dplyr::select(-recording_id, -species) %>% 
    interval_full_join(dplyr::filter(raw, recording_id==loop$recording_id[i], species==loop$species[i]) %>% 
                         dplyr::select(-recording_id, -species)) %>% 
    mutate(recording_id = loop$recording_id[i],
           species = loop$species[i])

print(paste0("Finished ", i, " of ", nrow(loop)))
  
}

out <- do.call(rbind, out.list)

write.csv(out, file.path(root, "Results", "ExpertData", "ExpertData_IntervalJoin.csv"), row.names = FALSE)

#7. Summarize to minute----
out <- read.csv(file.path(root, "Results", "ExpertData", "ExpertData_IntervalJoin.csv"))

min <- out |> 
  mutate(minute = ifelse(is.na(minute), ceiling(start.y/60), minute),
         minute = ifelse(minute==0, 1, minute)) |> 
  group_by(classifier, species, recording_id, minute) |> 
  summarize(score = max(score, na.rm=TRUE),
            count = sum(count, na.rm=TRUE)) |> 
  ungroup() |> 
  pivot_wider(names_from=classifier, values_from=score) |> 
  dplyr::select(-'NA') |> 
  dplyr::filter(species %in% spp$species)

write.csv(min, file.path(root, "Results", "ExpertData", "ExpertData_ByMinute.csv"))
