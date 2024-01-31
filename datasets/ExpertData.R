library(wildRtrax)
library(tidyverse)
library(readxl)

#PREAMBLE############

#1. login----
config <- "WTlogin.R"
source(config)
wt_auth()

#2. Get project list----
project.list <- wt_get_download_summary(sensor_id = 'ARU')

#3. Set root file path---
root <- "G:/Shared drives/ABMI_Recognizers/HawkEars"

#4. Get dataframe with AWS urls in it----
#This file is obtained with the getReports/DownloadClipView.R script in the recognizersandbox repo.

load("G:/My Drive/ABMI/Projects/Recognizers/Data/clip_report_2023-04-12.Rdata")

#EXPERT LISTENERS##############

#1. Import the whole database----
load("G:Shared drives/BAM_NationalModels/NationalModels5.0/Data/WildTrax/wildtrax_raw_2023-01-20.Rdata")

#2. Get listener lookup table----
obs <- read.csv("G:/My Drive/ABMI/Projects/TMTT/data/app_user.csv") %>%
  rename(observer = user_id) %>%
  dplyr::select(observer, user_name)

#3. Filter to just parameters of interest----
raw.aru <- raw.wt %>%
  dplyr::filter(sensorId=="ARU", organization=="BU", status=="Transcribed") %>%
  separate(method, into=c("duration", "tagMethod"), sep=" ", remove=FALSE) %>%
  dplyr::filter(tagMethod=="1SPM") %>%
  mutate(observer = as.numeric(observer)) %>%
  left_join(obs)

#4. Just use recordings for top listeners----
raw.use <- raw.aru %>%
  dplyr::filter(user_name %in% c("Nicole Boucher", "Scott Wilson", "Alex MacPhail"))

#5. Filter to just bird species----
raw.bird <- raw.use %>%
  rename(species_code = speciesCode,
         project_name = project,
         recording_date = date) %>%
  wt_tidy_species(zerofill = FALSE)

#6. Link to recording url----
raw.rec <- raw %>%
  rename(project_name = project, recording_date = date) %>%
  dplyr::select(project_name, location, recording_date, recording_url) %>%
  unique() %>%
  right_join(raw.bird, multiple="all")

#7. Make wide----
raw.wide <- raw.rec %>%
  mutate(minute = ceiling(tag_start_s/60),
         minute = ifelse(minute==0, 1, minute),
         abundance = as.numeric(abundance),
         abundance = ifelse(is.na(abundance), 1, abundance)) %>%
  dplyr::select(recording_url, project_name, location, latitude, longitude, observer_id, duration, recording_date, minute, species_code, abundance) %>%
  pivot_wider(names_from="species_code", values_from="abundance", names_sort=TRUE, values_fn = ~sum(.x), values_fill=0) %>%
  arrange(project_name, location, recording_date, minute) %>%
  mutate(id = row_number()) %>%
  relocate(id, .before = recording_url)

#8. Randomly select n recordings with each species in it----
n <- 10
set.seed(1234)
ids <- c()
for(i in 8:ncol(raw.wide)){
  
  #get the rows already selected
  bird.i <-   raw.wide %>%
    dplyr::filter(id %in% ids)
  
  #figure out how many have the target species in
  n.i <- nrow(bird.i[bird.i[,i]>0,])
  
  #calculate how many more are needed
  n.use <- n - n.i
  
  #if more are needed, id the rows with the target species in it
  if(n.use > 0){
    bird1 <- raw.wide[,c(1,i)]
    bird2 <- bird1[bird1[,2]>0,]
    
    #if there are more available than needed, random selection
    if(nrow(bird2) > n.use){
      ids <- c(ids, sample_n(bird2, n.use)$id)
    }
    
    #otherwise just take them all
    if(nrow(bird2)<=n.use){
      ids <- c(ids, bird2$id)
    }
  }
}

#9. Filter to the selected rows----
expert.use <- raw.wide %>%
  dplyr::filter(id %in% ids) %>%
  dplyr::select(-id)

summary(expert.use)

#10. Save out----
#write.csv(expert.use, file.path(root, "Data", "Evaluation", "ExpertData.csv"), row.names=FALSE)

#LINK EXPERT CORRECTIONS TO TASK ID####

#1. Read expert data back in----
use <- read.csv(file.path(root, "Data", "Evaluation", "ExpertData.csv"))

#2. Read in corrections from HawkEars----
corr <- read.csv(file.path(root, "Results", "ExpertData_tag_updates.csv")) %>%
  rbind(read.csv(file.path(root, "Results", "ExpertData_tag_updates_2024_01_04.csv")))

#3. Read in new database with task id in it----
load("G:/My Drive/ABMI/Projects/BirdModels/Data/WildTrax/wildtrax_raw_2023-12-11.Rdata")

#4. Put it all together----
rec <- use %>%
  group_by(recording_url, location, project_name, observer_id, recording_date) %>%
  summarize(max_minute = max(minute)) %>%
  ungroup() %>%
  separate(recording_url, into=c("f1", "f2", "f3", "f4", "recordingname"), sep="/") %>%
  separate(recordingname, into=c("recording", "filetype")) %>%
  dplyr::select(-f1, -f2, -f3, -f4, -filetype) %>%
  mutate(recording = as.numeric(recording)) %>%
  unique() %>%
  right_join(corr, multiple="all") %>%
  rename(project = project_name,
         recording_date_time = recording_date) %>%
  left_join(aru.wt %>%
              dplyr::select(location, project, observer_id, task_id, recording_date_time, task_duration),
            multiple="all") %>%
  unique()

#5. Check for multiples----
dup <- rec %>%
  group_by(recording, location, recording_date_time, minute, species, review.notes) %>%
  mutate(n=n()) %>%
  ungroup() %>%
  dplyr::filter(n > 1)

#6. Remove duplicates and add the missing ones----
out <- rec %>%
  dplyr::filter(!task_id %in% c(41672, 123467, 120925, 41692)) %>%
  mutate(task_id = ifelse(is.na(task_id), 19916, task_id)) %>%
  dplyr::select(recording, task_id, location, county.name, observer_id, recording_date_time, minute, species, change.type, review.notes, link.to.similar)

write.csv(out, file.path(root, "Results", "ExpertData_tag_updates_taskid.csv"))