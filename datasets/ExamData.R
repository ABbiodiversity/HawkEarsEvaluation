library(wildRtrax)
library(tidyverse)
library(readxl)

#PREAMBLE############

#1. login----
config <- "script/login.R"
source(config)
wt_auth()

#2. Get project list----
project.list <- wt_get_download_summary(sensor_id = 'ARU')

#3. Set root file path---
root <- "G:/Shared drives/ABMI_Recognizers/HawkEars"

#4. Get dataframe with AWS urls in it----
#This file is obtained with the getReports/DownloadClipView.R script in the recognizersandbox repo.

load("G:/My Drive/ABMI/Projects/Recognizers/Data/clip_report_2023-04-12.Rdata")

#EXAMS PROJECTS###############

#1. Get exams projects----
exam.list <- project.list %>%
  dplyr::filter(str_sub(project, 1, 5)=="Exams")

#2. Download all the data----
dat.list <- list()
for(i in 1:nrow(exam.list)){
  
  dat.list[[i]] <- wt_download_report(project_id = exam.list$project_id[i], sensor_id = "ARU", weather_cols = F, report = "summary")
  
  print(paste0("Finished dataset ", exam.list$project[i], " : ", i, " of ", nrow(exam.list), " projects"))
  
}

dat <- do.call(rbind, dat.list)
#rec <- do.call(rbind, rec.list)

#3. Remove non-avian species----
dat.bird <- wt_tidy_species(dat, zerofill=FALSE)

#4. Link recording url with data----
dat.rec <- raw %>%
  rename(project_name = project, recording_date = date) %>%
  dplyr::select(project_name, location, recording_date, recording_url) %>%
  unique() %>%
  right_join(dat.bird, multiple="all")

#5. Identify unique tasks & select----
#at least 10 observers
#only SPM
#transcribed status
tasks <- dat.rec %>%
  dplyr::select(location, status, recording_date, method, observer, recording_url) %>%
  unique() %>%
  dplyr::filter(method=="180s 1SPM",
                status=="Transcribed") %>%
  group_by(location, recording_date) %>%
  summarize(tasks=n()) %>%
  ungroup() %>%
  dplyr::filter(tasks >= 10)

#6. Filter data and summarize to # of tags per minute per recording----
sum <- dat.rec %>%
  dplyr::filter(method=="180s 1SPM",
                status=="Transcribed") %>%
  inner_join(tasks) %>%
  mutate(minute = ceiling(tag_start_s/60),
         minute = ifelse(minute==0, 1, minute)) %>%
  dplyr::select(recording_url, location, minute, observer, species_code) %>%
  unique() %>%
  group_by(recording_url, location, minute, species_code) %>%
  summarize(tags = n()) %>%
  ungroup() %>%
  left_join(tasks) %>%
  mutate(percent = tags/tasks)

#7. Filter to threshold of % of tags and make wide----
thresh <- 1
use <- sum %>%
  dplyr::filter(tags > thresh) %>%
  dplyr::select(recording_url, location, recording_date, minute, tasks, species_code, percent) %>%
  pivot_wider(names_from="species_code", values_from="percent", names_sort=TRUE)

#8. Save out----
write.csv(use, file.path(root, "Data", "Evaluation", "ExamData.csv"), row.names=FALSE)
