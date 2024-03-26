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

#4. Get summary of results from Jan----
raw <- read.csv(file.path(root, "Results", "species_report_expertdata.csv"))

#DATA PRESENTS########

dat <- raw %>% 
  mutate(recall = true_pos/(true_pos + false_neg),
         precision = true_pos/(true_pos + false_pos),
         fscore = (2*precision*recall)/(precision+recall),
         pos = true_pos + false_pos,
         n = true_pos + false_neg)

ggplot(dat) +
  geom_point(aes(x=recall, y=pos))

ggplot(dat) +
  geom_point(aes(x=precision, y=n))

ggplot(dat) +
  geom_point(aes(x=fscore, y=pos))

#MEASURE RECORDING NOISE####

#1. Get recordings from evaluation dataset----
recs <- eval %>% 
  dplyr::select(recording_url) %>% 
  separate(recording_url, into=c(""))


#1. Get list of recordings----
files <- data.frame(path = list.files(file.path(root, "Data", "Recordings", "ExpertData", "mp3"), full.names = TRUE),
                    file = list.files(file.path(root, "Data", "Recordings", "ExpertData", "mp3"))) %>% 
  mutate(recid = str_sub(file, -100, -5))

for(i in 1:length(files)){
  
  mp3.i <- readMP3(files$path[i])
  rain.i <- getMetrics(mp3.i, t.step=60, parallel=TRUE)
  out.i <- rain.i %>% 
    data.frame() %>% 
    mutate(minute = row_number())
  
}

