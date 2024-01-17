#This script looks at HawkEars performance on the expert dataset.

#PREAMBLE############

#1. Load libraries----
library(tidyverse)

#2. Set root file path---
root <- "G:/Shared drives/ABMI_Recognizers/HawkEars"

#3. Get evaluation dataset----
eval <- read.csv(file.path(root, "Data", "Evaluation", "ExpertData.csv")) %>% 
  dplyr::filter(!is.na(recording_url))

#4. Get results from Jan----
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

