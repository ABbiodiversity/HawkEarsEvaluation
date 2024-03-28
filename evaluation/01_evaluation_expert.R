#This script evaluates raw hawkears and birdnet detections for the expert dataset.

#PREAMBLE############

#1. Load libraries----
library(tidyverse)
library(Metrics)
library(PRROC)
library(yardstick)

#2. Set root file path---
root <- "G:/Shared drives/ABMI_Recognizers/HawkEars"

#3. Load data----
min <- read.csv(file.path(root, "Results", "ExpertData", "ExpertData_ByMinute.csv")) %>% 
  mutate(actual = ifelse(is.na(count), 0, 1))

#AUC########

#1. Per minute ROC----
auc(min$actual, min$BirdNET)
auc(min$actual, min$HawkEars)

#2. Per minute PR----
min.bn <- min %>% 
  mutate(truth = factor(ifelse(actual==1, "Present", "Absent"))) %>% 
  rename(Present = BirdNET) %>% 
  mutate(Absent = 1 - Present) %>% 
  mutate(predicted = factor(ifelse(Present > Absent, "Present", "Absent"))) %>% 
  dplyr::select(truth, Present, Absent, predicted, species)

pr_auc(min.bn, truth, Present)
pr_curve(min.bn, truth, Present) %>% autoplot()

min.he <- min %>% 
  mutate(truth = factor(ifelse(actual==1, "Present", "Absent"))) %>% 
  rename(Present = HawkEars) %>% 
  mutate(Absent = 1 - Present) %>% 
  mutate(predicted = factor(ifelse(Present > Absent, "Present", "Absent"))) %>% 
  dplyr::select(truth, Present, Absent, predicted, species)

pr_auc(min.he, truth, Present)
pr_curve(min.he, truth, Present) %>% autoplot()


#3. By species----
min.bn %>% 
  group_by(species) %>% 
  pr_auc(truth, Present) %>% 
  summarize(cmap = mean(.estimate, na.rm=TRUE))

min.he %>% 
  group_by(species) %>% 
  pr_auc(truth, Present) %>% 
  View()
  summarize(cmap = mean(.estimate, na.rm=TRUE))


