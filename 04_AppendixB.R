# ---
# title: "04 species table for Appendix B for manuscript"
# author: "Elly Knight"
# date: "2025-03-18"
# inputs: "evaluation output from `02_evaluate_community.R" and from python evaluation script in HawkEars repo for vocal activity dataset, training data records"
# outputs: "table for manuscript appendix"

# 1. Setup ----

## 1.1 Load libraries----
library(tidyverse)

## 1.2 Get inputs ----
dat <- read.csv("data/community_minute.csv")
train <- read.csv("data/HawkEars-training-record-counts-2024-09.csv")
appendix <- read.csv("results/Evaluation_community_appendix.csv")
species <- read.csv("results/Evaluation_community_species.csv")
activity <- read.csv("results/Evaluation_vocalactivity.csv") |> 
  dplyr::filter(threshold >= 0.1) |> 
  rename(fscore = f1score)

# 2. Determine better classifier based on fscore ----

## 2.1 List of species that have annotations ----
spp <- appendix |> 
  dplyr::filter(minutes > 0)

## 2.2 Determine better fscore classifier ----
species_fscore <- species |> 
  dplyr::filter(species %in% spp$species) |> 
  group_by(species) |> 
  mutate(maxfscore=max(fscore, na.rm=TRUE)) |> 
  ungroup() |> 
  dplyr::filter(fscore==maxfscore) |> 
  group_by(species) |> 
  dplyr::filter(threshold == max(threshold)) |> 
  ungroup() |> 
  rename(better = classifier) 

# 3. Determine metrics for vocal activity dataset ----

## 3.1 Read in the annotations ----
files_activity <- list.files("data/annotations_vocalactivity", full.names = TRUE)
annotations_activity <- map_dfr(read.csv, .x=files_activity)

## 3.2 Get annotation sample sizes ----
minutes_activity <- annotations_activity |> 
  group_by(species) |> 
  summarize(minutes_activity = n()) |> 
  ungroup()

## 3.3 Get metrics at universal threshold ----
thresh_universal <- activity |> 
  dplyr::filter(threshold==0.75, classifier=="HawkEars") |> 
  dplyr::select(species, precision, recall) |> 
  rename(precision_universal_activity = precision, recall_universal_activity = recall)

## 3.4 Get metrics at max Fscore ----
thresh_fscore <- activity |> 
  dplyr::filter(classifier=="HawkEars") |> 
  group_by(species) |> 
  summarize(fscore = max(fscore)) |> 
  ungroup() |> 
  left_join(activity) |> 
  dplyr::select(species, fscore, precision, recall) |> 
  rename(precision_fscore_activity = precision, recall_fscore_activity = recall, fscore_activity = fscore) |> 
  unique()

# 4. Put together ----

## 4.1 Put together ----
appendix_out <- train  |> 
  rename(species = Code) |> 
  left_join(appendix |> 
              dplyr::filter(minutes > 0)) |> 
  left_join(species_fscore |> 
              dplyr::select(species, better)) |> 
  left_join(minutes_activity) |> 
  left_join(thresh_universal) |> 
  left_join(thresh_fscore) |> 
  mutate_at(vars(precision_universal:recall_fscore,
                 precision_universal_activity:recall_fscore_activity), ~round(.x, 3))


## 4.2 Fix column names ----
colnames(appendix_out) <- c("Common name", "Species code", "Number of training clips", "Number of evaluation detections (minutes; community)", "Precision_universalthreshold (community)", "Recall _universalthreshold (community)", "F1score threshold (community)", "Precision_F1scorethreshold (community)", "Recall_F1scorethreshold (community)", "Classifier with highest F1score (community)", "Number of evaluation detections (calls; vocal activity)", "Precision_universalthreshold (vocal activity)", "Recall _universalthreshold (vocal activity)", "F1score threshold (vocal activity)", "Precision_F1scorethreshold (vocal activity)", "Recall_F1scorethreshold (vocal activity)")

## 4.3 Save----
write.csv(appendix_out, "AppendixB.csv", row.names = FALSE)

## end script ##