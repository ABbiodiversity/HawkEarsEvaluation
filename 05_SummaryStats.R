# ---
# title: "05 summary statistics for manuscript"
# author: "Elly Knight"
# date: "2025-03-18"
# inputs: "evaluation output from `02_evaluate_community.R" and from python evaluation script in HawkEars repo for vocal activity dataset, training data records"
# outputs: "none"

# 1. Setup ----

## 1.1 Load libraries----
library(tidyverse)

## 1.2 Get inputs ----
annotations_community <- read.csv("data/annotations_community.csv")
files_community <- read.csv("data/filelist_community.csv") |> 
  unique()
minutes <- read.csv("results/Evaluation_community_recording.csv")
dat <- read.csv("data/community_minute.csv")
appendix <- read.csv("results/AppendixB.csv")
activity <- read.csv("results/Evaluation_vocalactivity.csv") |> 
  dplyr::filter(threshold >= 0.1)

# 2. Describe community dataset ----

## 2.1 Number of recordings ----
nrow(files_community)

## 2.2 Number of recording minutes ----
annotations_community |> 
  dplyr::select(recording, minute) |> 
  unique() |> 
  nrow()

## 2.3 Number of species ----
dat |> 
  dplyr::filter(detection==1) |> 
  select(species) |> 
  unique() |> 
  nrow()

## 2.4 Range of detections ----
summary(appendix$Number.of.training.clips)

# 3. Describe vocal activity dataset ----

## 3.1 Number of recordings ----
files_activity <- list.files("annotations_vocalactivity", full.names = TRUE)
map_dfr(read.csv, .x=files_activity) |> 
  dplyr::select(species, recording) |> 
  unique() |> 
  nrow()

## 3.2 Mean duration per species ----
map_dfr(read.csv, .x=files_activity) |> 
  dplyr::select(species, recording) |> 
  unique() |> 
  group_by(species) |> 
  summarize(minutes = n())

# 4. Community performance----

## 4.1 Calculate mean across recording minutes ----
minutes_summary <- minutes |> 
  group_by(threshold, classifier, metric) |> 
  summarize(mn = mean(value, na.rm = TRUE),
            std = sd(value, na.rm = TRUE)) |> 
  ungroup()

## 4.2 Precision cross over values ----
#BirdNET
minutes_summary |> 
  dplyr::select(-std) |> 
  dplyr::filter(metric=="precision") |> 
  pivot_wider(names_from=classifier, values_from=mn) |> 
  dplyr::filter(HawkEars > BirdNET) |> 
  head(1)

#Perch
minutes_summary |> 
  dplyr::select(-std) |> 
  dplyr::filter(metric=="precision") |> 
  pivot_wider(names_from=classifier, values_from=mn) |> 
  dplyr::filter(HawkEars > Perch) |> 
  head(1)

## 4.3 Recall cross over values ----
minutes_summary |> 
  dplyr::select(-std) |> 
  dplyr::filter(metric=="recall") |> 
  pivot_wider(names_from=classifier, values_from=mn) |> 
  dplyr::filter(BirdNET > HawkEars) |> 
  head(1)

minutes_summary |> 
  dplyr::select(-std) |> 
  dplyr::filter(metric=="recall") |> 
  pivot_wider(names_from=classifier, values_from=mn) |> 
  dplyr::filter(Perch > HawkEars) |> 
  head(1)

## 4.4 Performance at recommended threshold ----

#metric at recommended threshold
minutes_summary |> 
  dplyr::filter(classifier=="HawkEars", threshold==0.75)

## 4.5 Max performance ----

#max recall
head(minutes_summary |> dplyr::filter(classifier=="HawkEars", metric=="recall"), 1)

#max precision
minutes_summary |> dplyr::filter(classifier=="HawkEars", metric=="precision", threshold==0.95)

## 4.6 Species with better fscore ----

table(appendix$Classifier.with.highest.F1score..community.)

## 4.7 Species richness ----

#HawkEars at default
minutes_summary |> 
  dplyr::filter(threshold==0.75, metric=="richness", classifier=="HawkEars")

minutes_summary |> 
  dplyr::filter(round(mn, 2) %in% c(0.78:0.80),
                metric=="precision") |> 
  group_by(classifier) |> 
  dplyr::filter(threshold==min(threshold)) |> 
  ungroup() |> 
  dplyr::select(classifier, threshold) |> 
  left_join(minutes_summary) |> 
  dplyr::filter(metric=="richness")

# 5. Call rate performance ----

## 5.1 recall at 90% precision ----
activity |> 
  mutate(diff = abs(0.9 - precision)) |> 
  group_by(species, classifier) |> 
  dplyr::filter(diff == min(diff)) |> 
  sample_n(1) |> 
  ungroup() |> 
  group_by(classifier) |> 
  summarize(recallmn = mean(recall),
            recallsd = sd(recall))

## 5.2 RUGR performance ----
activity |> 
  dplyr::filter(species=="RUGR") |> 
  group_by(classifier) |> 
  summarize(recall = max(recall))

## end script ##