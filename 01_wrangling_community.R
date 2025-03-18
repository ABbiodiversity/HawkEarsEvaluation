# ---
# title: "01 wrangling for community dataset"
# author: "Elly Knight"
# date: "2025-03-17"
# inputs: "community dataset annotations, output tags from HawkEars, BirdNET, Perch"
# outputs: "dataset of classifier detections merged with human annotations"

# ---

# 1. Setup ----

## 1.1 Load packages ----
library(tidyverse) # data manipulation and visualization

## 1.2. Set working directory ----
# we keep raw classifier output outside the R project because the files slow down RStudio
setwd(dirname(getwd()))

## 1.3 Read in annotations ----
ann <- read.csv(file.path("HawkEarsEvaluation", "data", "annotations.csv"))

# 2. Read in HawkEars output ----

## 2.1 Get list of output files ----
files.he <- data.frame(file = list.files("tags_he")) |> 
  separate(file, into=c("recording", "classifier", "filetype"), remove=FALSE) |> 
  select(file, recording) |> 
  mutate(path = file.path("tags_he", file))

## 2.2 Read them in----
list.he <- list()
for(i in 1:nrow(files.he)){
  list.he[[i]] <- read_table(files.he$path[i], col_names = c("start", "end", "species_score"), show_col_types=FALSE)  |> 
    separate(species_score, into=c("species", "score"), sep=";") |> 
    mutate(score = as.numeric(score),
           recording = files.he$recording[i],
           classifier = "HawkEars")
}

# 3. Read in BirdNET output ----

## 3.1 Get list of output files ----
files.bn <- data.frame(file = list.files("tags_bn"), full.names=TRUE) |> 
  separate(file, into=c("recording", "classifier", "filetype"), remove=FALSE) |> 
  select(file, recording) |> 
  mutate(path = file.path("tags_bn", file))

## 3.2 Read them in----
list.bn <- list()
for(i in 1:nrow(files.he)){
  list.bn[[i]] <- read_table(files.bn$path[i], col_names = c("start", "end", "species_score"), show_col_types=FALSE)  |> 
    separate(species_score, into=c("species", "score"), sep=";") |> 
    mutate(score = as.numeric(score),
           recording = files.he$recording[i],
           classifier = "BirdNET")
}

# 4. Read in Perch output ----

## 4.1 Get list of output files ----
files.pr <- data.frame(file = list.files("tags_pr"), full.names=TRUE) |> 
  separate(file, into=c("recording", "classifier", "filetype"), remove=FALSE) |> 
  select(file, recording) |> 
  mutate(path = file.path("tags_pr", file))

## 4.2 Read them in----
list.pr <- list()
for(i in 1:nrow(files.pr)){
  list.pr[[i]] <- read_table(files.pr$path[i], col_names = c("start", "end", "species_score"), show_col_types=FALSE)  |> 
    separate(species_score, into=c("species", "score"), sep=";") |> 
    mutate(score = as.numeric(score),
           recording = files.pr$recording[i],
           classifier = "Perch")
}

# 5. Combine  & wrangle outputs ----

## 5.1 Raw data----
raw <- do.call(rbind, list.he) |>  
  rbind(do.call(rbind, list.bn)) |> 
  rbind(do.call(rbind, list.pr))

## 5.2 Summarize to minute----
minute1 <- raw |> 
  mutate(startmin = ceiling((start+0.000001)/60),
         startmin = ifelse(startmin==0, 1, startmin),
         endmin = ceiling((end)/60))

## 5.3 Identify the windows that overlap multiple minute intervals ----
# i.e., merged HawkEars detections that occur in multiple intervals and therefore should be evaluated for multiple intervals
minute2 <- minute1 |> 
  dplyr::filter(startmin!=endmin) |> 
  mutate(minute = startmin+1) |> 
  rbind(minute1 |> 
          mutate(minute = startmin))

## 5.4 Summarize to max detection score per minute ----
minute3 <- minute2 |> 
  group_by(classifier, recording, minute, species) |> 
  summarize(score = max(score)) |> 
  ungroup() |> 
  pivot_wider(names_from=classifier, values_from=score) |> 
  mutate(recording=as.integer(recording))

## 5.5 Separate annotations into one row per species ----
ann_sp <- ann |>
  separate_rows(species, sep=",") |> 
  select(-X) |> 
  mutate(detection = 1)

## 5.6 Get list of annotated minute intervals----
# Not all minutes were used per recording to stratify detections across species
intervals <- ann_sp |> 
  dplyr::select(recording, minute) |> 
  unique()

## 5.7 Get list of HawkEars species----
spp <- raw |> 
  dplyr::filter(classifier=="HawkEars") |> 
  dplyr::select(species) |> 
  unique()

## 5.8 Put everything together----
out <- full_join(minute3, ann_sp, multiple="all") |> 
  inner_join(intervals) |> 
  inner_join(spp) |> 
  mutate(detection = ifelse(is.na(detection), 0, 1),
         minute_id = paste0(recording, "_", minute))

write.csv(out, file.path("HawkEarsEvaluation", "data", "community_minute.csv"), row.names = FALSE)

## end script ##