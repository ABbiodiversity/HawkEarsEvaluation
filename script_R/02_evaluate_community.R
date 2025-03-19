# ---
# title: "02 evaluation for community dataset"
# author: "Elly Knight"
# date: "2025-03-18"
# inputs: "dataset of classifier detections merged with human annotations"
# outputs: "datasets for performance metrics across thresholds for several resolutions of evaluation"

# ---

# 1. Setup ----

## 1.1 Load packages ----
library(tidyverse) # data manipulation and visualization

## 1.2 Read in detections and annotations file ----
dat <- read.csv("data/community_minute.csv")

## 1.3 Load evaluation functions ----
source("00_functions.R")

## 1.4 Set threshold sequence for evaluation ----
threshold <- seq(0.01, 0.99, 0.01)

# 2. Wrangling ----

## 2.1 Split out for each classifier and classify tp/fp/fn ----
dat.he <- dat |> 
  mutate(tp = ifelse(!is.na(HawkEars) & detection==1, 1, 0),
         fp = ifelse(!is.na(HawkEars) & detection==0, 1, 0),
         fn = ifelse(is.na(HawkEars) & detection==1, 1, 0)) |> 
  dplyr::select(-BirdNET, -Perch) |> 
  rename(confidence = HawkEars)

dat.bn <- dat |> 
  mutate(tp = ifelse(!is.na(BirdNET) & detection==1, 1, 0),
         fp = ifelse(!is.na(BirdNET) & detection==0, 1, 0),
         fn = ifelse(is.na(BirdNET) & detection==1, 1, 0)) |> 
  dplyr::select(-HawkEars, -Perch) |> 
  rename(confidence = BirdNET)

dat.pr <- dat |> 
  mutate(tp = ifelse(!is.na(Perch) & detection==1, 1, 0),
         fp = ifelse(!is.na(Perch) & detection==0, 1, 0),
         fn = ifelse(is.na(Perch) & detection==1, 1, 0)) |> 
  dplyr::select(-HawkEars, -BirdNET) |> 
  rename(confidence = Perch)

# 3. Overall evaluation ----

## 3.1 Get total annotations ----
annotations_total <- sum(dat$detection)

## 3.2 Evaluate ----
overall.he <- do.call(rbind, lapply(X=threshold, FUN=calculate_prf, data=dat.he, annotations = annotations_total)) |> 
  mutate(classifier="HawkEars")

overall.bn <- do.call(rbind, lapply(X=threshold, FUN=calculate_prf, data=dat.bn, annotations = annotations_total)) |> 
  mutate(classifier = "BirdNET")

overall.pr <- do.call(rbind, lapply(X=threshold, FUN=calculate_prf, data=dat.pr, annotations = annotations_total)) |> 
  mutate(classifier = "Perch")

overall <- rbind(overall.he, overall.bn, overall.pr) |> 
  pivot_longer(precision:fscore, names_to="metric", values_to="value")

## 3.3 Plot to check ----
ggplot(overall) +
  geom_line(aes(x=threshold, y=value, colour=classifier)) +
  facet_wrap(~metric, scales="free")

# 4. Evaluation per recording ----

## 4.1 Set up recording loop----
minutes <- sort(unique(dat.he$minute_id))

minutes.list <- list()
for(i in 1:length(minutes)){
  
  ## 4.2 Subset data----
  dat.he.i <- dat.he |> 
    dplyr::filter(minute_id==minutes[i])
  dat.bn.i <- dat.bn |> 
    dplyr::filter(minute_id==minutes[i])
  dat.pr.i <- dat.pr |> 
    dplyr::filter(minute_id==minutes[i])
  
  ## 4.3 Get total annotations ----
  annotations_recording = sum(dat.he.i$detection)
  
  ## 4.4 Get total richness----
  richness_recording <- dat.he.i |> 
    dplyr::filter(detection==1) |> 
    dplyr::select(species) |> 
    unique() |> 
    nrow()
  
  ## 4.5 Evaluate ----
  prf.he <-  do.call(rbind, lapply(X=threshold, FUN=calculate_prf, data=dat.he.i, annotations = annotations_recording))
  prf.bn <-  do.call(rbind, lapply(X=threshold, FUN=calculate_prf, data=dat.bn.i, annotations = annotations_recording))
  prf.pr <-  do.call(rbind, lapply(X=threshold, FUN=calculate_prf, data=dat.pr.i, annotations = annotations_recording))
  
  ## 4.6 Get species richness ----
  rich.he <- do.call(rbind, lapply(X=threshold, FUN=calculate_richness, data=dat.he.i))
  rich.bn <- do.call(rbind, lapply(X=threshold, FUN=calculate_richness, data=dat.bn.i))
  rich.pr <- do.call(rbind, lapply(X=threshold, FUN=calculate_richness, data=dat.pr.i))
  
  ## 4.7 Put together ----
  minutes.list[[i]] <- inner_join(prf.he, rich.he, by="threshold") |> 
    mutate(classifier = "HawkEars") |> 
    rbind(inner_join(prf.bn, rich.bn, by="threshold") |> 
            mutate(classifier = "BirdNET")) |> 
    rbind(inner_join(prf.pr, rich.pr,  by="threshold") |> 
            mutate(classifier = "Perch")) |> 
    pivot_longer(c(precision, recall, fscore, richness), names_to="metric", values_to="value") |> 
    mutate(minute_id = minutes[i],
           total_richness = richness_recording)
  
  ## 4.8 Tidy and report out ----
  rm(dat.he.i, dat.bn.i, dat.pr.i, annotations_recording, richness_recording, prf.he, prf.bn, prf.pr, rich.he, rich.bn, rich.pr)
  
  cat("Finished recording minute", i, "of", length(minutes), "\n")
  
}

## 4.9 Put together----
minutes_out <- do.call(rbind, minutes.list)

## 4.10 Summarize & plot to check----
minutes_summary <- minutes_out |> 
  group_by(threshold, classifier, metric) |> 
  summarize(mn = mean(value, na.rm = TRUE),
            std = sd(value, na.rm = TRUE)) |> 
  ungroup()

ggplot(minutes_summary) + 
  geom_ribbon(aes(x=threshold, ymin = mn-std, ymax = mn+std, group=classifier), alpha = 0.3) +
  geom_line(aes(x=threshold, y=mn, colour=classifier)) + 
  facet_wrap(~metric, scales="free")

## 4.11 Save-----
write.csv(minutes_out, "results/Evaluation_community_recording.csv", row.names = FALSE)

# 5. Evaluation per species ----

## 5.1 Set up species loop----
sp <- dat |> 
  dplyr::select(species) |> 
  unique() |> 
  arrange(species)

appendix.list <- list()
species.list <- list()
for(i in 1:nrow(sp)){
  
  ## 5.2 Wrangle----
  dat.he.i <- dat |> 
    dplyr::filter(species==sp$species[i]) |> 
    mutate(tp = ifelse(!is.na(HawkEars) & detection==1, 1, 0),
           fp = ifelse(!is.na(HawkEars) & detection==0, 1, 0),
           fn = ifelse(is.na(HawkEars) & detection==1, 1, 0)) |> 
    dplyr::select(-BirdNET, -Perch) |> 
    rename(confidence = HawkEars)
  
  dat.bn.i <- dat |> 
    dplyr::filter(species==sp$species[i]) |> 
    mutate(tp = ifelse(!is.na(BirdNET) & detection==1, 1, 0),
           fp = ifelse(!is.na(BirdNET) & detection==0, 1, 0),
           fn = ifelse(is.na(BirdNET) & detection==1, 1, 0)) |> 
    dplyr::select(-HawkEars, -Perch) |> 
    rename(confidence = BirdNET)
  
  dat.pr.i <- dat |> 
    dplyr::filter(species==sp$species[i]) |> 
    mutate(tp = ifelse(!is.na(Perch) & detection==1, 1, 0),
           fp = ifelse(!is.na(Perch) & detection==0, 1, 0),
           fn = ifelse(is.na(Perch) & detection==1, 1, 0)) |> 
    dplyr::select(-HawkEars, -BirdNET) |> 
    rename(confidence = Perch)
  
  ## 5.3 Get total annotations ----
  annotations_species = sum(dat.he.i$detection)
  
  ## 5.4 Evaluate ----
  prf.he <- do.call(rbind, lapply(X=threshold, FUN=calculate_prf, data=dat.he.i, annotations=annotations_species))
  
  prf.bn <- do.call(rbind, lapply(X=threshold, FUN=calculate_prf, data=dat.bn.i, annotations=annotations_species))
  
  prf.pr <- do.call(rbind, lapply(X=threshold, FUN=calculate_prf, data=dat.pr.i, annotations=annotations_species))
  
  ## 5.5 Get performance for threshold of max F1score ----
  thresh_fscore <- arrange(prf.he, -fscore)[1,] |> 
    mutate(threshold_type = "fscore")
  
  ## 5.6 Get performance for universal threshold ----
  thresh_universal <- prf.he[prf.he$threshold==0.75,] |> 
    mutate(threshold_type = "universal")

  ## 5.8 Put together details for appendix ----
  appendix.list[[i]] <- rbind(thresh_fscore, thresh_universal) |> 
    dplyr::select(-fscore) |> 
    pivot_wider(names_from=threshold_type, values_from=precision:threshold) |> 
    dplyr::select(-threshold_universal) |> 
    mutate(species = sp$species[i],
           minutes = annotations_species) |> 
    dplyr::select(species, minutes, precision_universal, recall_universal, threshold_fscore, precision_fscore, recall_fscore)

  ## 5.9. Put together total evaluation details ----
  species.list[[i]] <- prf.he |> 
    mutate(classifier="HawkEars") |> 
    rbind(prf.bn |> 
            mutate(classifier="BirdNET")) |> 
    rbind(prf.pr |> 
            mutate(classifier="Perch")) |> 
    mutate(species = sp$species[i])
  
  ## 5.10 Tidy and report out ----
  rm(dat.he.i, dat.bn.i, dat.pr.i, annotations_species, prf.he, prf.bn, prf.pr, thresh_fscore, thresh_universal)
  
  cat("Finished species", sp$species[i], ":", i, "of", nrow(sp), "\n")
  
}

## 5.11 Put together ----
species_out <- do.call(rbind, species.list) |> 
  arrange(species)

appendix_out <- do.call(rbind, appendix.list) |> 
  arrange(species)

## 5.12 Save ----
write.csv(species_out, "results/Evaluation_community_species.csv", row.names = FALSE)
write.csv(appendix_out, "results/Evaluation_community_appendix.csv", row.names = FALSE)

## end script ##