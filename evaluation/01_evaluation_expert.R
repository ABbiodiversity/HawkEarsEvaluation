#This script evaluates raw hawkears, birdnet, and perch detections for the expert dataset.

#PREAMBLE############

#1. Load libraries----
library(tidyverse)
library(Metrics)
library(PRROC)
library(yardstick)
library(carat)

#2. Set root file path---
root <- "G:/Shared drives/ABMI_Recognizers/HawkEars"

#3. Load data----
raw <- read.csv(file.path(root, "Results", "ExpertData", "ExpertData_ByMinute.csv"))

#3. Set species list----
sp <- raw |> 
  dplyr::filter(!is.na(HawkEars)) |> 
  dplyr::select(species) |> 
  unique()

#4. Wrangle----
min <- raw |> 
  dplyr::filter(species %in% sp$species) |> 
  mutate(actual = ifelse(is.na(count), 0, 1))

#5. Write functions----
calculate_prf <- function(threshold, data, human_total){
  
  #Summarize
  data_thresholded <- dplyr::filter(data, confidence >= threshold) |>
    summarize(precision = sum(tp)/(sum(tp) + sum(fp)),
              recall = sum(tp)/human_total) |>
    mutate(fscore = (2*precision*recall)/(precision + recall),
           threshold = threshold)
  
}

calculate_richness <- function(threshold, data){
  
  #Summarize
  data_richness <- dplyr::filter(data, confidence >= threshold,
                                 actual==1) |> 
    dplyr::select(species) |> 
    unique() |> 
    summarize(richness = n()) |> 
    mutate(threshold = threshold)
    
  
}

#6. Set threshold sequence----
threshold <- seq(0.1, 0.99, 0.01)

#4. Wrangle for yardstick----
min.bn <- min %>% 
  mutate(truth = factor(ifelse(actual==1, "Present", "Absent"))) %>% 
  rename(Present = BirdNET) %>% 
  mutate(Absent = 1 - Present) %>% 
  mutate(predicted = factor(ifelse(Present > Absent, "Present", "Absent"))) %>% 
  dplyr::select(truth, Present, Absent, predicted, species)

min.he <- min %>% 
  mutate(truth = factor(ifelse(actual==1, "Present", "Absent"))) %>% 
  rename(Present = HawkEars) %>% 
  mutate(Absent = 1 - Present) %>% 
  mutate(predicted = factor(ifelse(Present > Absent, "Present", "Absent"))) %>% 
  dplyr::select(truth, Present, Absent, predicted, species)

#ALL SPECIES########

#1. ROC AUC----
auc(min$actual, min$BirdNET)
auc(min$actual, min$HawkEars)

roc_auc(min.bn, truth, Present)
autoplot(roc_curve(min.bn, truth, Present))
roc_auc(min.he, truth, Present)
autoplot(roc_curve(min.he, truth, Present))

#2. MAP (PR AUC)----
pr_auc(min.bn, truth, Present)
pr_curve(min.bn, truth, Present) %>% autoplot()

pr_auc(min.he, truth, Present)
pr_curve(min.he, truth, Present) %>% autoplot()

pr <- pr_curve(min.bn, truth, Present) |> 
  mutate(thresh = round(.threshold, 2)) |> 
  group_by(thresh) |> 
  summarize(precision = mean(precision), 
            recall = mean(recall)) |> 
  ungroup()

pr.old <- read.csv(file.path(root, "Results", "ExpertData", "ExpertData_PR_Total.csv"))

pr.both <- pr |> 
  right_join(pr.old |> 
               dplyr::filter(classifier=="BirdNET"))

ggplot(pr.both) +
  geom_point(aes(x=p, y=precision)) +
  geom_abline(aes(intercept=0, slope=1))

ggplot(pr.both) +
  geom_point(aes(x=r, y=recall)) +
  geom_abline(aes(intercept=0, slope=1))

#3. MAPc (mean PR AUC across species)----
min.bn %>% 
  group_by(species) %>% 
  pr_auc(truth, Present) %>% 
  summarize(cmap = mean(.estimate, na.rm=TRUE))

min.he %>% 
  group_by(species) %>% 
  pr_auc(truth, Present) %>% 
  summarize(cmap = mean(.estimate, na.rm=TRUE))

#COMMUNITY METRICS##############

#1. Wrangle----
dat.he <- min |> 
  mutate(tp = ifelse(!is.na(HawkEars) & !is.na(count), 1, 0),
         fp = ifelse(!is.na(HawkEars) & is.na(count), 1, 0),
         fn = ifelse(is.na(HawkEars) & !is.na(count), 1, 0)) |> 
  dplyr::select(-BirdNET) |> 
  rename(confidence = HawkEars)

dat.bn <- min |> 
  mutate(tp = ifelse(!is.na(BirdNET) & !is.na(count), 1, 0),
         fp = ifelse(!is.na(BirdNET) & is.na(count), 1, 0),
         fn = ifelse(is.na(BirdNET) & !is.na(count), 1, 0)) |> 
  dplyr::select(-HawkEars) |> 
  rename(confidence = BirdNET)

#2. Get total expert detections----
human_total = sum(min$actual)

#3. Get PRF----
prf.he <-  do.call(rbind, lapply(X=threshold, FUN=calculate_prf, data=dat.he, human_total=human_total))
prf.bn <-  do.call(rbind, lapply(X=threshold, FUN=calculate_prf, data=dat.bn, human_total=human_total))

#4. Get species richness----
rich.he <- do.call(rbind, lapply(X=threshold, FUN=calculate_richness, data=dat.he))
rich.bn <- do.call(rbind, lapply(X=threshold, FUN=calculate_richness, data=dat.bn))

#5. Wrangle----
out <- inner_join(prf.he, rich.he) |> 
  mutate(classifier = "HawkEars") |> 
  rbind(inner_join(prf.bn, rich.bn) |> 
          mutate(classifier = "BirdNET")) |> 
  pivot_longer(c(precision, recall, fscore, richness), names_to="metric", values_to="value")

#6. Plot----
ggplot(out) + 
  geom_line(aes(x=threshold, y=value, colour=classifier)) + 
  facet_wrap(~metric, scales="free")

#7. Save-----
write.csv(out, file.path(root, "Results", "ExpertData", "OverallPRFRichness.csv"), row.names = FALSE)


#PER SPECIES#########

#1. Set up species loop----
out.list <- list()
for(i in 1:nrow(sp)){
  
  #2. Wrangle----
  dat.i <- min |> 
    dplyr::filter(species==sp$species[i]) |> 
    mutate(tp = ifelse(!is.na(HawkEars) & !is.na(count), 1, 0),
           fp = ifelse(!is.na(HawkEars) & is.na(count), 1, 0),
           fn = ifelse(is.na(HawkEars) & !is.na(count), 1, 0)) |> 
    dplyr::select(-BirdNET) |> 
    rename(confidence = HawkEars)
  
  #3. Get total expert detections----
  human_total = sum(dat.i$actual)
  
  #4. Apply function----
  prf <- do.call(rbind, lapply(X=threshold, FUN=calculate_prf, data=dat.i, human_total=human_total))
  
  #5. Get F-score threshold----
  fscore <- arrange(prf, -fscore)[1,] |> 
    mutate(thresh = "fscore")
  
  #6. Get universal threshold----
  universe <- prf[prf$threshold==0.7,] |> 
    mutate(thresh = "universal")
  
  #7. Put together----
  out.list[[i]] <- rbind(fscore, universe) |> 
    dplyr::select(-fscore) |> 
    pivot_wider(names_from=thresh, values_from=precision:threshold) |> 
    dplyr::select(-threshold_universal) |> 
    mutate(species = sp$species[i])
  
  cat("Finished species", sp$species[i], ":", i, "of", nrow(sp), "\n")
  
}

#8. Save----
out <- do.call(rbind, out.list) |> 
  arrange(species)

write.csv(out, file.path(root, "Results", "ExpertData", "Appendix2_PR.csv"), row.names = FALSE)

#GEOGRAPHY#########