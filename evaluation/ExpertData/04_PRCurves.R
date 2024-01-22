#This script calculates precision-recall curves for hawkears and birdnet detections relative to the expert dataset.

#PREAMBLE############

#1. Load libraries----
library(tidyverse)

#2. Set root file path---
root <- "G:/Shared drives/ABMI_Recognizers/HawkEars"

#3. Get detection dataset----
det <- read.csv(file.path(root, "Results", "ExpertData", "ExpertData_ByMinute.csv")) %>% 
  mutate(detection = ifelse(is.na(count), 0, 1))

#OVERALL PR CURVES##########

#1. Set up loop----
thresh <- seq(0, 0.99, 0.01)

#2. Calculate total number of detections for recall calculation----
det.n <- nrow(dplyr::filter(det, !is.na(count)))

pr <- data.frame()
for(i in 1:length(thresh)){
  
  #2. Wrangle BirdNET----
  pr.bn <- det %>% 
    dplyr::filter(BirdNET > thresh[i]) %>% 
    mutate(tp = ifelse(!is.na(count), 1, 0),
           fp = ifelse(is.na(count), 1, 0),
           fn = ifelse(!is.na(count) & is.na(BirdNET), 1, 0)) %>% 
    summarize(p = sum(tp)/(sum(tp) + sum(fp)),
              r = sum(tp)/det.n) %>% 
    mutate(classifier = "BirdNET",
           thresh = thresh[i])
  
  #3. Wrangle HawkEars----
  pr.he <- det %>% 
    dplyr::filter(HawkEars > thresh[i]) %>% 
    mutate(tp = ifelse(!is.na(count), 1, 0),
           fp = ifelse(is.na(count), 1, 0),
           fn = ifelse(!is.na(count) & is.na(BirdNET), 1, 0)) %>% 
    summarize(p = sum(tp)/(sum(tp) + sum(fp)),
              r = sum(tp)/det.n) %>% 
    mutate(classifier = "HawkEars",
           thresh = thresh[i])
  
  #4. Put together----
  pr <- rbind(pr, pr.bn, pr.he)
  
}

#5. Plot-----
ggplot(pr) + 
  geom_line(aes(x=p, y=r, colour=classifier), linewidth=2) +
  xlab("Precision") +
  ylab("Recall")

ggplot(pr) + 
  geom_line(aes(x=thresh, y=p, colour=classifier), linewidth=2) +
  xlab("Threshold") +
  ylab("Precision")

ggplot(pr) + 
  geom_line(aes(x=thresh, y=r, colour=classifier), linewidth=2) +
  xlab("Threshold") +
  ylab("Recall")

#SPECIES PR CURVES##########

#1. Set up loop----
thresh <- seq(0, 0.99, 0.01)

#2. Calculate total number of detections for recall calculation----
det.n.sp <- det %>% 
  dplyr::filter(!is.na(count)) %>% 
  group_by(species) %>% 
  summarize(det.n=n()) %>% 
  ungroup()

pr.sp <- data.frame()
for(i in 1:length(thresh)){
  
  #2. Wrangle BirdNET----
  pr.bn <- det %>% 
    dplyr::filter(BirdNET > thresh[i]) %>% 
    mutate(tp = ifelse(!is.na(count), 1, 0),
           fp = ifelse(is.na(count), 1, 0),
           fn = ifelse(!is.na(count) & is.na(BirdNET), 1, 0)) %>% 
    group_by(species) %>% 
    summarize(tp = sum(tp),
              fp = sum(fp),
              fn = sum(fn)) %>% 
    ungroup() %>% 
    left_join(det.n.sp) %>% 
    mutate(p = tp/(tp + fp),
           r = tp/det.n,
           classifier = "BirdNET",
           thresh = thresh[i])
  
  #3. Wrangle HawkEars----
  pr.he <- det %>% 
    dplyr::filter(HawkEars > thresh[i]) %>% 
    mutate(tp = ifelse(!is.na(count), 1, 0),
           fp = ifelse(is.na(count), 1, 0),
           fn = ifelse(!is.na(count) & is.na(BirdNET), 1, 0)) %>% 
    group_by(species) %>% 
    summarize(tp = sum(tp),
              fp = sum(fp),
              fn = sum(fn)) %>% 
    ungroup() %>% 
    left_join(det.n.sp) %>% 
    mutate(p = tp/(tp + fp),
           r = tp/det.n,
           classifier = "HawkEars",
           thresh = thresh[i])
  
  
  #4. Put together----
  pr.sp <- rbind(pr.sp, pr.bn, pr.he)
  
}

#5. Plot-----
ggplot(pr.sp) + 
  geom_line(aes(x=p, y=r, colour=species), linewidth=2, show.legend = FALSE) +
  xlab("Precision") +
  ylab("Recall") +
  facet_wrap(~classifier)

ggplot(pr.sp) +
  geom_line(aes(x=thresh, y=p, colour=species), show.legend = FALSE) +
  xlab("Threshold") +
  ylab("Precision") +
  facet_wrap(~classifier)

ggplot(pr.sp) +
  geom_line(aes(x=thresh, y=r, colour=species), show.legend = FALSE) +
  xlab("Threshold") +
  ylab("Precision") +
  facet_wrap(~classifier)

#relationship with # detections----
ggplot(pr.sp %>% 
         dplyr::filter(thresh==0.8)) +
  geom_point(aes(x=det.n, y=p, colour=species), show.legend=FALSE)

ggplot(pr.sp %>% 
         dplyr::filter(thresh==0.8)) +
  geom_point(aes(x=det.n, y=r, colour=species), show.legend=FALSE)

