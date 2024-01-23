#This script looks at recording level effects on recall and precision of hawkears and birdnet detections relative to the expert dataset.

#PREAMBLE############

#1. Load libraries----
library(tidyverse)

#2. Set root file path---
root <- "G:/Shared drives/ABMI_Recognizers/HawkEars"

#3. Get detection dataset----
det <- read.csv(file.path(root, "Results", "ExpertData", "ExpertData_ByMinute.csv")) %>% 
  mutate(detection = ifelse(is.na(count), 0, 1))

#4. Get recording covariate dataset----
covs <- read.csv(file.path(root, "Results", "ExpertData", "ExpertData_RecordingCovariates.csv"))

#WRANGLE############

#1. Set up loop----
thresh <- seq(0, 0.99, 0.01)

#2. Calculate total number of detections for recall calculation----
det.n.rec <- det %>% 
  dplyr::filter(!is.na(count)) %>% 
  group_by(recording_id, minute) %>% 
  summarize(det.n=n()) %>% 
  ungroup()

pr.rec <- data.frame()
for(i in 1:length(thresh)){
  
  #2. Wrangle BirdNET----
  pr.bn <- det %>% 
    dplyr::filter(BirdNET > thresh[i]) %>% 
    mutate(tp = ifelse(!is.na(count), 1, 0),
           fp = ifelse(is.na(count), 1, 0),
           fn = ifelse(!is.na(count) & is.na(BirdNET), 1, 0)) %>% 
    group_by(recording_id, minute) %>% 
    summarize(tp = sum(tp),
              fp = sum(fp),
              fn = sum(fn)) %>% 
    ungroup() %>% 
    left_join(det.n.rec) %>% 
    mutate(p = tp/(tp + fp),
           r = tp/det.n,
           f = 2*(p*r)/(p+r),
           classifier = "BirdNET",
           thresh = thresh[i])
  
  #3. Wrangle HawkEars----
  pr.he <- det %>% 
    dplyr::filter(HawkEars > thresh[i]) %>% 
    mutate(tp = ifelse(!is.na(count), 1, 0),
           fp = ifelse(is.na(count), 1, 0),
           fn = ifelse(!is.na(count) & is.na(BirdNET), 1, 0)) %>% 
    group_by(recording_id, minute) %>% 
    summarize(tp = sum(tp),
              fp = sum(fp),
              fn = sum(fn)) %>% 
    ungroup() %>% 
    left_join(det.n.rec) %>% 
    mutate(p = tp/(tp + fp),
           r = tp/det.n,
           f = 2*(p*r)/(p+r),
           classifier = "HawkEars",
           thresh = thresh[i])
  
  
  #4. Put together----
  pr.rec <- rbind(pr.rec, pr.bn, pr.he)
  
}

#5. Add the covariates----
dat <- pr.rec %>% 
  inner_join(covs)

#DATA PRESENTS##########

#1. HardRain

#Band 1 PSD
ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=p, y=band.1.psd, colour=factor(thresh))) + 
  geom_smooth(aes(x=p, y=band.1.psd, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=r, y=band.1.psd, colour=factor(thresh))) + 
  geom_smooth(aes(x=r, y=band.1.psd, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=f, y=band.1.psd, colour=factor(thresh))) + 
  geom_smooth(aes(x=f, y=band.1.psd, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

#Band 2 PSD
ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=p, y=band.2.psd, colour=factor(thresh))) + 
  geom_smooth(aes(x=p, y=band.2.psd, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=r, y=band.2.psd, colour=factor(thresh))) + 
  geom_smooth(aes(x=r, y=band.2.psd, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=f, y=band.2.psd, colour=factor(thresh))) + 
  geom_smooth(aes(x=f, y=band.2.psd, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

#Band 1 S2N
ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=p, y=band.1.s2n, colour=factor(thresh))) + 
  geom_smooth(aes(x=p, y=band.1.s2n, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=r, y=band.1.s2n, colour=factor(thresh))) + 
  geom_smooth(aes(x=r, y=band.1.s2n, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=f, y=band.1.s2n, colour=factor(thresh))) + 
  geom_smooth(aes(x=f, y=band.1.s2n, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

#Band 2 S2N
ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=p, y=band.2.s2n, colour=factor(thresh))) + 
  geom_smooth(aes(x=p, y=band.2.s2n, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=r, y=band.2.s2n, colour=factor(thresh))) + 
  geom_smooth(aes(x=r, y=band.2.s2n, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=f, y=band.2.s2n, colour=factor(thresh))) + 
  geom_smooth(aes(x=f, y=band.2.s2n, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

#2. Location----
#Latitude
ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=p, y=latitude, colour=factor(thresh))) + 
  geom_smooth(aes(x=p, y=latitude, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=r, y=latitude, colour=factor(thresh))) + 
  geom_smooth(aes(x=r, y=latitude, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=f, y=latitude, colour=factor(thresh))) + 
  geom_smooth(aes(x=f, y=latitude, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

#Longitude
ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=p, y=longitude, colour=factor(thresh))) + 
  geom_smooth(aes(x=p, y=longitude, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=r, y=longitude, colour=factor(thresh))) + 
  geom_smooth(aes(x=r, y=longitude, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=f, y=longitude, colour=factor(thresh))) + 
  geom_smooth(aes(x=f, y=longitude, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

#3. File type----
ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) +
  geom_boxplot(aes(x=factor(thresh), y=p, colour=filetype)) +
  facet_wrap(~classifier)

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) +
  geom_boxplot(aes(x=factor(thresh), y=r, colour=filetype)) +
  facet_wrap(~classifier)

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) +
  geom_boxplot(aes(x=factor(thresh), y=f, colour=filetype)) +
  facet_wrap(~classifier)
