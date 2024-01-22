#This script looks at species-specific effects on recall and precision of hawkears and birdnet detections relative to the expert dataset.

#PREAMBLE############

#1. Load libraries----
library(tidyverse)

#2. Set root file path---
root <- "G:/Shared drives/ABMI_Recognizers/HawkEars"

#3. Get detection dataset----
det <- read.csv(file.path(root, "Results", "ExpertData", "ExpertData_ByMinute.csv")) %>% 
  mutate(detection = ifelse(is.na(count), 0, 1))

#4. Get species covariate dataset----
trait <- read.csv("G:/Shared drives/ABMI_Recognizers/HawkEars/Results/speciestraits.csv")

#5. Get training sample size dataset----
train <- read.csv("G:/Shared drives/ABMI_Recognizers/HawkEars/Results/HawkEars-training-record-counts.csv")

#WRANGLE############

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
           f = 2*(p*r)/(p+r),
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
           f = 2*(p*r)/(p+r),
           classifier = "HawkEars",
           thresh = thresh[i])
  
  
  #4. Put together----
  pr.sp <- rbind(pr.sp, pr.bn, pr.he)
  
}

#5. Add the covariates----
dat <- pr.sp %>% 
  left_join(trait %>% 
              rename(species = spp) %>% 
              dplyr::select(-ScientificName, CommonName)) %>% 
  left_join(train %>% 
              rename(species = Code, train = Count) %>% 
              dplyr::select(-Name)) %>% 
  mutate(MidFreq.khz = (MinFreq.khz + MaxFreq.khz)/2,
         SongDurationMid.sec = (SongDurationMin.sec + SongDurationMax.sec))

#DATA PRESENTS##########

#1. Sample size----
ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8),
                             classifier=="HawkEars")) + 
  geom_point(aes(x=p, y=train, colour=factor(thresh))) + 
  geom_smooth(aes(x=p, y=train, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ .)

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8),
                             classifier=="HawkEars")) + 
  geom_point(aes(x=r, y=train, colour=factor(thresh))) + 
  geom_smooth(aes(x=r, y=train, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ .)

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8),
                             classifier=="HawkEars")) + 
  geom_point(aes(x=f, y=train, colour=factor(thresh))) + 
  geom_smooth(aes(x=f, y=train, colour=factor(thresh)),  method="lm") +
  facet_grid(thresh ~ .)

#Relationship with recall

#2. Vocalization frequency----

#Mid frequency
ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=p, y=MidFreq.khz, colour=factor(thresh))) + 
  geom_smooth(aes(x=p, y=MidFreq.khz, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=r, y=MidFreq.khz, colour=factor(thresh))) + 
  geom_smooth(aes(x=r, y=MidFreq.khz, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=f, y=MidFreq.khz, colour=factor(thresh))) + 
  geom_smooth(aes(x=f, y=MidFreq.khz, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

#Max frequency
ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=p, y=MaxFreq.khz, colour=factor(thresh))) + 
  geom_smooth(aes(x=p, y=MaxFreq.khz, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=r, y=MaxFreq.khz, colour=factor(thresh))) + 
  geom_smooth(aes(x=r, y=MaxFreq.khz, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=f, y=MaxFreq.khz, colour=factor(thresh))) + 
  geom_smooth(aes(x=f, y=MaxFreq.khz, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

#Min frequency
ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=p, y=MinFreq.khz, colour=factor(thresh))) + 
  geom_smooth(aes(x=p, y=MinFreq.khz, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=r, y=MinFreq.khz, colour=factor(thresh))) + 
  geom_smooth(aes(x=r, y=MinFreq.khz, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=f, y=MinFreq.khz, colour=factor(thresh))) + 
  geom_smooth(aes(x=f, y=MinFreq.khz, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

#Max freq related to recall, min freq related to precision

#3. Mass-----
ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8),
                             Weight.g < 100)) + 
  geom_point(aes(x=p, y=Weight.g, colour=factor(thresh))) + 
  geom_smooth(aes(x=p, y=Weight.g, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8),
                             Weight.g < 100)) + 
  geom_point(aes(x=r, y=Weight.g, colour=factor(thresh))) + 
  geom_smooth(aes(x=r, y=Weight.g, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8),
                             Weight.g < 100)) + 
  geom_point(aes(x=f, y=Weight.g, colour=factor(thresh))) + 
  geom_smooth(aes(x=f, y=Weight.g, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

#positive effect of mass on precision for birdnet only

#4. Song duration----

#Mid duration
ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=p, y=SongDurationMid.sec, colour=factor(thresh))) + 
  geom_smooth(aes(x=p, y=SongDurationMid.sec, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=r, y=SongDurationMid.sec, colour=factor(thresh))) + 
  geom_smooth(aes(x=r, y=SongDurationMid.sec, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=f, y=SongDurationMid.sec, colour=factor(thresh))) + 
  geom_smooth(aes(x=f, y=SongDurationMid.sec, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

#Max duration
ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=p, y=SongDurationMax.sec, colour=factor(thresh))) + 
  geom_smooth(aes(x=p, y=SongDurationMax.sec, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=r, y=SongDurationMax.sec, colour=factor(thresh))) + 
  geom_smooth(aes(x=r, y=SongDurationMax.sec, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=f, y=SongDurationMax.sec, colour=factor(thresh))) + 
  geom_smooth(aes(x=f, y=SongDurationMax.sec, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

#Min duration
ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=p, y=SongDurationMin.sec, colour=factor(thresh))) + 
  geom_smooth(aes(x=p, y=SongDurationMin.sec, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=r, y=SongDurationMin.sec, colour=factor(thresh))) + 
  geom_smooth(aes(x=r, y=SongDurationMin.sec, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=f, y=SongDurationMin.sec, colour=factor(thresh))) + 
  geom_smooth(aes(x=f, y=SongDurationMin.sec, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

#longer songs have lower precision

#5. Migration type-----
ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) +
  geom_boxplot(aes(x=factor(thresh), y=p, colour=MigType))

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) +
  geom_boxplot(aes(x=factor(thresh), y=r, colour=MigType))

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) +
  geom_boxplot(aes(x=factor(thresh), y=f, colour=MigType))

#Residents seem to have lower overall performance????
#6. Forest age-----
ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=p, y=ForestAge, colour=factor(thresh))) + 
  geom_smooth(aes(x=p, y=ForestAge, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=r, y=ForestAge, colour=factor(thresh))) + 
  geom_smooth(aes(x=r, y=ForestAge, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=f, y=ForestAge, colour=factor(thresh))) + 
  geom_smooth(aes(x=f, y=ForestAge, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

#Matters for precision, but in opposite directions for birdnet vs hawkears?

#7. EDR----
ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=p, y=log.tau0, colour=factor(thresh))) + 
  geom_smooth(aes(x=p, y=log.tau0, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=r, y=log.tau0, colour=factor(thresh))) + 
  geom_smooth(aes(x=r, y=log.tau0, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(x=f, y=log.tau0, colour=factor(thresh))) + 
  geom_smooth(aes(x=f, y=log.tau0, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

#Affects precision