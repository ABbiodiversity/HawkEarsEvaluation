#This script explores hawkears and birdnet detections for the expert dataset.

#PREAMBLE############

#1. Load libraries----
library(tidyverse)
library(lme4)

#2. Set root file path---
root <- "G:/Shared drives/ABMI_Recognizers/HawkEars"

#3. Get detection dataset----
det <- read.csv(file.path(root, "Results", "ExpertData", "ExpertData_ByMinute.csv")) %>% 
  mutate(detection = ifelse(is.na(count), 0, 1))

#4. Get recording covariate dataset----
covs <- read.csv(file.path(root, "Results", "ExpertData", "ExpertData_RecordingCovariates.csv"))

#5. Get species covariate dataset----

#SOME PLOTS#############

#relationship between two classifiers
ggplot(det) + 
  geom_point(aes(x=HawkEars, y=BirdNET, colour=factor(detection))) + 
  geom_smooth(aes(x=HawkEars, y=BirdNET, colour=factor(detection)))

#Is either score related to abundance?
ggplot(det) + 
  geom_point(aes(x=HawkEars, y=count, colour=factor(detection))) + 
  geom_smooth(aes(x=HawkEars, y=count, colour=factor(detection)))

ggplot(det) + 
  geom_point(aes(x=BirdNET, y=count, colour=factor(detection))) + 
  geom_smooth(aes(x=BirdNET, y=count, colour=factor(detection)))

det.1 <- dplyr::filter(det, !is.na(count))

m.count.he <- glm(count ~ HawkEars, data=det.1, family="poisson")
summary(m.count.he)

m.count.bn <- glm(count ~ BirdNET, data=det.1, family="poisson")
summary(m.count.bn)
