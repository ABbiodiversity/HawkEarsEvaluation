#This script looks at recording level effects on recall and precision of hawkears and birdnet detections relative to the expert dataset.

#PREAMBLE############

#1. Load libraries----
library(tidyverse)
library(lme4)
library(MuMIn)
library(gridExtra)
library(ggridges)

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
    mutate(BirdNET = ifelse(BirdNET <= thresh[i], NA, BirdNET)) %>% 
    dplyr::filter(!(is.na(BirdNET) & is.na(count))) %>% 
    mutate(tp = ifelse(!is.na(count) & !is.na(BirdNET), 1, 0),
           fp = ifelse(is.na(count) & !is.na(BirdNET), 1, 0),
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
    mutate(HawkEars = ifelse(HawkEars <= thresh[i], NA, HawkEars)) %>% 
    dplyr::filter(!(is.na(HawkEars) & is.na(count))) %>% 
    mutate(tp = ifelse(!is.na(count) & !is.na(HawkEars), 1, 0),
           fp = ifelse(is.na(count) & !is.na(HawkEars), 1, 0),
           fn = ifelse(!is.na(count) & is.na(HawkEars), 1, 0)) %>% 
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

#6. Save----
write.csv(pr.rec, file.path(root, "Results", "ExpertData", "ExpertData_PR_Recording.csv"), row.names = FALSE)

#DATA PRESENTS##########

#1. HardRain

#Band 1 PSD
ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=p, x=band.1.psd, colour=factor(thresh))) + 
  geom_smooth(aes(y=p, x=band.1.psd, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=r, x=band.1.psd, colour=factor(thresh))) + 
  geom_smooth(aes(y=r, x=band.1.psd, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=f, x=band.1.psd, colour=factor(thresh))) + 
  geom_smooth(aes(y=f, x=band.1.psd, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

#Band 2 PSD
ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=p, x=band.2.psd, colour=factor(thresh))) + 
  geom_smooth(aes(y=p, x=band.2.psd, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=r, x=band.2.psd, colour=factor(thresh))) + 
  geom_smooth(aes(y=r, x=band.2.psd, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=f, x=band.2.psd, colour=factor(thresh))) + 
  geom_smooth(aes(y=f, x=band.2.psd, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

#Band 1 S2N
plot.p.s1 <- ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=p, x=band.1.s2n, colour=factor(thresh))) + 
  geom_smooth(aes(y=p, x=band.1.s2n, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free") +
  xlab("Precision per recording minute") +
  ylab("Signal to noise ratio (0.6-1.2 Hz)") +
  scale_colour_discrete(name = "Threshold")
plot.p.s1

plot.r.s1 <- ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=r, x=band.1.s2n, colour=factor(thresh))) + 
  geom_smooth(aes(y=r, x=band.1.s2n, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free") +
  xlab("Recall per recording minute") +
  ylab("Signal to noise ratio (0.6-1.2 Hz)") +
  scale_colour_discrete(name = "Threshold")

ggsave(grid.arrange(plot.p.s1, plot.r.s1, ncol=1), filename=file.path(root, "Figures", "ExpertData_S2N1.jpeg"),
       width = 16, height = 16)

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=f, x=band.1.s2n, colour=factor(thresh))) + 
  geom_smooth(aes(y=f, x=band.1.s2n, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

#Band 2 S2N
plot.p.s2 <- ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=p, x=band.2.s2n, colour=factor(thresh))) + 
  geom_smooth(aes(y=p, x=band.2.s2n, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free") +
  xlab("Precision per recording minute") +
  ylab("Signal to noise ratio (4.4-5.6 Hz)") +
  scale_colour_discrete(name = "Threshold")

plot.r.s2 <- ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=r, x=band.2.s2n, colour=factor(thresh))) + 
  geom_smooth(aes(y=r, x=band.2.s2n, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free") +
  xlab("Recall per recording minute") +
  ylab("Signal to noise ratio (4.4-5.6 Hz)") +
  scale_colour_discrete(name = "Threshold")

ggsave(grid.arrange(plot.p.s2, plot.r.s2, ncol=1), filename=file.path(root, "Figures", "ExpertData_S2N2.jpeg"),
       width = 16, height = 16)

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=f, x=band.2.s2n, colour=factor(thresh))) + 
  geom_smooth(aes(y=f, x=band.2.s2n, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

#2. Location----
#Latitude
plot.p.l <- ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=p, x=latitude, colour=factor(thresh))) + 
  geom_smooth(aes(y=p, x=latitude, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free") +
  xlab("Precision per recording minute") +
  ylab("Latitude") +
  scale_colour_discrete(name = "Threshold")

plot.r.l <- ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=r, x=latitude, colour=factor(thresh))) + 
  geom_smooth(aes(y=r, x=latitude, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free") +
  xlab("Recall per recording minute") +
  ylab("Latitude") +
  scale_colour_discrete(name = "Threshold")

ggsave(grid.arrange(plot.p.l, plot.r.l, ncol=1), filename=file.path(root, "Figures", "ExpertData_Latitude.jpeg"),
       width = 16, height = 16)

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=f, x=latitude, colour=factor(thresh))) + 
  geom_smooth(aes(y=f, x=latitude, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

#Longitude
plot.p.ln <- ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=p, x=longitude, colour=factor(thresh))) + 
  geom_smooth(aes(y=p, x=longitude, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free") +
  xlab("Precision per recording minute") +
  ylab("Longitude") +
  scale_colour_discrete(name = "Threshold")

plot.r.ln <- ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=r, x=longitude, colour=factor(thresh))) + 
  geom_smooth(aes(y=r, x=longitude, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free") +
  xlab("Recall per recording minute") +
  ylab("Longitude") +
  scale_colour_discrete(name = "Threshold")

ggsave(grid.arrange(plot.p.ln, plot.r.ln, ncol=1), filename=file.path(root, "Figures", "ExpertData_Longitude.jpeg"),
       width = 16, height = 16)

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=f, x=longitude, colour=factor(thresh))) + 
  geom_smooth(aes(y=f, x=longitude, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

#Both
plot.p.loc <- ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8),
                                           !is.na(p))) +
  geom_point(aes(x=longitude, y=latitude, colour=p))  +
  xlab("Longitude") +
  ylab("Latitude") +
  facet_grid(thresh ~ classifier, scales="free") +
  scale_colour_viridis_c(name="Precision per\nrecording minute")
plot.p.loc

plot.r.loc <- ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8),
                                           !is.na(r))) +
  geom_point(aes(x=longitude, y=latitude, colour=r))  +
  xlab("Longitude") +
  ylab("Latitude") +
  facet_grid(thresh ~ classifier, scales="free") +
  scale_colour_viridis_c(name="Recall per\nrecording minute")
plot.r.loc

ggsave(grid.arrange(plot.p.loc, plot.r.loc, ncol=1), filename=file.path(root, "Figures", "ExpertData_Location.jpeg"),
       width = 16, height = 16)
  
#3. File type----
plot.r.p <- ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8),
                                         !is.na(filetype))) +
  geom_density_ridges(aes(x=p, y=factor(thresh), fill=filetype), alpha = 0.5) +
  xlab("Precision per recording minute") +
  ylab("Threshold") +
  scale_fill_discrete(name = "Recording\nfile type") +
  facet_wrap(~classifier)
plot.r.p

plot.r.r <- ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8),
                                         !is.na(filetype))) +
  geom_density_ridges(aes(x=r, y=factor(thresh), fill=filetype), alpha = 0.5) +
  xlab("Recall per recording minute") +
  ylab("Threshold") +
  scale_fill_discrete(name = "Recording\nfile type") +
  facet_wrap(~classifier)
plot.r.r

ggsave(grid.arrange(plot.r.p, plot.r.r, ncol=1), filename=file.path(root, "Figures", "ExpertData_RecordingFileType.jpeg"),
       width = 16, height = 16)

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) +
  geom_boxplot(aes(y=factor(thresh), x=f, colour=filetype)) +
  facet_wrap(~classifier)


#4. Acoustic indices----

#ACI
plot.p.ac <- ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8),
                             aci < 2500)) + 
  geom_point(aes(y=p, x=aci, colour=factor(thresh))) + 
  geom_smooth(aes(y=p, x=aci, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free") +
  ylab("Precision per recording minute") +
  xlab("Acoustic complexity index") +
  scale_colour_discrete(name = "Threshold")
plot.p.ac

plot.r.ac <- ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8),
                             aci < 2500)) + 
  geom_point(aes(y=r, x=aci, colour=factor(thresh))) + 
  geom_smooth(aes(y=r, x=aci, colour=factor(thresh)), method="lm")  +
  facet_grid(thresh ~ classifier, scales="free") +
  ylab("Recall per recording minute") +
  xlab("Acoustic complexity index") +
  scale_colour_discrete(name = "Threshold")
plot.r.ac

ggsave(grid.arrange(plot.r.ac, plot.r.ac, ncol=1), filename=file.path(root, "Figures", "ExpertData_ACI.jpeg"),
       width = 16, height = 16)

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8),
                             aci < 2500)) + 
  geom_point(aes(y=f, x=aci, colour=factor(thresh))) + 
  geom_smooth(aes(y=f, x=aci, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

#ADI
ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=p, x=adi, colour=factor(thresh))) + 
  geom_smooth(aes(y=p, x=adi, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=r, x=adi, colour=factor(thresh))) + 
  geom_smooth(aes(y=r, x=adi, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=f, x=adi, colour=factor(thresh))) + 
  geom_smooth(aes(y=f, x=adi, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

#AEI
#ACI
ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=p, x=aei, colour=factor(thresh))) + 
  geom_smooth(aes(y=p, x=aei, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=r, x=aei, colour=factor(thresh))) + 
  geom_smooth(aes(y=r, x=aei, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=f, x=aei, colour=factor(thresh))) + 
  geom_smooth(aes(y=f, x=aei, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")
