#This script looks at species-specific effects on recall and precision of hawkears and birdnet detections relative to the expert dataset.

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

#4. Get species covariate dataset----
trait <- read.csv("G:/Shared drives/ABMI_Recognizers/HawkEars/Results/speciestraits.csv")

#5. Get training sample size dataset----
train <- read.csv("G:/Shared drives/ABMI_Recognizers/HawkEars/Results/HawkEars-training-record-counts.csv")

#WRANGLE############

#1. Get the species-specific performance----
pr.sp <- read.csv(file.path(root, "Results", "ExpertData", "ExpertData_PR_Species.csv"))

#2. Add the covariates----
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
plot.p.n <- ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8),
                                         classifier=="HawkEars")) + 
  geom_point(aes(x=train, y=p, colour=factor(thresh))) + 
  geom_smooth(aes(x=train, y=p, colour=factor(thresh)), method="gam") +
  facet_wrap(~thresh) +
  xlab("Precision per species") +
  ylab("Training dataset size") +
  scale_colour_discrete(name = "Threshold")
plot.p.n

plot.r.n <- ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8),
                             classifier=="HawkEars")) + 
  geom_point(aes(x=train, y=r, colour=factor(thresh))) + 
  geom_smooth(aes(x=train, y=r, colour=factor(thresh)), method="gam") +
  facet_wrap(~thresh) +
  xlab("Recall per species") +
  ylab("Training dataset size") +
  scale_colour_discrete(name = "Threshold")
plot.r.n

ggsave(grid.arrange(plot.p.n, plot.r.n, ncol=1), filename=file.path(root, "Figures", "ExpertData_TrainingSize.jpeg"),
       width = 10, height = 16)

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8),
                             classifier=="HawkEars")) + 
  geom_point(aes(x=train, y=f, colour=factor(thresh))) + 
  geom_smooth(aes(x=train, y=f, colour=factor(thresh)),  method="lm") +
  facet_grid(thresh ~ .)

#Relationship with recall

#2. Vocalization frequency----

#Mid frequency
ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=p, x=MidFreq.khz, colour=factor(thresh))) + 
  geom_smooth(aes(y=p, x=MidFreq.khz, colour=factor(thresh)), method="gam") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=r, x=MidFreq.khz, colour=factor(thresh))) + 
  geom_smooth(aes(y=r, x=MidFreq.khz, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=f, x=MidFreq.khz, colour=factor(thresh))) + 
  geom_smooth(aes(y=f, x=MidFreq.khz, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

#Max frequency
plot.f.p <- ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=p, x=MaxFreq.khz, colour=factor(thresh))) + 
  geom_smooth(aes(y=p, x=MaxFreq.khz, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free") +
  xlab("Precision per species") +
  ylab("Maximum vocalization frequency (kHz)") +
  scale_colour_discrete(name = "Threshold")
plot.f.p

plot.f.r <- ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=r, x=MaxFreq.khz, colour=factor(thresh))) + 
  geom_smooth(aes(y=r, x=MaxFreq.khz, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free") +
  xlab("Recall per species") +
  ylab("Maximum vocalization frequency (kHz)") +
  scale_colour_discrete(name = "Threshold")
plot.f.r

ggsave(grid.arrange(plot.f.p, plot.f.r, ncol=1), filename=file.path(root, "Figures", "ExpertData_VocalizationFrequency.jpeg"),
       width = 16, height = 16)

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=f, x=MaxFreq.khz, colour=factor(thresh))) + 
  geom_smooth(aes(y=f, x=MaxFreq.khz, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

#Min frequency
ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=p, x=MinFreq.khz, colour=factor(thresh))) + 
  geom_smooth(aes(y=p, x=MinFreq.khz, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=r, x=MinFreq.khz, colour=factor(thresh))) + 
  geom_smooth(aes(y=r, x=MinFreq.khz, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=f, x=MinFreq.khz, colour=factor(thresh))) + 
  geom_smooth(aes(y=f, x=MinFreq.khz, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

#3. Mass-----
ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8),
                             Weight.g < 100)) + 
  geom_point(aes(y=p, x=Weight.g, colour=factor(thresh))) + 
  geom_smooth(aes(y=p, x=Weight.g, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8),
                             Weight.g < 100)) + 
  geom_point(aes(y=r, x=Weight.g, colour=factor(thresh))) + 
  geom_smooth(aes(y=r, x=Weight.g, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8),
                             Weight.g < 100)) + 
  geom_point(aes(y=f, x=Weight.g, colour=factor(thresh))) + 
  geom_smooth(aes(y=f, x=Weight.g, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

#positive effect of mass on precision for birdnet only

#4. Song duration----

#Mid duration
ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=p, x=SongDurationMid.sec, colour=factor(thresh))) + 
  geom_smooth(aes(y=p, x=SongDurationMid.sec, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=r, x=SongDurationMid.sec, colour=factor(thresh))) + 
  geom_smooth(aes(y=r, x=SongDurationMid.sec, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=f, x=SongDurationMid.sec, colour=factor(thresh))) + 
  geom_smooth(aes(y=f, x=SongDurationMid.sec, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

#Max duration
plot.s.p <- ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=p, x=SongDurationMax.sec, colour=factor(thresh))) + 
  geom_smooth(aes(y=p, x=SongDurationMax.sec, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free") +
  xlab("Precision per species") +
  ylab("Maximum song duration (sec)") +
  scale_colour_discrete(name = "Threshold")

plot.s.r <- ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=r, x=SongDurationMax.sec, colour=factor(thresh))) + 
  geom_smooth(aes(y=r, x=SongDurationMax.sec, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free") +
  xlab("Recall per species") +
  ylab("Maximum song duration (sec)") +
  scale_colour_discrete(name = "Threshold")

ggsave(grid.arrange(plot.s.p, plot.s.r, ncol=1), filename=file.path(root, "Figures", "ExpertData_VocalizationDuration.jpeg"),
       width = 16, height = 16)

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=f, x=SongDurationMax.sec, colour=factor(thresh))) + 
  geom_smooth(aes(y=f, x=SongDurationMax.sec, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

#Min duration
ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=p, x=SongDurationMin.sec, colour=factor(thresh))) + 
  geom_smooth(aes(y=p, x=SongDurationMin.sec, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=r, x=SongDurationMin.sec, colour=factor(thresh))) + 
  geom_smooth(aes(y=r, x=SongDurationMin.sec, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=f, x=SongDurationMin.sec, colour=factor(thresh))) + 
  geom_smooth(aes(y=f, x=SongDurationMin.sec, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

#longer songs have lower precision

#5. Migration type-----
plot.m.p <- ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8),
                             !is.na(MigType))) +
  geom_density_ridges(aes(x=p, y=factor(thresh), fill=MigType), alpha = 0.5) +
  xlab("Precision per species") +
  ylab("Threshold") +
  scale_fill_discrete(name = "Migration type", labels=c("Long distance",
                                                          "Short distance",
                                                          "Resident"))
plot.m.p

plot.m.r <- ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8),
                             !is.na(MigType))) +
  geom_density_ridges(aes(x=r, y=factor(thresh), fill=MigType), alpha = 0.5) +
  xlab("Recall per species") +
  ylab("Threshold") +
  scale_fill_discrete(name = "Migration type", labels=c("Long distance",
                                                        "Short distance",
                                                        "Resident"))
plot.m.r

ggsave(grid.arrange(plot.m.p, plot.m.r, ncol=1), filename=file.path(root, "Figures", "ExpertData_MigrationType.jpeg"),
       width = 10, height = 16)

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) +
  geom_boxplot(aes(x=factor(thresh), y=f, colour=MigType))

#Residents seem to have lower overall performance????

#6. EDR----
plot.e.p <- ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=p, x=log.tau0, colour=factor(thresh))) + 
  geom_smooth(aes(y=p, x=log.tau0, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free") +
  xlab("Precision per species") +
  ylab("Effective detection radius (100 m)") +
  scale_colour_discrete(name = "Threshold")

plot.e.r <- ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=r, x=log.tau0, colour=factor(thresh))) + 
  geom_smooth(aes(y=r, x=log.tau0, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free") +
  xlab("Recall per species") +
  ylab("Effective detection radius (100 m)") +
  scale_colour_discrete(name = "Threshold")

ggsave(grid.arrange(plot.e.p, plot.e.r, ncol=1), filename=file.path(root, "Figures", "ExpertData_DetectionRadius.jpeg"),
       width = 16, height = 16)

ggplot(dat %>% dplyr::filter(thresh %in% c(0.2, 0.4, 0.6, 0.8))) + 
  geom_point(aes(y=f, x=log.tau0, colour=factor(thresh))) + 
  geom_smooth(aes(y=f, x=log.tau0, colour=factor(thresh)), method="lm") +
  facet_grid(thresh ~ classifier, scales="free")

#Affects precision

#MODEL BOTH CLASSIFIERS AT THE SAME TIME##########

#1. Effect of sample size----

#Recall
dat.n.p <- dplyr::filter(dat, !is.na(p), classifier=="HawkEars", !is.na(train))

m1 <- lmer(p ~ poly(train, 2)*thresh + (1|species), data=dat.n.p, na.action = "na.fail", REML = FALSE)
summary(m1)
dredge(m1)

dat.n.r <- dplyr::filter(dat, !is.na(r), classifier=="HawkEars", !is.na(train))

m1 <- lmer(r ~ poly(train, 2)*thresh + (1|species), data=dat.n.r, na.action = "na.fail", REML = FALSE)
summary(m1)
dredge(m1)

#2. Effect of vocalization frequency----

#precision
dat.f.p <- dplyr::filter(dat, !is.na(MinFreq.khz), !is.na(p))
m2 <- lmer(p ~ MidFreq.khz*thresh + MidFreq.khz*classifier + (1|species), data=dat.f.p, na.action="na.fail", REML = FALSE)
m3 <- lmer(p ~ MinFreq.khz*thresh + MinFreq.khz*classifier + (1|species), data=dat.f.p, na.action="na.fail", REML = FALSE)
m4 <- lmer(p ~ MaxFreq.khz*thresh + MaxFreq.khz*classifier + (1|species), data=dat.f.p, na.action="na.fail", REML = FALSE)
AIC(m2, m3, m4) #Use max
dredge(m4) #Global model

#recall
dat.f.r <- dplyr::filter(dat, !is.na(MinFreq.khz), !is.na(r))
m5 <- lmer(r ~ MidFreq.khz*thresh + MidFreq.khz*classifier + (1|species), data=dat.f.r, na.action="na.fail", REML = FALSE)
m6 <- lmer(r ~ MinFreq.khz*thresh + MinFreq.khz*classifier + (1|species), data=dat.f.r, na.action="na.fail", REML = FALSE)
m7 <- lmer(r ~ MaxFreq.khz*thresh + MaxFreq.khz*classifier + (1|species), data=dat.f.r, na.action="na.fail", REML = FALSE)
AIC(m5, m6, m7) #Use max again
dredge(m7) #Linear effect, no interactions

#3. Effect of song duration----

#precision
dat.s.p <- dplyr::filter(dat, !is.na(SongDurationMid.sec), !is.na(p))
m8 <- lmer(p ~ SongDurationMin.sec*thresh + SongDurationMin.sec*classifier + (1|species), data=dat.s.p, na.action="na.fail", REML = FALSE)
m9 <- lmer(p ~ SongDurationMid.sec*thresh + SongDurationMid.sec*classifier + (1|species), data=dat.s.p, na.action="na.fail", REML = FALSE)
m10 <- lmer(p ~ SongDurationMax.sec*thresh + SongDurationMax.sec*classifier + (1|species), data=dat.s.p, na.action="na.fail", REML = FALSE)
AIC(m8, m9, m10) #Use max
dredge(m10) #Global model

#recall
dat.s.r <- dplyr::filter(dat, !is.na(SongDurationMid.sec), !is.na(r))
m11 <- lmer(r ~ SongDurationMin.sec*thresh + SongDurationMin.sec*classifier + (1|species), data=dat.s.r, na.action="na.fail", REML = FALSE)
m12 <- lmer(r ~ SongDurationMid.sec*thresh + SongDurationMid.sec*classifier + (1|species), data=dat.s.r, na.action="na.fail", REML = FALSE)
m13 <- lmer(r ~ SongDurationMax.sec*thresh + SongDurationMax.sec*classifier + (1|species), data=dat.s.r, na.action="na.fail", REML = FALSE)
AIC(m11, m12, m13) #Use min
dredge(m11) #Global model

#4. Migration type----

#precision
dat.m.p <- dplyr::filter(dat, !is.na(MigType), !is.na(p))
m14 <- lmer(p ~ MigType*thresh + MigType*classifier + (1|species), data=dat.m.p, na.action="na.fail", REML = FALSE)
dredge(m14)

#recall
dat.m.r <- dplyr::filter(dat, !is.na(MigType), !is.na(r))
m15 <- lmer(r ~ MigType*thresh + MigType*classifier + (1|species), data=dat.m.r, na.action="na.fail", REML = FALSE)
dredge(m15)

#5. EDR----

#Precision
dat.e.p <- dplyr::filter(dat, !is.na(log.tau0), !is.na(p))
m18 <- lmer(p ~ log.tau0*thresh + log.tau0*classifier + (1|species), data=dat.e.p, na.action="na.fail", REML = FALSE)
dredge(m18)

#Recall
dat.e.r <- dplyr::filter(dat, !is.na(log.tau0), !is.na(r))
m19 <- lmer(r ~ log.tau0*thresh + log.tau0*classifier + (1|species), data=dat.e.r, na.action="na.fail", REML = FALSE)
dredge(m19)

#MODEL HAWKEARS####

#1. Subset data----
dat.he.p <- dplyr::filter(dat, classifier=="HawkEars",
                          !is.na(train),
                          !is.na(log.tau0),
                          !is.na(SongDurationMin.sec),
                          !is.na(p))

dat.he.r <- dplyr::filter(dat, classifier=="HawkEars",
                          !is.na(train),
                          !is.na(log.tau0),
                          !is.na(SongDurationMin.sec),
                          !is.na(r))

#2. Look for correlation----
covs.p <- dat.he.p %>% 
  dplyr::select(train, log.tau0, SongDurationMin.sec, MaxFreq.khz)
usdm::vif(covs.p)
cor(covs.p)

covs.r <- dat.he.r %>% 
  dplyr::select(train, log.tau0, SongDurationMin.sec, MaxFreq.khz)
usdm::vif(covs.r)
cor(covs.r)

#3. Model----
m.he.p <- lmer(p ~ train*thresh + 
                 MaxFreq.khz*thresh + 
                 SongDurationMax.sec*thresh +
                 MigType*thresh +
                 log.tau0*thresh +
                 (1|species),
               data=dat.he.p,
               na.action="na.fail",
               REML=FALSE)
dredge(m.he.p)

m.he.r <- lmer(r ~ train*thresh + 
                 MaxFreq.khz*thresh + 
                 SongDurationMin.sec*thresh +
                 MigType*thresh +
                 log.tau0*thresh +
                 (1|species),
               data=dat.he.r,
               na.action="na.fail",
               REML=FALSE)
dredge(m.he.r)

#MODEL BIRDNET####

#1. Subset data----
dat.bn.p <- dplyr::filter(dat, classifier=="BirdNET",
                          !is.na(log.tau0),
                          !is.na(SongDurationMin.sec),
                          !is.na(p))

dat.bn.r <- dplyr::filter(dat, classifier=="BirdNET",
                          !is.na(log.tau0),
                          !is.na(SongDurationMin.sec),
                          !is.na(r))

#2. Look for correlation----
covs.p <- dat.bn.p %>% 
  dplyr::select(log.tau0, SongDurationMin.sec, MaxFreq.khz)
usdm::vif(covs.p)
cor(covs.p)

covs.r <- dat.bn.r %>% 
  dplyr::select(log.tau0, SongDurationMin.sec, MaxFreq.khz)
usdm::vif(covs.r)
cor(covs.r)

#3. Model----
m.bn.p <- lmer(p ~ MaxFreq.khz*thresh + 
                 SongDurationMax.sec*thresh +
                 MigType*thresh +
                 log.tau0*thresh +
                 (1|species),
               data=dat.bn.p,
               na.action="na.fail",
               REML=FALSE)
dredge(m.bn.p)

m.bn.r <- lmer(r ~ MaxFreq.khz*thresh + 
                 SongDurationMin.sec*thresh +
                 MigType*thresh +
                 log.tau0*thresh +
                 (1|species),
               data=dat.bn.r,
               na.action="na.fail",
               REML=FALSE)
dredge(m.bn.r)