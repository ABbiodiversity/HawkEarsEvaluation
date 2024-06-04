#This script produces figures for the HawkEars manuscript.

#PREAMBLE############

#1. Load libraries----
library(tidyverse)

#2. Set root file path---
root <- "G:/Shared drives/ABMI_Recognizers/HawkEars"

#3. Colours----
cols2 <- c("#4A7DA8", "#E39D22")
cols3 <- c("#4A7DA8", "#E39D22", "#0F9D58")

#4. Theme----

#FIGURE 4: PRF & RICHNESS####

#1. Get data----
prfrich <- read.csv(file.path(root, "Results", "ExpertData", "PRFRichness_Recording.csv"))

#2. Metric factors----
prfrich$metric <- factor(prfrich$metric, levels=c("precision", "recall", "fscore", "richness"),
                         labels=c("Precision", "Recall", "F1-score", "Species richness"))

#3. Plot----
ggplot(prfrich) + 
  geom_ribbon(aes(x=threshold, ymin = mn-std, ymax = mn+std, group=classifier), alpha = 0.1) +
  geom_line(aes(x=threshold, y=mn, colour=classifier)) + 
  facet_wrap(~metric, scales="free_y") + 
  scale_colour_manual(values=cols2, name="") +
  theme_classic() +
  ylab("Mean value per recording") + 
  xlab("Score threshold")

#4. Save----
ggsave(file.path(root, "Figures", "MS", "Figure4_PRFRichness.jpeg"), width=8, height = 5, units="in")

#FIGURE 5: FSCORE COMPARISON####

#1. Get data---
prfsp <- read.csv(file.path(root, "Results", "ExpertData", "PRF_Species.csv"))
outsp <- read.csv(file.path(root, "Results", "ExpertData", "Appendix2_PR.csv"))

#2. Wrangle to max fscore----
prf.maxf <- prfsp |> 
  group_by(species, classifier) |> 
  dplyr::filter(fscore == max(fscore, na.rm=TRUE)) |> 
  sample_n(1) |> 
  ungroup() |> 
  left_join(prf) |> 
  pivot_wider(names_from=classifier, values_from=precision:threshold) |> 
  mutate(fscore_delta = fscore_HawkEars - fscore_BirdNET) |> 
  left_join(outsp |> dplyr::select(species, hits))

#3. Plot----
ggplot(prf.maxf) + 
  geom_text(aes(x=fscore_BirdNET, y=fscore_HawkEars, label=species, colour=log(hits))) +
  geom_abline(aes(intercept=0, slope=1)) + 
  theme_classic() +
  scale_colour_viridis_c(name = "Log number of detections\nin evaluation dataset") +
  theme(legend.position = "bottom") +
  xlab("Maximum F1-score for BirdNET") +
  ylab("Maximum F1-score for HawkEars")

ggsave(file.path(root, "Figures", "MS", "Figure5_SpeciesFscore.jpeg"), width=8, height = 9, units="in")

#FIGURE 6: CALL RATE#####

#1. Get data----
prfrate <- read.csv(file.path(root, "Results", "SingleSpecies", "PRF.csv"))

#2. Plot----
ggplot(prfrate) + 
  geom_line(aes(x=threshold, y=f1score, colour=classifier)) + 
  facet_wrap(~species) + 
  scale_colour_manual(values=cols3, name="") +
  theme_classic() +
  ylab("F1-score") + 
  xlab("Score threshold")

ggsave(file.path(root, "Figures", "MS", "Figure6_CallRateFScore.jpeg"), width=12, height = 9, units="in")


#APPENDIX 2: SPECIES DETAILS COMMUNITY COMPOSITION######

#1. Get data----
outsp <- read.csv(file.path(root, "Results", "ExpertData", "PRF.csv"))
train <- read.csv(file.path(root, "Results", "HawkEars-training-record-counts.csv"))
raw <- read.csv(file.path(root, "Data", "Evaluation", "ExpertData.csv"))
prfsp <- read.csv(file.path(root, "Results", "ExpertData", "PRF_Species.csv"))

#2. Calculate number of recordings & minutes----
minutes <- raw |> 
  pivot_longer(ALFL:YRWA, names_to="species", values_to="abundance") |> 
  dplyr::filter(abundance > 0,
                minute <= 3) |> 
  group_by(species) |> 
  summarize(minutes=n()) |> 
  ungroup()

recordings <- raw |> 
  pivot_longer(ALFL:YRWA, names_to="species", values_to="abundance") |> 
  dplyr::filter(abundance > 0,
                minute <= 3) |> 
  group_by(species, recording_url) |>
  summarize(abundance = sum(abundance)) |> 
  group_by(species) |> 
  summarize(recordings=n()) |> 
  ungroup()

#3. Figure out which classifier is better----
prf.maxf <- prfsp |> 
  group_by(species, classifier) |> 
  dplyr::filter(fscore == max(fscore, na.rm=TRUE)) |> 
  sample_n(1) |> 
  ungroup() |> 
  left_join(prf) |> 
  pivot_wider(names_from=classifier, values_from=precision:threshold) |> 
  mutate(fscore_delta = fscore_HawkEars - fscore_BirdNET,
         better = ifelse(fscore_delta > 0 | is.na(fscore_delta), "HawkEars", "BirdNET"))

#4. Put together----
app2 <- train  |> 
  rename(species = Code) |> 
  left_join(outsp |> 
              dplyr::filter(hits > 0) |> 
              dplyr::select(-recordings, -hits)) |> 
  left_join(recordings) |> 
  left_join(minutes) |> 
  left_join(prf.maxf |> 
              dplyr::select(species, better)) |> 
  mutate(recordings = ifelse(is.na(recordings), 0, recordings),
         minutes = ifelse(is.na(minutes), 0, minutes),
         ROC = NA,
         MAP = NA) |> 
  dplyr::select(Name, species, Count, recordings, minutes, ROC, MAP, precision_universal, recall_universal, threshold_fscore, precision_fscore, recall_fscore, better)

#5. Fix column names----
colnames(app2) <- c("Common name", "Species code", "Number of training clips", "Number of evaluation recordings", "Number of evaluation detections (minutes)",  "ROC AUC", "MAP AUC", "Precision (universal threshold)", "Recall (universal threshold)", "F1-score threshold", "Precision (F1-score threshold", "Recall (F1-score threshold", "Classifier with highest F1-score")

#6. Save----
write.csv(app2, file.path(root, "Writing", "Appendix2.csv"), row.names = FALSE)

#APPENDIX 3: SPECIES DETAILS COMMUNITY COMPOSITION########


#SUMMARY STATS##########

#1. Expert dataset----
load("G:Shared drives/BAM_NationalModels/NationalModels5.0/Data/WildTrax/wildtrax_raw_2023-01-20.Rdata")

raw <- read.csv(file.path(root, "Data", "Evaluation", "ExpertData.csv"))

dat <- raw |> 
  dplyr::select(-c(ALFL:YRWA)) |> 
  mutate(datetime = ymd_hms(recording_date),
         hour = hour(datetime),
         doy = yday(datetime),
         month = month(datetime),
         year = year(datetime),
         tod = case_when(hour %in% 3:16 ~ "day",
                         hour %in% c(19:23, 0, 2) ~ "night"),
         toy = case_when(doy %in% c(86:142) ~ "early",
                         doy %in% c(143:223) ~ "songbird"))

sp <- raw |> 
  pivot_longer(ALFL:YRWA, names_to="species", values_to="abundance")

#time of day
table(dat$tod)

dat |> 
  group_by(tod) |> 
  summarize(n()/nrow(dat))

#day of year
dat |> 
  group_by(toy) |> 
  summarize(n()/nrow(dat))

#years
table(dat$year)

#number of species
length(unique(sp$species))

#recordings per species
sp.rec <- sp |> 
  dplyr::filter(abundance > 0) |> 
  group_by(species) |> 
  summarize(n=n()) |> 
  ungroup()

summary(sp.rec)

#2. Community performance----

#precision cross over
prfrich <- read.csv(file.path(root, "Results", "ExpertData", "PRFRichness_Recording.csv"))
prfsp <- read.csv(file.path(root, "Results", "ExpertData", "PRF_Species.csv"))
outsp <- read.csv(file.path(root, "Results", "ExpertData", "PRF.csv"))

prfrich |> 
  dplyr::select(-std) |> 
  dplyr::filter(metric=="precision") |> 
  pivot_wider(names_from=classifier, values_from=mn) |> 
  dplyr::filter(round(BirdNET, 1)==round(HawkEars, 1))

prfrich |> 
  dplyr::select(-std) |> 
  dplyr::filter(metric=="fscore") |> 
  pivot_wider(names_from=classifier, values_from=mn) |> 
  dplyr::filter(round(BirdNET, 1)==round(HawkEars, 1))

#metric at 0.7 threshold
prfrich |> 
  dplyr::filter(classifier=="HawkEars", threshold==0.7)

#max recall
head(prfrich |> dplyr::filter(classifier=="HawkEars", metric=="recall"))

#max precision
tail(prfrich |> dplyr::filter(classifier=="HawkEars", metric=="precision"))

#number of species with higher maxfscore
prf.maxf <- prfsp |> 
  group_by(species, classifier) |> 
  dplyr::filter(fscore == max(fscore, na.rm=TRUE)) |> 
  sample_n(1) |> 
  ungroup() |> 
  left_join(prf) |> 
  pivot_wider(names_from=classifier, values_from=precision:threshold) |> 
  mutate(fscore_delta = fscore_HawkEars - fscore_BirdNET,
         better = ifelse(fscore_delta > 0 | is.na(fscore_delta), "HawkEars", "BirdNET")) |> 
  left_join(outsp |> dplyr::select(species, hits))

table(prf.maxf$better)
