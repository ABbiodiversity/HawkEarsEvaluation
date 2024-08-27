#This script produces figures for the HawkEars manuscript.

#PREAMBLE############

#1. Load libraries----
library(tidyverse)
library(readxl)
library(sf)
library(terra)
library(ebirdst)
library(auk)
library(tidyterra)
library(gridExtra)
library(RColorBrewer)

#2. Set root file path---
root <- "G:/Shared drives/ABMI_Recognizers/HawkEars"

#3. Colours----
cols3 <- c("#355F89", "#E39D22", "#34A853")

#FIGURE 1: CANADA####

#1. List of North American species----
nabirds <- read.csv(file.path(root, "Other", "ABA_Checklist-8.14.csv"), skip=4) |> 
  data.table::setnames(c("family", "common_name", "french", "scientific_name", "species", "status")) |> 
  dplyr::select(-family, -french) |> 
  dplyr::filter(status %in% c(1, 2))

#2. Get the ebird codes----
codes <- get_ebird_taxonomy()

#3. Make to do list----
all <- left_join(nabirds, codes) |> 
  left_join(ebirdst_runs |> 
              mutate(available = 1))

#check
nrow(dplyr::filter(all, is.na(available)))

todo <- dplyr::filter(all, !is.na(available))

#5. Download the range files----
for(i in 1:nrow(todo)){
  
  ebirdst_download_status(species = todo$species_code[i],
                          download_abundance = FALSE,
                          download_ranges = TRUE,
                          force=TRUE)
}

#6. Make a raster of north america----
sa <- rast(xmin = -180, xmax = -50, ymin = 15, ymax = 90, nrow=1000, ncol=1000)

#7. Make a raster of all North America species----
for(i in 1:nrow(todo)){
  
  #8. Read in the shapefile----
  shp.i <- try(load_ranges(todo$species_code[i]) |> 
    dplyr::filter(season=="breeding"))
  
  if(inherits(shp.i, "try-error")){next}
  if(nrow(shp.i)==0){next}
  
  #9. Make it a raster & stack----
  r.i <- rasterize(vect(shp.i), sa)
  if(i==1){rast.out <- r.i} else { rast.out <- mosaic(rast.out, r.i, fun="sum")}
  
  cat("Finished species", i, "of", nrow(todo), "\n")
  
}

#10. Save the output raster----
writeRaster(rast.out, file.path(root, "Other", "ABA_Species_Mosaic.tif"), overwrite=TRUE)

rast.out <- rast(file.path(root, "Other", "ABA_Species_Mosaic.tif"))

#11. Get list of HawkEars spp----
spp <- read.csv(file.path(root, "Results", "HawkEars-training-record-counts.csv")) |> 
  rename(species = Code) |> 
  dplyr::select(species) |> 
  unique() |> 
  mutate(species = case_when(species =="AMGO" ~ "AGOL",
                          species=="NOGO" ~ "AGOS",
                          species=="PSFL" ~ "WEFL",
                          species=="BTGW" ~ "BTYW",
                          !is.na(species) ~ species)) 

#12. Make next todo----
todo.he <- todo |> 
  dplyr::filter(species %in% spp$species)

#13. Set up the loop for the hawkears raster----
for(i in 1:nrow(todo.he)){
  
  #14. Read in the shapefile----
  shp.i <- try(load_ranges(todo.he$species_code[i]) |> 
    dplyr::filter(season=="breeding"))
  
  if(inherits(shp.i, "try-error")){next}
  if(nrow(shp.i)==0){next}
  
  #15. Make it a raster & stack----
  r.i <- rasterize(vect(shp.i), sa)
  if(i==1){rast.he <- r.i} else { rast.he <- mosaic(rast.he, r.i, fun="sum")}

  cat("Finished species", i, "of", nrow(todo.he), "\n")
  
}

#16. Save the output raster----
writeRaster(rast.he, file.path(root, "Other", "HawkEars_Species_Mosaic.tif"), overwrite=TRUE)

rast.he <- rast(file.path(root, "Other", "HawkEars_Species_Mosaic.tif"))

#17. Crop to a shapefile----
country <- read_sf(file.path(root, "Other", "ne_110m_admin_0_countries", "ne_110m_admin_0_countries.shp")) |> 
  dplyr::filter(NAME %in% c("Canada", "United States of America"))

rast.plot.he <- rast.he |> 
  crop(vect(country), mask=TRUE)

rast.plot.nam <- rast.out |> 
  crop(vect(country), mask=TRUE)

#18. Divide total species by HE species----
rast.plot.prop <- rast.plot.he/rast.plot.nam

#19. Make plots----
plot.he <- ggplot() +   
  geom_spatraster(data=rast.plot.he, na.rm=TRUE) +
  scale_fill_viridis_c(na.value=NA, name = "Number of\nspecies") +
  theme_classic() +
  ylim(c(25, 85)) +
  theme(legend.position = "bottom")
plot.he

plot.prop <- ggplot() +   
  geom_spatraster(data=rast.plot.prop, na.rm=TRUE) +
  scale_fill_viridis_c(na.value=NA, name = "Proportion\nof species", option = "B") +
  theme_classic() +
  ylim(c(25, 85)) +
  theme(legend.position = "bottom")
plot.prop

ggsave(grid.arrange(plot.he, plot.prop, ncol=2), file = file.path(root, "Figures", "MS", "Figure1_StudyAreaSpecies.jpeg"), width = 8, height = 4)

#20. Species not in HawkEars----
missing <- anti_join(todo, spp) |> 
  dplyr::select(common_name:breeding_end)

pt <- st_as_sf(data.frame(x=-113, y=49), coords = c("x", "y"), crs = 4326)

missing$prairie <- NA
for(i in 1:nrow(missing)){
  
  #14. Read in the shapefile----
  shp.i <- try(load_ranges(missing$species_code[i]) |> 
                 dplyr::filter(season=="breeding"))
  
  if(nrow(shp.i)==0){next}
  
  int.i <- st_intersection(shp.i, pt)
  
  if(nrow(int.i)==0){missing$prairie[i] <- 0} else {missing$prairie[i] <- 1}
  
  cat("Finished species", i, "of", nrow(missing), "\n")
  
}

table(missing$prairie)

write.csv(missing, file.path(root, "Other", "MissingSpecies.csv"), row.names = FALSE)

#FIGURE 3: PRF & RICHNESS####

#1. Get data----
prfrich_raw <- read.csv(file.path(root, "Results", "ExpertData", "PRFRichness_Summary.csv")) 
prfrich <- read.csv(file.path(root, "Results", "ExpertData", "PRFRichness_Recording.csv"))

#2. Metric factors----
prfrich$metric <- factor(prfrich$metric, levels=c("precision", "recall", "fscore", "richness"),
                         labels=c("Precision", "Recall", "F1-score", "Species richness"))

prfrich_raw$metric <- factor(prfrich_raw$metric, levels=c("precision", "recall", "fscore", "richness"),
                         labels=c("Precision", "Recall", "F1-score", "Species richness"))

#3. Plot----
ggplot(prfrich) + 
  geom_line(data=prfrich_raw, aes(x=threshold, y=value, group=minute_id), alpha = 0.5, linewidth = 0.2, colour="grey70") +
  geom_line(aes(x=threshold, y=mn, colour=classifier), linewidth=1) + 
  facet_grid(metric ~ classifier, scales="free") + 
  scale_colour_manual(values=cols3, name="") +
  theme_classic() +
  ylab("Mean value per recording") + 
  xlab("Score threshold") +
  theme(legend.position = "bottom")

#4. Save----
ggsave(file.path(root, "Figures", "MS", "Figure3_PRFRichness.jpeg"), width=9, height = 10, units="in")

#BONUS FIGURE: FSCORE COMPARISON####

#1. Get data---
prfsp <- read.csv(file.path(root, "Results", "ExpertData", "PRF_Species.csv"))
outsp <- read.csv(file.path(root, "Results", "ExpertData", "PRF_Species_Summary.csv"))

#2. Wrangle to max fscore----
prf.maxf <- prfsp |> 
  group_by(species, classifier) |> 
  dplyr::filter(fscore == max(fscore, na.rm=TRUE)) |> 
  sample_n(1) |> 
  ungroup() |> 
  left_join(prfsp) |> 
  pivot_wider(names_from=classifier, values_from=precision:threshold) |> 
  mutate(fscore_delta = fscore_HawkEars - fscore_BirdNET) |> 
  left_join(outsp |>
              dplyr::select(species, hits))

#3. Plot----
ggplot(prf.maxf) + 
  geom_text(aes(x=fscore_BirdNET, y=fscore_HawkEars, label=species, colour=log(hits))) +
  geom_abline(aes(intercept=0, slope=1)) + 
  theme_classic() +
  scale_colour_viridis_c(name = "Log number of detections\nin evaluation dataset") +
  theme(legend.position = "right") +
  xlab("Maximum F1-score for BirdNET") +
  ylab("Maximum F1-score for HawkEars")

ggsave(file.path(root, "Figures", "MS", "FigureNA_SpeciesFscore.jpeg"), width=8, height = 5.5, units="in")

#FIGURE 4: CALL RATE#####

#1. Get data----
prfrate <- read.csv(file.path(root, "Results", "SingleSpecies", "PRF.csv")) |> 
  dplyr::filter(threshold >= 0.1)

species <- data.frame(species = unique(prfrate$species),
                      name = c("Barred owl", "Black-throated green warbler", "Common yellowthroat",
                               "Olive-sided flycatcher", "Ovenbird", "Ruffed grouse",
                               "Swainson's thrush", "Tennesee warbler", "White-throated sparrow"))

#2. Plot----
ggplot(prfrate |> left_join(species)) + 
  geom_line(aes(x=threshold, y=f1score, colour=classifier), linewidth = 1) + 
  facet_wrap(~name) + 
  scale_colour_manual(values=cols3, name="") +
  theme_classic() +
  ylab("F1-score") + 
  xlab("Score threshold") +
  theme(legend.position = "bottom") +
  ylim(c(0,1))

ggsave(file.path(root, "Figures", "MS", "Figure4_CallRateFScore.jpeg"), width=9, height = 10, units="in")

#APPENDIX 1: SPECIES DETAILS COMMUNITY COMPOSITION######

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
  group_by(species) |> 
  mutate(maxfscore=max(fscore, na.rm=TRUE)) |> 
  ungroup() |> 
  dplyr::filter(fscore==maxfscore) |> 
  group_by(species) |> 
  dplyr::filter(threshold == max(threshold)) |> 
  ungroup() |> 
  rename(better = classifier)

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
  dplyr::select(Name, species, Count, minutes, precision_universal, recall_universal, threshold_fscore, precision_fscore, recall_fscore, better)

#5. Get the vocal activity results----
prfrate <- read.csv(file.path(root, "Results", "SingleSpecies", "PRF.csv")) |> 
  dplyr::filter(threshold >= 0.1)

#6. Get metrics at 0.7----
prfrate_universal <- prfrate |> 
  dplyr::filter(threshold==0.7, classifier=="HawkEars") |> 
  dplyr::select(species, precision, recall) |> 
  rename(precision_universal_activity = precision, recall_universal_activity = recall)
  
#7. Get metrics at max Fscore----
prfrate_f1score <- prfrate |> 
  dplyr::filter(classifier=="HawkEars") |> 
  group_by(species) |> 
  summarize(f1score = max(f1score)) |> 
  ungroup() |> 
  left_join(prfrate) |> 
  dplyr::select(species, f1score, precision, recall) |> 
  rename(precision_f1score_activity = precision, recall_f1score_activity = recall, f1score_activity = f1score)

#8. Get the dataset details----
cr <- read.csv(file.path(root, "Data", "Evaluation", "SingleSpecies_all.csv")) |> 
  mutate(task_duration = round(task_duration)) |> 
  unique() |> 
  dplyr::filter(!target %in% c("Woodpeckers", "YERA")) 

#9. Summarize the dataset-----
cr.det <- cr |> 
  mutate(target = case_when(species_code=="WTSP" & target=="TEWA, WTSP" ~ "WTSP",
                            species_code=="TEWA" & target=="TEWA, WTSP" ~ "TEWA",
                            !is.na(target) ~ target)) |> 
  dplyr::filter(species_code!="NONE",
                !is.na(target),
                target!="TEWA, WTSP") |> 
  group_by(target) |> 
  summarize(detections = n()) |> 
  ungroup() |> 
  rename(species = target)

#8. Put together again----
app2b <- app2 |> 
  left_join(prfrate_universal) |> 
  left_join(prfrate_f1score) |> 
  left_join(cr.det)

#5. Fix column names----
colnames(app2b) <- c("Common name", "Species code", "Number of training clips", "Number of evaluation detections (minutes; community)", "Precision_universalthreshold (community)", "Recall _universalthreshold (community)", "F1score threshold (community)", "Precision_F1scorethreshold (community)", "Recall_F1scorethreshold (community)", "Classifier with highest F1score (community)", "Number of evaluation detections (calls; vocal activity)", "Precision_universalthreshold (vocal activity)", "Recall _universalthreshold (vocal activity)", "F1score threshold (vocal activity)", "Precision_F1scorethreshold (vocal activity)", "Recall_F1scorethreshold (vocal activity)")

#6. Save----
write.csv(app2b, file.path(root, "Writing", "Appendix1.csv"), row.names = FALSE)

#APPENDIX 3: HEURISTICS##########

heuristic_cols <- brewer.pal(n=8, name = "Dark2")[c(1,2,3,6)]

#1. Get data----
heur_raw <- read.csv(file.path(root, "Results", "ExpertData", "pr_curves_for_heuristics.csv"))

#2. Wrangle----
heur <- heur_raw |> 
  pivot_longer(base:all, names_to="heuristic", values_to="recall") |> 
  mutate(heuristic = factor(heuristic, levels = c("base", "choose", "overlap", "all"), labels = c("Base", "Channel selection", "Above plus\nwindow overlap", "Above plus\nspecies pool\nadjustment")))

#3. Plot----
ggplot(heur) + 
  geom_line(aes(x=precision, y=recall, colour=heuristic), linewidth = 1) + 
  theme_classic() +
  ylab("Recall") + 
  xlab("Precision") +
  theme(legend.position = "right") +
  scale_colour_manual(name = "", values = heuristic_cols) +
  ylim(c(0,1))

#4. Save----
ggsave(file.path(root, "Figures", "MS", "Appendix3_Heuristics.jpeg"), width = 7, height = 5)


#SUMMARY STATS##########

#1. Expert dataset----
load("G:Shared drives/BAM_NationalModels5/Data/WildTrax/wildtrax_raw_2023-01-20.Rdata")

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

#recording minutes per species
sp.rec <- sp |> 
  dplyr::filter(abundance > 0) |> 
  group_by(species) |> 
  summarize(n=n()) |> 
  ungroup()

summary(sp.rec)

#2. Call rate dataset----
cr <- read.csv(file.path(root, "Data", "Evaluation", "SingleSpecies_all.csv")) |> 
  dplyr::select(project_id, task_id, recording_date_time, task_duration, target) |> 
  mutate(task_duration = round(task_duration)) |> 
  unique() |> 
  dplyr::filter(!target %in% c("Woodpeckers", "YERA")) |> 
  mutate(datetime = ymd_hms(recording_date_time),
         hour = hour(datetime),
         doy = yday(datetime),
         month = month(datetime),
         year = year(datetime),
         tod = case_when(hour %in% 3:16 ~ "day",
                         hour %in% c(19:23, 0, 2) ~ "night"),
         toy = case_when(doy %in% c(86:142) ~ "early",
                         doy %in% c(143:223) ~ "songbird"))

#summarize
cr.total <- cr |> 
  group_by(target) |> 
  summarize(n=n(),
            total_duration = sum(task_duration)/60,
            mean_duration = mean(task_duration)/60) |> 
  ungroup()

#duration per species
cr.total
summary(cr.total)

#mean recording duration
table(cr$task_duration)

#Number of recordings
nrow(cr)

#time of day
cr |> 
  group_by(tod, target) |> 
  summarize(n=n()) |> 
  group_by(target) |> 
  mutate(n/sum(n))

ggplot(cr) +
  geom_histogram(aes(x=hour)) +
  facet_wrap(~target, scales="free_y")

#day of year
dat |> 
  group_by(toy) |> 
  summarize(n()/nrow(dat))

#years
table(dat$year)

#3. Community performance----

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
  dplyr::filter(metric=="precision") |> 
  pivot_wider(names_from=classifier, values_from=mn) |> 
  dplyr::filter(round(Perch, 1)==round(HawkEars, 1))

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

#max fscore
fscore <- prfrich |> 
  dplyr::filter(metric=="fscore") |> 
  group_by(classifier) |> 
  dplyr::filter(mn == max(mn)) |> 
  ungroup()

#species richness at highest fscore
fscore |> 
  dplyr::select(classifier, threshold) |> 
  left_join(prfrich, multiple="all")

#species richness at precision of 0.7 threshold
prfrich |> 
  dplyr::filter(round(mn, 2)==0.9,
                metric=="precision") |> 
  group_by(classifier) |> 
  dplyr::filter(threshold==min(threshold)) |> 
  ungroup() |> 
  dplyr::select(classifier, threshold) |> 
  left_join(prfrich)

#number of species with higher maxfscore
prf.maxf <- prfsp |> 
  group_by(species) |> 
  mutate(maxfscore=max(fscore, na.rm=TRUE)) |> 
  ungroup() |> 
  dplyr::filter(fscore==maxfscore) |> 
  group_by(species) |> 
  dplyr::filter(threshold == max(threshold)) |> 
  ungroup()

table(prf.maxf$species, prf.maxf$classifier)
table(prf.maxf$classifier)
table(prf.maxf$species)

#4. Call rate performance----

#max f1 score
prfrate |> 
  group_by(species, classifier) |> 
  summarize(f1score = max(f1score)) |> 
  group_by(species) |> 
  summarize(diff = max(f1score) - min(f1score)) |> 
  arrange(diff)

#max precision
prfrate |> 
  group_by(species, classifier) |> 
  summarize(precision = max(precision))

#recall at 90% precision
prfrate |> 
  mutate(diff = abs(0.9 - precision)) |> 
  group_by(species, classifier) |> 
  dplyr::filter(diff == min(diff)) |> 
  sample_n(1) |> 
  ungroup() |> 
  group_by(classifier) |> 
  summarize(recallmn = mean(recall),
            recallsd = sd(recall))

