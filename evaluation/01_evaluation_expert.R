#This script evaluates raw hawkears, birdnet, and perch detections for the expert dataset.

#PREAMBLE############

#1. Load libraries----
library(tidyverse)

#2. Set root file path---
root <- "G:/Shared drives/ABMI_Recognizers/HawkEars"

#3. Load data----
min <- read.csv(file.path(root, "Results", "ExpertData", "ExpertData_ByMinute.csv")) |> 
  mutate(minute_id = paste0(recording_id, "_", minute))

#4. Write functions----
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
  data_richness <- dplyr::filter(data, confidence >= threshold, detection==1) |> 
    dplyr::select(species) |> 
    unique() |> 
    summarize(richness = n()) |> 
    mutate(threshold = threshold)
  
}

#5. Set threshold sequence----
threshold <- seq(0.1, 0.99, 0.01)

#OVERALL METRICS##############

#1. Wrangle----
dat.he <- min |> 
  mutate(tp = ifelse(!is.na(HawkEars) & detection==1, 1, 0),
         fp = ifelse(!is.na(HawkEars) & detection==0, 1, 0),
         fn = ifelse(is.na(HawkEars) & detection==1, 1, 0)) |> 
  dplyr::select(-BirdNET, -Perch) |> 
  rename(confidence = HawkEars)

dat.bn <- min |> 
  mutate(tp = ifelse(!is.na(BirdNET) & detection==1, 1, 0),
         fp = ifelse(!is.na(BirdNET) & detection==0, 1, 0),
         fn = ifelse(is.na(BirdNET) & detection==1, 1, 0)) |> 
  dplyr::select(-HawkEars, -Perch) |> 
  rename(confidence = BirdNET)

dat.pr <- min |> 
  mutate(tp = ifelse(!is.na(Perch) & detection==1, 1, 0),
         fp = ifelse(!is.na(Perch) & detection==0, 1, 0),
         fn = ifelse(is.na(Perch) & detection==1, 1, 0)) |> 
  dplyr::select(-HawkEars, -BirdNET) |> 
  rename(confidence = Perch)

#2. Total detections----
human_total <- sum(min$detection)

#3. Overall metrics----
overall.he <- do.call(rbind, lapply(X=threshold, FUN=calculate_prf, data=dat.he, human_total = human_total)) |> 
  mutate(classifier="HawkEars")
overall.bn <- do.call(rbind, lapply(X=threshold, FUN=calculate_prf, data=dat.bn, human_total = human_total)) |> 
  mutate(classifier = "BirdNET")
overall.pr <- do.call(rbind, lapply(X=threshold, FUN=calculate_prf, data=dat.pr, human_total = human_total)) |> 
  mutate(classifier = "Perch")

overall <- rbind(overall.he, overall.bn, overall.pr) |> 
  pivot_longer(precision:fscore, names_to="metric", values_to="value")

#4. Plot----
ggplot(overall) +
  geom_line(aes(x=threshold, y=value, colour=classifier)) +
  facet_wrap(~metric, scales="free")

#5. Check PR curve----
pr <- overall |> 
  dplyr::filter(metric %in% c("precision", "recall")) |> 
  pivot_wider(names_from=metric, values_from = value)

ggplot(pr) +
  geom_line(aes(x=precision, y=recall, colour=classifier))

#PER RECORDING MINUTE#######

#1. Set up recording loop----
mins <- unique(dat.he$minute_id)

min.list <- list()
for(i in 1:length(mins)){
  
  #2. Subset data----
  dat.he.i <- dat.he |> 
    dplyr::filter(minute_id==mins[i])
  dat.bn.i <- dat.bn |> 
    dplyr::filter(minute_id==mins[i])
  dat.pr.i <- dat.pr |> 
    dplyr::filter(minute_id==mins[i])
  
  #3. Get total expert detections----
  human_total = sum(dat.he.i$detection)
  
  #4. Get PRF----
  prf.he <-  do.call(rbind, lapply(X=threshold, FUN=calculate_prf, data=dat.he.i, human_total=human_total))
  prf.bn <-  do.call(rbind, lapply(X=threshold, FUN=calculate_prf, data=dat.bn.i, human_total=human_total))
  prf.pr <-  do.call(rbind, lapply(X=threshold, FUN=calculate_prf, data=dat.pr.i, human_total=human_total))
  
  #5. Get species richness----
  rich.he <- do.call(rbind, lapply(X=threshold, FUN=calculate_richness, data=dat.he.i))
  rich.bn <- do.call(rbind, lapply(X=threshold, FUN=calculate_richness, data=dat.bn.i))
  rich.pr <- do.call(rbind, lapply(X=threshold, FUN=calculate_richness, data=dat.pr.i))
  
  #Get total richness----
  rich.human <- dat.he.i |> 
    dplyr::filter(detection==1) |> 
    dplyr::select(species) |> 
    unique() |> 
    nrow()
  
  #6. Wrangle----
  min.list[[i]] <- inner_join(prf.he, rich.he) |> 
    mutate(classifier = "HawkEars") |> 
    rbind(inner_join(prf.bn, rich.bn) |> 
            mutate(classifier = "BirdNET")) |> 
    rbind(inner_join(prf.pr, rich.pr) |> 
            mutate(classifier = "Perch")) |> 
    pivot_longer(c(precision, recall, fscore, richness), names_to="metric", values_to="value") |> 
    mutate(minute_id = mins[i],
           total_richness = rich.human)
  
  cat("Finished recording", i, "of", length(mins), "\n")
  
}

#7. Put together----
out <- do.call(rbind, min.list)

#8. Summarize----
out.min <- out |> 
  group_by(threshold, classifier, metric) |> 
  summarize(mn = mean(value, na.rm = TRUE),
            std = sd(value, na.rm = TRUE)) |> 
  ungroup()

#9. Plot----
ggplot(out.min) + 
  geom_ribbon(aes(x=threshold, ymin = mn-std, ymax = mn+std, group=classifier), alpha = 0.3) +
  geom_line(aes(x=threshold, y=mn, colour=classifier)) + 
  facet_wrap(~metric, scales="free")

#10. Save-----
write.csv(out, file.path(root, "Results", "ExpertData", "PRFRichness_Summary.csv"), row.names = FALSE)
write.csv(out.min, file.path(root, "Results", "ExpertData", "PRFRichness_Recording.csv"), row.names = FALSE)

#PER SPECIES#########

#1. Set up species loop----
sp <- min |> 
  dplyr::select(species) |> 
  unique()

out.list <- list()
prf.list <- list()
for(i in 1:nrow(sp)){
  
  #2. Wrangle----
  dat.he <- min |> 
    dplyr::filter(species==sp$species[i]) |> 
    mutate(tp = ifelse(!is.na(HawkEars) & detection==1, 1, 0),
           fp = ifelse(!is.na(HawkEars) & detection==0, 1, 0),
           fn = ifelse(is.na(HawkEars) & detection==1, 1, 0)) |> 
    dplyr::select(-BirdNET, -Perch) |> 
    rename(confidence = HawkEars)
  
  dat.bn <- min |> 
    dplyr::filter(species==sp$species[i]) |> 
    mutate(tp = ifelse(!is.na(BirdNET) & detection==1, 1, 0),
           fp = ifelse(!is.na(BirdNET) & detection==0, 1, 0),
           fn = ifelse(is.na(BirdNET) & detection==1, 1, 0)) |> 
    dplyr::select(-HawkEars, -Perch) |> 
    rename(confidence = BirdNET)
  
  dat.pr <- min |> 
    dplyr::filter(species==sp$species[i]) |> 
    mutate(tp = ifelse(!is.na(Perch) & detection==1, 1, 0),
           fp = ifelse(!is.na(Perch) & detection==0, 1, 0),
           fn = ifelse(is.na(Perch) & detection==1, 1, 0)) |> 
    dplyr::select(-HawkEars, -BirdNET) |> 
    rename(confidence = Perch)
  
  #3. Get total expert detections----
  human_total = sum(dat.he$detection)
  
  #4. Apply function----
  prf <- do.call(rbind, lapply(X=threshold, FUN=calculate_prf, data=dat.he, human_total=human_total)) 
  #5. Get F-score threshold----
  fscore <- arrange(prf, -fscore)[1,] |> 
    mutate(thresh = "fscore")
  
  #6. Get universal threshold----
  universe <- prf[prf$threshold==0.7,] |> 
    mutate(thresh = "universal")
  
  #7. Get number of minutes----
  minutes <- dat.he |> 
    dplyr::filter(detection==1) |> 
    dplyr::select(minute_id) |> 
    unique() |> 
    summarize(minutes = n())
  
  #8. Put together----
  out.list[[i]] <- rbind(fscore, universe) |> 
    dplyr::select(-fscore) |> 
    pivot_wider(names_from=thresh, values_from=precision:threshold) |> 
    dplyr::select(-threshold_universal) |> 
    mutate(species = sp$species[i],
           minutes = minutes$minutes,
           hits = human_total) |> 
    dplyr::select(species, minutes, hits, precision_universal, recall_universal, threshold_fscore, precision_fscore, recall_fscore)
  
  #9. Apply function to birdnet----
  prf.bn <- do.call(rbind, lapply(X=threshold, FUN=calculate_prf, data=dat.bn, human_total=human_total))
  
  #10. Apply function to birdnet----
  prf.pr <- do.call(rbind, lapply(X=threshold, FUN=calculate_prf, data=dat.pr, human_total=human_total))
  
  #11. Put together----
  prf.list[[i]] <- prf |> 
    mutate(classifier="HawkEars") |> 
    rbind(prf.bn |> 
            mutate(classifier="BirdNET")) |> 
    rbind(prf.pr |> 
            mutate(classifier="Perch")) |> 
    mutate(species = sp$species[i])
  
  cat("Finished species", sp$species[i], ":", i, "of", nrow(sp), "\n")
  
}

#12. Save----
out.sp <- do.call(rbind, out.list) |> 
  arrange(species)

prf <- do.call(rbind, prf.list)

write.csv(out.sp, file.path(root, "Results", "ExpertData", "PRF_Species_Summary.csv"), row.names = FALSE)
write.csv(prf, file.path(root, "Results", "ExpertData", "PRF_Species.csv"), row.names = FALSE)

#13. Interrogate----
prf <- read.csv(file.path(root, "Results", "ExpertData", "PRF_Species.csv"))
prf.maxf <- prf |> 
  group_by(species, classifier) |> 
  dplyr::filter(fscore == max(fscore, na.rm=TRUE)) |> 
  sample_n(1) |> 
  ungroup() |> 
  left_join(prf) |> 
  pivot_wider(names_from=classifier, values_from=precision:threshold) |> 
  mutate(fscore_delta = fscore_HawkEars - fscore_BirdNET)

ggplot(prf.maxf) + 
  geom_point(aes(x=fscore_HawkEars, y=fscore_BirdNET)) +
  geom_abline(aes(intercept=0, slope=1))
