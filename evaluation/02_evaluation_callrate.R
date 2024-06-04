#This script evaluates raw hawkears, birdnet, and perch detections for the call rate dataset.

#PREAMBLE############

#1. Load libraries----
library(tidyverse)

#2. Set root file path---
root <- "G:/Shared drives/ABMI_Recognizers/HawkEars"

#PUT JAN'S FILES TOGETHER###########

#1. Get list of files----
files <- data.frame(path = list.files(file.path(root, "Results", "SingleSpecies"), pattern="*.csv", full.names = TRUE),
                    file = list.files(file.path(root, "Results", "SingleSpecies"), pattern="*.csv")) |> 
  separate(file, into=c("species", "classifier", "p", "r", "filetype"), remove=FALSE)

#2. Read them in----
dat.list <- list()
for(i in 1:nrow(files)){
  
  dat.list[[i]] <- read.csv(files$path[i]) |> 
    mutate(species = files$species[i],
           classifier = files$classifier[i])
  
  
}

dat <- do.call(rbind, dat.list)
colnames(dat) <- c("threshold", "precision", "recall", "f1score", "f5score", "f25score", "species", "classifier")

write.csv(dat, file.path(root, "Results", "SingleSpecies", "PRF.csv"), row.names = FALSE)

#3. Look around----

ggplot(dat) + 
  geom_line(aes(x=threshold, y=f1score, colour=classifier)) +
  facet_wrap(~species)
