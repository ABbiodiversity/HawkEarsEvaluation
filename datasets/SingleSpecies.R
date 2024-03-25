library(wildRtrax)
library(tidyverse)
library(readxl)
library(lubridate)

#PREAMBLE############

#1. login----
config <- "WTlogin.R"
source(config)
wt_auth()

#2. Get project list----
project.list <- wt_get_download_summary(sensor_id = 'ARU')

#3. Set root file path---
root <- "G:/Shared drives/ABMI_Recognizers/HawkEars"

#4. Get dataframe with AWS urls in it----
#This file is obtained with the getReports/DownloadClipView.R script in the recognizersandbox repo.

load("G:/My Drive/ABMI/Projects/Recognizers/Data/clip_view_2024-03-25.Rdata")

#SINGLE SPECIES####

#1. Data frame of target projects----
sp <- data.frame(project_id=c(498, 669, 760, 777, 1157, 1238, 623, 787, 791, 2151, 1950, 680, 1321, 678, 2418),
                 target=c("BTNW", "OVEN", "SWTH", "BTNW", "BADO", "SWTH", "OVEN", "OSFL", "BTNW", "YERA", "TEWA, WTSP", "BTNW", "RUGR", "Woodpeckers", "COYE"))

#2. Download all the data----
dat.list <- list()
for(i in 1:nrow(sp)){
  
  dat.list[[i]] <- wt_download_report(project_id = sp$project_id[i], sensor_id = "ARU", weather_cols = F, report = "main")
  
  print(paste0("Finished dataset ", sp$project_id[i], " : ", i, " of ", nrow(sp), " projects"))
  
}

dat <- do.call(rbind, dat.list)

#3. Wrangle to wide----
dat_wide <- dat %>%
  wt_make_wide() %>%
  inner_join(raw %>%
               mutate(recording_date_time = ymd_hms(date)) %>% 
               rename(organization = org) %>%
               dplyr::select(organization, project_id, location, recording_date_time, wildtrax_url, recording_url) %>%
               unique(),
             multiple="all") %>%
  left_join(sp) %>%
  dplyr::select(-BADG, -GCSP, -TOWA, -UNKN, -WISN)

#4. Keep the detail----
dat_all <- dat %>%
  inner_join(raw %>%
               mutate(recording_date_time = ymd_hms(date)) %>% 
               rename(organization = org) %>%
               dplyr::select(organization, project_id, location, recording_date_time, wildtrax_url, recording_url) %>%
               unique(),
             multiple="all") %>%
  left_join(sp) %>%
  dplyr::filter(!species_code %in% c("BADG", "GCSP", "TOWA", "UNKN", "WISN"))

#5. Save----
write.csv(dat_wide, "G:/Shared drives/ABMI_Recognizers/HawkEars/Data/Evaluation/SingleSpecies_wide.csv", row.names = FALSE)
write.csv(dat_all, "G:/Shared drives/ABMI_Recognizers/HawkEars/Data/Evaluation/SingleSpecies_all.csv", row.names = FALSE)

