library(wildRtrax)
library(tidyverse)
library(readxl)

#PREAMBLE############

#1. login----
config <- "script/login.R"
source(config)
wt_auth()

#2. Get project list----
project.list <- wt_get_download_summary(sensor_id = 'ARU')

#3. Set root file path---
root <- "G:/Shared drives/ABMI_Recognizers/HawkEars"

#4. Get dataframe with AWS urls in it----
#This file is obtained with the getReports/DownloadClipView.R script in the recognizersandbox repo.

load("G:/My Drive/ABMI/Projects/Recognizers/Data/clip_view_2023-12-15.Rdata")

#GEOGRAPHIC SPREAD####

#TO DO: SAMPLE AT THE RECORDING LEVEL DUMMY AND THEN MAKE WIDE####


#1. Read in most recent download----
load("G:/My Drive/ABMI/Projects/BirdModels/Data/WildTrax/wildtrax_raw_2023-12-11.Rdata")

#2. Wrangle----
#round lat lon
#add AWS link
#filter to SPT
aru.latlon <- aru.wt %>%
  dplyr::filter(task_method=="1SPT") %>%
  wt_tidy_species() %>%
  wt_replace_tmtt() %>%
  wt_make_wide() %>%
  mutate(lat = round(latitude),
         lon= round(longitude)) %>%
  left_join(raw %>%
              rename(recording_date_time = date,
                     organization = org) %>%
              dplyr::select(organization, project_id, location, recording_date_time, wildtrax_url, recording_url) %>%
              unique(),
            multiple="all") %>%
  left_join(projects.use)

latlon <- aru.latlon %>%
  dplyr::select(lat, lon) %>%
  unique()

#3. Select data from lat*lon we have permission for----
set.seed(1234)
aru.use1 <- aru.latlon %>%
  dplyr::filter(status=="Published - Public" | organization %in% c("ABMI", "BU")) %>%
  group_by(lat, lon) %>%
  slice_sample(n=10) %>%
  ungroup() %>%
  unique() %>%
  mutate(origin = ifelse(status=="Published - Public", "Public", organization))

#4. Select data from lat*lon we're missing that are owned by CWS----
set.seed(1234)
aru.use2 <- aru.latlon %>%
  anti_join(aru.use1 %>%
              dplyr::select(lat, lon) %>%
              unique()) %>%
  dplyr::filter(str_sub(organization, 1, 3)=="CWS") %>%
  group_by(lat, lon) %>%
  slice_sample(n=10) %>%
  ungroup() %>%
  unique() %>%
  mutate(origin = organization)

#5. Select data from lat*lon we're missing from Gov-QC and maritimes parks----
set.seed(1234)
aru.use3 <- aru.latlon %>%
  anti_join(aru.use1 %>%
              dplyr::select(lat, lon) %>%
              unique()) %>%
  dplyr::filter(organization=="Gov-QC" | (organization=="PC" & lon > -70)) %>%
  group_by(lat, lon) %>%
  slice_sample(n=10) %>%
  ungroup() %>%
  unique() %>%
  mutate(origin = organization)

#6. Put together----
aru.use <- rbind(aru.use1, aru.use2, aru.use3)

#7. Visualize----
canada <- map_data("world", region=c("Canada"))

ggplot(aru.use) +
  geom_polygon(data=canada, aes(x=long, y=lat, group=group)) +
  geom_point(aes(x=longitude, y=latitude, colour=origin)) +
  coord_sf(xlim=c(-145, -55), ylim=c(40, 70), crs=4326)

ggsave("G:/My Drive/ABMI/Projects/Recognizers/Figures/LatLonDataset.jpeg", width = 7, height = 4)

#8. Plot for permissions----

#Ontario
aru.use2.on <- aru.use2 %>%
  dplyr::filter(organization=="CWS-ONT")

ggplot(aru.use2.on) +
  geom_polygon(data=canada, aes(x=long, y=lat, group=group)) +
  geom_point(aes(x=longitude, y=latitude, colour=project)) +
  coord_sf(xlim=c(-145, -55), ylim=c(40, 70), crs=4326) +
  theme(legend.position = "bottom") +
  guides(colour=guide_legend(nrow=5))

ggsave("G:/My Drive/ABMI/Projects/Recognizers/Figures/LatLonDataset_CWSON.jpeg", width = 7, height = 6)

#North
aru.use2.nor <- aru.use2 %>%
  dplyr::filter(organization=="CWS-NOR")

ggplot(aru.use2.nor) +
  geom_polygon(data=canada, aes(x=long, y=lat, group=group)) +
  geom_point(aes(x=longitude, y=latitude, colour=project)) +
  coord_sf(xlim=c(-145, -55), ylim=c(40, 70), crs=4326) +
  theme(legend.position = "bottom") +
  guides(colour=guide_legend(nrow=7))

ggsave("G:/My Drive/ABMI/Projects/Recognizers/Figures/LatLonDataset_CWSNOR.jpeg", width = 7, height = 7)


#8. Save----
write.cs(aru.use, )
