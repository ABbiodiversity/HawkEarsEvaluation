#This script wrangles recording-level metadata for hawkears evaluation.

#TO DO: MAYBE SOME LANDCOVER OR TREE COVER?

#PREAMBLE############

#1. Load libraries----
library(tidyverse)
library(tuneR)
library(seewave)
library(hardRain)
library(soundecology)

#2. Set root file path---
root <- "G:/Shared drives/ABMI_Recognizers/HawkEars"

#3. Get evaluation dataset----
eval <- read.csv(file.path(root, "Data", "Evaluation", "ExpertData.csv")) %>% 
  dplyr::filter(!is.na(recording_url))

#DOWNLOAD RECORDINGS##########

#1. Get download list----
recs <- eval %>% 
  dplyr::select(recording_url) %>% 
  unique() %>% 
  separate(recording_url, into=c("f1", "f2", "f3", "f4", "file"), sep="/", remove=FALSE) %>% 
  separate(file, into=c("recording_id", "filetype")) %>% 
  dplyr::select(recording_url, recording_id, filetype)

#2. Get list of recordings already done----
done <- data.frame(file = list.files(file.path(root, "Data", "Recordings", "ExpertData", "wav"))) %>% 
  mutate(recording_id = str_sub(file, -100, -5))

#3. Set up download loop---- 
todo <- anti_join(recs, done)

for(i in 1:nrow(todo)){
  
  #4. download compressed recordings----
  try(download.file(todo$recording_url[i],
                    destfile = file.path(tempdir(), paste0(todo$recording_id[i], ".", todo$filetype[i])),
                    quiet=TRUE,
                    method="libcurl",
                    mode="wb"))
  
  #5. convert to wav if mp3----
  if(todo$filetype[i]=="mp3"){
    
    mp3.i <- readMP3(file.path(tempdir(), paste0(todo$recording_id[i], ".", todo$filetype[i])))
    
    if(class(mp3.i)=="Wave"){
      writeWave(mp3.i, file.path(root, "Data", "Recordings", "ExpertData", "wav", paste0(todo$recording_id[i], ".wav")))
    }
  }
  
  #6. convert to wav if flac----
  if(todo$filetype[i]=="flac"){
    
    setwd(tempdir())
    
    wav2flac(file=paste0(todo$recording_id[i], ".flac"),
             reverse=TRUE)
    
    file.copy(from=file.path(tempdir(), paste0(todo$recording_id[i], ".wav")),
              to=file.path(root, "Data", "Recordings", "ExpertData", "wav", paste0(todo$recording_id[i], ".wav")))
  }
  
  print(paste0("Finished recording ", i, " of ", nrow(todo)))
  
}

#MEASURE HARD RAIN##########

#1. Get list of recordings----
files <- data.frame(path = list.files(file.path(root, "Data", "Recordings", "ExpertData", "wav"), full.names = TRUE),
                    file = list.files(file.path(root, "Data", "Recordings", "ExpertData", "wav"))) %>% 
  mutate(recording_id = as.integer(str_sub(file, -100, -5)))

#2. Set up loop----
out.list <- list()
for(i in 1:nrow(files)){
  
  #3. Read in wav----
  wav.i <- try(readWave(files$path[i]))
  
  if(class(wav.i)[1]=="Wave"){
    
    #4. Measure-----
    rain.i <- getMetrics(wav.i, t.step=60, parallel=TRUE)
    
    #5. Wrangle----
    out.list[[i]] <- rain.i %>% 
      data.frame() %>% 
      mutate(minute = row_number(),
             recording_id = files$recording_id[i])
    
  }
  
  print(paste0("Finished recording ", i, " of ", nrow(files)))
  
}

out <- do.call(rbind, out.list)

#6. Save----
write.csv(out, file.path(root, "Results", "ExpertData", "ExpertData_HardRain.csv"), row.names = FALSE)

#MEASURE ACI#########

#1. Get list of recordings----
files <- data.frame(path = list.files(file.path(root, "Data", "Recordings", "ExpertData", "wav"), full.names = TRUE),
                    file = list.files(file.path(root, "Data", "Recordings", "ExpertData", "wav"))) %>% 
  mutate(recording_id = as.integer(str_sub(file, -100, -5)))

#2. Get list of minutes per recording to run----
loop <- read.csv(file.path(root, "Data", "Evaluation", "ExpertData.csv")) %>% 
  dplyr::filter(!is.na(recording_url)) %>% 
  separate(recording_url, into=c("f1", "f2", "f3", "f4", "recfile"), sep="/") %>% 
  separate(recfile, into=c("recording_id", "filetype")) %>% 
  dplyr::select(-c(f1, f2, f3, f4, filetype)) %>% 
  mutate(recording_id = as.integer(recording_id),
         CAJA = ifelse(CAJA==0, GRAJ, CAJA)) %>% 
  dplyr::select(-GRAJ, -CHIK) %>% 
  dplyr::select(recording_id, minute) %>% 
  unique() %>% 
  left_join(files)

#3. Set up loop----
out.list <- list()
for(i in 1:nrow(loop)){
  
  #4. Read in wav----
  wav.i <- readWave(loop$path[i], from = loop$minute[i]-1, to=loop$minute[i], units="minutes")
  
  #5. Measure-----
  aci.i <- acoustic_complexity(wav.i)
  adi.i <- acoustic_diversity(wav.i)
  aei.i <- acoustic_evenness(wav.i)
  
  #6. Wrangle----
  out.list[[i]] <- data.frame(aci.l = aci.i$AciTotAll_left,
                              aci.r = aci.i$AciTotAll_right,
                              adi.l = adi.i$adi_left,
                              adi.r = adi.i$adi_right,
                              aei.l = aei.i$aei_left,
                              aei.r = aei.i$aei_right) %>% 
    mutate(minute = loop$minute[i],
           recording_id = loop$recording_id[i],
           aci = mean(aci.l, aci.r),
           adi = mean(adi.l, adi.r),
           aei = mean(aei.l, aei.r))
  
  print(paste0("Finished recording minute ", i, " of ", nrow(loop)))
  
}

out <- do.call(rbind, out.list)

#7. Save-----
write.csv(out, file.path(root, "Results", "ExpertData", "ExpertData_AcousticIndices.csv"), row.names = FALSE)


#PUT TOGETHER############

hr <- read.csv(file.path(root, "Results", "ExpertData", "ExpertData_HardRain.csv"))
ai <- read.csv(file.path(root, "Results", "ExpertData", "ExpertData_AcousticIndices.csv"))

covs <- eval %>% 
  separate(recording_url, into=c("f1", "f2", "f3", "f4", "file"), sep="/", remove=FALSE) %>% 
  separate(file, into=c("recording_id", "filetype")) %>% 
  dplyr::select(latitude, longitude, project_name, location, recording_date, recording_url, recording_id, filetype) %>% 
  mutate(recording_id = as.integer(recording_id)) %>% 
  unique() %>% 
  left_join(hr, multiple="all") %>% 
  left_join(ai, multiple="all")

write.csv(covs, file.path(root, "Results", "ExpertData", "ExpertData_RecordingCovariates.csv"), row.names = FALSE)
