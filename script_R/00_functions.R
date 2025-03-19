calculate_prf <- function(threshold, data, annotations){
  
  #Summarize
  data_thresholded <- dplyr::filter(data, confidence >= threshold) |>
    summarize(precision = sum(tp)/(sum(tp) + sum(fp)),
              recall = sum(tp)/annotations) |>
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
