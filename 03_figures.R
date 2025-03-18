# ---
# title: "03 figures for manuscript"
# author: "Elly Knight"
# date: "2025-03-18"
# inputs: "evaluation output from `02_evaluate_community.R" and from python evaluation script in HawkEars repo for vocal activity dataset
# outputs: "figures for manuscript"

# ---

# 1. Setup ----

## 1.1 Load libraries----
library(tidyverse)
library(RColorBrewer)

## 1.2 Set colours----
cols3 <- c("#355F89", "#E39D22", "#34A853")
heuristic_cols <- brewer.pal(n=8, name = "Dark2")[c(1,2,3,6,8)]

# 2. Figure 3 - Performance for community dataset per recording minute ----

## 2.1 Read in the evaluation ----
minutes_out <- read.csv("Evaluation_community_recording.csv")

## 2.2 Summarize to mean ----
minutes_summary <- minutes_out |> 
  group_by(threshold, classifier, metric) |> 
  summarize(mn = mean(value, na.rm = TRUE),
            std = sd(value, na.rm = TRUE)) |> 
  ungroup()

## 2.3 Make factors for plotting ----
minutes_summary$metric <- factor(minutes_summary$metric, levels=c("precision", "recall", "fscore", "richness"),
                         labels=c("Precision", "Recall", "F1-score", "Species richness"))

minutes_out$metric <- factor(minutes_out$metric, levels=c("precision", "recall", "fscore", "richness"),
                             labels=c("Precision", "Recall", "F1-score", "Species richness"))

## 2.4 Plot ----
ggplot() + 
  geom_line(data=minutes_out,
            aes(x=threshold, y=value, group=minute_id),
            alpha = 0.5, linewidth = 0.2, colour="grey70") +
  geom_line(data=minutes_summary,
            aes(x=threshold, y=mn, colour=classifier),
            linewidth=1) + 
  facet_grid(metric ~ classifier, scales="free") + 
  scale_colour_manual(values=cols3, name="") +
  theme_classic() +
  ylab("Mean value per recording") + 
  xlab("Score threshold") +
  theme(legend.position = "bottom")

## 2.5 Save ----
ggsave("Figure3.jpeg", width=9, height = 10, units="in")

# 3. Figure 4 - Performance for vocal activity rate ----

## 3.1 Read in evaluation results ----
activity_out <- read.csv("Evaluation_vocalactivity.csv") |> 
  dplyr::filter(threshold >= 0.1)

## 3.2 Make a dataframe of species names and codes ----
species <- data.frame(species = sort(unique(activity_out$species)),
                      name = c("Barred owl", "Black-throated green warbler", "Common yellowthroat",
                               "Olive-sided flycatcher", "Ovenbird", "Ruffed grouse",
                               "Swainson's thrush", "Tennesee warbler", "White-throated sparrow"))

## 3.3 Plot----
ggplot(activity_out |> left_join(species)) + 
  geom_line(aes(x=threshold, y=f1score, colour=classifier), linewidth = 1) + 
  facet_wrap(~name) + 
  scale_colour_manual(values=cols3, name="") +
  theme_classic() +
  ylab("F1-score") + 
  xlab("Score threshold") +
  theme(legend.position = "bottom") +
  ylim(c(0,1))

ggsave("Figure4.jpeg", width=9, height = 10, units="in")

# 4. Appendix D - Heuristics ----

## 4.1 Read in heuristics evaluation ----
heur_raw <- read.csv("Evaluation_community_heuristics.csv")

## 4.2 Wrangle----
heur <- heur_raw |> 
  pivot_longer(base:filters, names_to="heuristic", values_to="recall") |> 
  mutate(heuristic = factor(heuristic, levels = c("base", "choose", "overlap", "species.pool", "filters"), labels = c("Base", "Channel selection", "Above plus\nwindow overlap", "Above plus\nspecies pool\nadjustment", "Above plus\nbandpass\nfilters")))

## 4.3 Plot----
ggplot(heur) + 
  geom_line(aes(x=precision, y=recall, colour=heuristic, linetype = heuristic), linewidth = 1) + 
  theme_classic() +
  ylab("Recall") + 
  xlab("Precision") +
  theme(legend.position = "right") +
  scale_colour_manual(name = "", values = heuristic_cols) +
  scale_linetype_manual(name = "", values = c("solid", "dashed", "dotted", "dotdash", "longdash")) +
  ylim(c(0,1)) 

#4. Save----
ggsave("AppendixD.jpeg", width = 7, height = 5, units="in")

## end script ##