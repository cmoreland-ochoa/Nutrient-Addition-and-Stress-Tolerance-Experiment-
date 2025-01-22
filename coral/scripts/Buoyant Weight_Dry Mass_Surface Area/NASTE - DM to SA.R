library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggplot2)
library(dplyr)
library("RColorBrewer")

#Update source file
MCAP<-read.csv("H:/Shared drives/Donahue Lab Data Drive/Jessica Glazner_Donahue Lab Data Drive/NASTE - Nutrient Addition and Stress Tolerance Experiment/DATA/Dry Mass-Surface Area/NASTE_DM to SA_MCAP.csv")

MCAP %>%
  ggplot( aes(x=surface_area_cm2, y=dry_mass_g, color=Timepoint)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE,aes(group=Timepoint))+
  facet_grid(Temperature~Treatment)+
  ggtitle("MCAP Dry Mass to Surface Area Ratio") +
  ylab("Dry Mass (g)")+
  xlab("Surface Area (cm2)")

PCOM<-read.csv("H:/Shared drives/Donahue Lab Data Drive/Jessica Glazner_Donahue Lab Data Drive/NASTE - Nutrient Addition and Stress Tolerance Experiment/DATA/Dry Mass-Surface Area/NASTE_DM to SA_PCOM.csv")

PCOM %>%
  ggplot( aes(x=surface_area_cm2, y=dry_mass_g, color=Timepoint)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE,aes(group=Timepoint))+
  facet_grid(Temperature~Treatment)+
  ggtitle("PCOM Dry Mass to Surface Area Ratio") +
  ylab("Dry Mass (g)")+
  xlab("Surface Area (cm2)")
