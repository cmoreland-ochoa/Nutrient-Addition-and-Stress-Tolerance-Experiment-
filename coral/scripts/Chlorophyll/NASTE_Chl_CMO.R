library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(qiime2R)

#Update source file
CHL<-read.csv("H:/Shared drives/Donahue Lab Data Drive/Jessica Glazner_Donahue Lab Data Drive/NASTE - Nutrient Addition and Stress Tolerance Experiment/DATA/Chlorophyll/NASTE_Chl concentrations_Calculations.csv")

theme_set(
  theme_minimal(base_size = 15) +                             # Set base theme
    theme(
      plot.title = element_text(size = 18, face = "bold"),    # Customize title
      axis.title = element_text(size = 15),                   # Customize axis titles
      axis.text = element_text(size = 15),                    # Customize axis text
      legend.title = element_text(size = 15),                 # Customize legend title
      legend.text = element_text(size = 15)                  # Customize legend text
    )
)

colors <- c(
  #treatmnent_temperature
  "C_AMB" = "#08B5D3",
  "G_AMB" = "#01AD74",
  "I_AMB" = "#D9A33A",
  "E_AMB" = "#E12618",
  "C_HEAT" = "#08B5D3",
  "G_HEAT" = "#01AD74",
  "I_HEAT" = "#D9A33A",
  "E_HEAT" = "#E12618"
)

#MCAP
MCAP.All<-subset(CHL, Species!="P")
MCAP<-subset(MCAP.All, Species!="BLANK")
MCAP$Chl_ug.cm.2<-as.numeric(MCAP$Chl_ug.cm.2)
MCAP %>%
  ggplot( aes(x=T.T, y=Chl_ug.cm.2, fill=T.T)) +
  geom_boxplot()+
  ggtitle("MCAP Chlorophyll") +
  ylab("Chl Concentrations (ug/cm^2)") +
  theme(legend.position='bottom',axis.title.x = element_blank(),axis.text.x=element_text(angle=45, hjust=1))+
  guides(guide_legend(nrow = 2,title="Treatment_Temperature"))+
  scale_fill_manual(values= colors)+
  ylim(0,6)+
  facet_wrap(~Timepoint)

#PCOM
PCOM.All<-subset(CHL, Species!="M")
PCOM<-subset(PCOM.All, Species!="BLANK")
PCOM$Chl_ug.cm.2<-as.numeric(PCOM$Chl_ug.cm.2)
PCOM %>%
  ggplot( aes(x=T.T, y=Chl_ug.cm.2, fill=T.T)) +
  geom_boxplot()+
  ggtitle("PCOM Chlorophyll") +
  ylab("Chl Concentrations (ug/cm^2)") +
  theme(legend.position='bottom',axis.title.x = element_blank(),axis.text.x=element_text(angle=45, hjust=1))+
  guides(guide_legend(nrow = 2,title="Treatment_Temperature"))+
  scale_fill_manual(values= colors)+
  ylim(0,13)+
  facet_wrap(~Timepoint)


