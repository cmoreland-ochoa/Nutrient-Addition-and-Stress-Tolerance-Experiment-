### Water Nutrients ###

#load libraries
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)

#load dataset
nuts <- read.csv("C:\\Users\\jglaz\\Desktop\\Datasets\\water_nuts.csv")

# Dosing Container Plots: nut x week for each species

# TIN


ggplot()+
  geom_line(data=subset(nuts, Tank %in% c("Dosing")),
            aes(x=Timepoint, y=TIN, group=Treatment, color=Treatment))+
  facet_wrap(~Species)+
  ggtitle("Dosing Container Inorganic Nitrogen Concentrations")+
  scale_color_manual(values=c('Blue', 'Red', 'Forest Green', 'Gold'))+
  theme(plot.title=element_text(size=20),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.text = element_text(size=15),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15))


# PO4

ggplot()+
  geom_line(data=subset(nuts, Tank %in% c("Dosing")),
            aes(x=Timepoint, y=Phosphate, group=Treatment, color=Treatment))+
  facet_wrap(~Species)+
  ggtitle("Dosing Container Phosphate Concentrations")+
  scale_color_manual(values=c('Blue', 'Red', 'Forest Green', 'Gold'))+
  theme(plot.title=element_text(size=20),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.text = element_text(size=15),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15))

# N:P - note these are TIN:PO4

ggplot()+
  geom_line(data=subset(nuts, Tank %in% c("Dosing")),
            aes(x=Timepoint, y=N_to_P, group=Treatment, color=Treatment))+
  facet_wrap(~Species)+
  ggtitle("Dosing Container N:P (TIN:PO4)")+
  labs(y="N:P")+
  scale_color_manual(values=c('Blue', 'Red', 'Forest Green', 'Gold'))+
  theme(plot.title=element_text(size=20),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.text = element_text(size=15),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15))

### Treatment Tank Plots: nut x week for each species

# TIN
avg_TIN <- nuts %>%
  group_by(Timepoint, Tank, Treatment, Species) %>%
  summarize(avg_TIN = mean(TIN))

ggplot()+
  geom_line(data=subset(avg_TIN, Tank %in% c("Aquaria")),
            aes(x=Timepoint, y=avg_TIN, group=Treatment, color=Treatment))+
  facet_wrap(~Species)+
  ggtitle("Treatment Tank Inorganic Nitrogen Concentrations")+
  labs(y="TIN")+
  scale_color_manual(values=c('Blue', 'Red', 'Forest Green', 'Gold'))+
  theme(plot.title=element_text(size=20),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.text = element_text(size=15),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15))


# PO4
avg_PO4 <- nuts %>%
  group_by(Timepoint, Tank, Treatment, Species) %>%
  summarize(avg_PO4 = mean(Phosphate))

ggplot()+
  geom_line(data=subset(avg_PO4, Tank %in% c("Aquaria")),
            aes(x=Timepoint, y=avg_PO4, group=Treatment, color=Treatment))+
  facet_wrap(~Species)+
  ggtitle("Treatment Tank PO4 Concentrations")+
  labs(y="PO4")+
  scale_color_manual(values=c('Blue', 'Red', 'Forest Green', 'Gold'))+
  theme(plot.title=element_text(size=20),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.text = element_text(size=15),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15))

# N:P
avg_ratio <- nuts %>%
  group_by(Timepoint, Tank, Treatment, Species) %>%
  summarize(avg_ratio = mean(N_to_P))

ggplot()+
  geom_line(data=subset(avg_ratio, Tank %in% c("Aquaria")),
            aes(x=Timepoint, y=avg_ratio, group=Treatment, color=Treatment))+
  facet_wrap(~Species)+
  ggtitle("Treatment Tank N:P")+
  labs(y="N:P")+
  scale_color_manual(values=c('Blue', 'Red', 'Forest Green', 'Gold'))+
  theme(plot.title=element_text(size=20),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.text = element_text(size=15),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15))


### Aquaria Plots: expected vs actual

ggplot()+
  geom_line(data=nuts,
            aes(x=Timepoint, y=Avg_TIN, group=Treatment, color=Treatment, linetype="dashed"))+
  geom_line(data=nuts, aes(x=Timepoint, y=Expected_TIN, group=Treatment, color=Treatment, linetype="solid"))+
  facet_wrap(~Species)+
  geom_jitter(data=nuts,aes(x=Timepoint, y=TIN, color=Treatment))+
  ggtitle("Treatment Tank Inorganic Nitrogen Concentrations")+
  labs(y="TIN")+
  scale_color_manual(values=c('Blue', 'Red', 'Forest Green', 'Gold'))+
  theme(plot.title=element_text(size=20),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.text = element_text(size=15),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15))
