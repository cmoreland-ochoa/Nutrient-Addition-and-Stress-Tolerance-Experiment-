library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggplot2)
library(dplyr)
library(reshape2)
library("RColorBrewer")

theme_q2r<- function () {
  theme_classic(base_size=8, base_family="Helvetica") +
    theme(panel.border = element_rect(color="black", size=1, fill=NA)) +
    theme(axis.line = element_blank(), strip.background = element_blank())
}

#Update source file 
MCAP<-read.csv("C:/Users/austc/Downloads/NASTE_Growth.xlsx - M.CAP formated.csv")

#MCAP Linear Extension
MCAP %>%
  ggplot( aes(x=Timepoint, y=H.AVG, fill=T.T)) +
  geom_boxplot() +
  facet_grid(Temperature~Treatment)+
  ggtitle("MCAP Linear Extension") +
  ylab("Linear Extension (cm)") +  
  scale_fill_discrete(name="Treatment\nType")+
  theme_q2r()+
  stat_summary(fun = mean, geom = "text", col = "black", size=4,    
               vjust = 2.5, aes(label = paste( round(..y.., digits = 1))))

Mcap.LE <- transform(MCAP,
                  Timepoint = as.factor(Timepoint),
                  T.T = as.factor(T.T),
                  Average=as.numeric(H.AVG))
M.stat.test <- Mcap.LE %>%
  group_by(T.T) %>%
  pairwise_t_test(
    H.AVG ~ Timepoint, paired = TRUE, 
    p.adjust.method = "bonferroni" 
  ) 
M.stat.test

#MCAP Surface Area

MCAP %>%
  ggplot( aes(x=Timepoint, y=SA, fill=T.T)) +
  geom_boxplot() +
  facet_grid(Temperature~Treatment)+
  ggtitle("MCAP Surface Area") +
  ylab("Average Surface Area (cm^2)") +  
  scale_fill_discrete(name="Treatment\nType")+
  theme_q2r()+
  stat_summary(fun = mean, geom = "text", col = "black", size=4,    
               vjust = 2.5, aes(label = paste( round(..y.., digits = 2))))

M.stat.test <- MCAP %>%
  group_by(T.T) %>%
  pairwise_t_test(
    SA ~ Timepoint, paired = TRUE, 
    p.adjust.method = "bonferroni" 
  ) 
M.stat.test

#MCAP Relative Growth Rate

MCAP %>%
  ggplot( aes(x=Timepoint, y=RGR, fill=T.T)) +
  geom_boxplot() +
  facet_grid(Temperature~Treatment)+
  ggtitle("MCAP Relative Growth Rate") +
  ylab("Relative Growth Rate") +  
  scale_fill_discrete(name="Treatment\nType")+
  theme_q2r()+
  stat_summary(fun = mean, geom = "text", col = "black", size=4,    
               vjust = 2.5, aes(label = paste( round(..y.., digits = 2))))


M.stat.test <- MCAP %>%
  group_by(T.T) %>%
  pairwise_t_test(
    RGR ~ Timepoint, paired = TRUE, 
    p.adjust.method = "bonferroni" 
  ) 
M.stat.test


#Update Source File
PCOM<-read.csv("C:/Users/austc/Downloads/NASTE_Growth.xlsx - P. COM formated.csv")

#PCOM Linear Extension
PCOM %>%
  ggplot( aes(x=Timepoint, y=H.AVG, fill=T.T)) +
  geom_boxplot() +
  facet_grid(Temperature~Treatment)+
  ggtitle("PCOM Linear Extension") +
  ylab("Linear Extension (cm)") +  
  scale_fill_discrete(name="Treatment\nType")+
  theme_q2r()+
  stat_summary(fun = mean, geom = "text", col = "black", size=4,    
               vjust = 2.5, aes(label = paste( round(..y.., digits = 1))))

Pcom.LE <- transform(PCOM,
                     Timepoint = as.factor(Timepoint),
                     T.T = as.factor(T.T),
                     Average=as.numeric(H.AVG))
P.stat.test <- Pcom.LE %>%
  group_by(T.T) %>%
  pairwise_t_test(
    H.AVG ~ Timepoint, paired = TRUE, 
    p.adjust.method = "bonferroni" 
  ) 
P.stat.test

#PCOM Surface Area

PCOM %>%
  ggplot( aes(x=Timepoint, y=SA, fill=T.T)) +
  geom_boxplot() +
  facet_grid(Temperature~Treatment)+
  ggtitle("PCOM Surface Area") +
  ylab("Average Surface Area (cm^2)") +  
  scale_fill_discrete(name="Treatment\nType")+
  theme_q2r()+
  stat_summary(fun = mean, geom = "text", col = "black", size=4,    
               vjust = 2.5, aes(label = paste( round(..y.., digits = 2))))

P.stat.test <- PCOM %>%
  group_by(T.T) %>%
  pairwise_t_test(
    SA ~ Timepoint, paired = TRUE, 
    p.adjust.method = "bonferroni" 
  ) 
P.stat.test

#MCAP Relative Growth Rate

PCOM %>%
  ggplot( aes(x=Timepoint, y=RGR, fill=T.T)) +
  geom_boxplot() +
  facet_grid(Temperature~Treatment)+
  ggtitle("PCOM Relative Growth Rate") +
  ylab("Relative Growth Rate") +  
  scale_fill_discrete(name="Treatment\nType")+
  theme_q2r()+
  stat_summary(fun = mean, geom = "text", col = "black", size=4,    
               vjust = 2.5, aes(label = paste( round(..y.., digits = 2))))


P.stat.test <- PCOM %>%
  group_by(T.T) %>%
  pairwise_t_test(
    RGR ~ Timepoint, paired = TRUE, 
    p.adjust.method = "bonferroni" 
  ) 
P.stat.test

