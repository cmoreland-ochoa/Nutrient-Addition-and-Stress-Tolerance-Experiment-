### PAM Dataset Manipulation ###

# load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(sjPlot)
library(lme4)
library(lmerTest)
library(emmeans)
library(car)
library(magrittr)

# load dataset 
PAM <- read.csv("C:\\Users\\jglaz\\Desktop\\Datasets\\PAM_data.csv")

# pivot dataset so that each measurement is it's own row
PAM <- PAM %>%
  pivot_longer(w0_PAM:w10_PAM, names_to = "week", values_to = "FvFm")


# drop rows with NA
PAM <- PAM %>% drop_na()

# adjust names in dataframe
PAM$temp[PAM$temp=="a"]<-"Ambient"
PAM$temp[PAM$temp=="h"]<-"Heated"

PAM$week[PAM$week == "w0_PAM"]<- "0"
PAM$week[PAM$week == "w1_PAM"]<- "1"
PAM$week[PAM$week == "w2_PAM"]<- "2"
PAM$week[PAM$week == "w3_PAM"]<- "3"
PAM$week[PAM$week == "w4_PAM"]<- "4"
PAM$week[PAM$week == "w5_PAM"]<- "5"
PAM$week[PAM$week == "w6_PAM"]<- "6"
PAM$week[PAM$week == "w7_PAM"]<- "7"
PAM$week[PAM$week == "w8_PAM"]<- "8"
PAM$week[PAM$week == "w9_PAM"]<- "9"
PAM$week[PAM$week == "w10_PAM"]<- "10"

#change week to factor
PAM$week<-as.numeric(PAM$week)

# Mcap Plot


plot1 <-ggplot(PAM, aes(x=week, y=FvFm, color=treatment, linetype=temp)) +
  geom_point(data=subset(PAM, species %in% c("m"))) +
  geom_smooth(se=F) +
  scale_color_manual(values=c("blue",
                              "red",
                              "forestgreen",
                              "gold"))+
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10))+
  ggtitle(expression(paste("Photosynthetic Output of", phantom(x), italic("Montipora capitata"))))+
  xlab("Week")+
  ylab("Fv/Fm")+
  labs(color="Treatment",linetype="Temperature")+
  ylim(0, 0.6)

plot1+theme(plot.title=element_text(size=20),
            axis.title=element_text(size=18),
            axis.text=element_text(size=13),
            legend.title=element_text(size=18),
            legend.text=element_text(size=13))

# Pcom plot
plot2 <-ggplot(PAM, aes(x=week, y=FvFm, color=treatment, linetype=temp)) +
  geom_point(data=subset(PAM, species %in% c("p"))) +
  geom_smooth(se=F) +
  scale_color_manual(values=c("blue",
                              "red",
                              "forestgreen",
                              "gold"))+
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10))+
  ggtitle(expression(paste("Photosynthetic Output of", phantom(x), italic("Porites compressa"))))+
  xlab("Week")+
  ylab("Fv/Fm")+
  labs(color="Treatment",linetype="Temperature")+
  ylim(0, 0.6)


plot2+theme(plot.title=element_text(size=20),
            axis.title=element_text(size=18),
            axis.text=element_text(size=13),
            legend.title=element_text(size=18),
            legend.text=element_text(size=13))

# LINEAR MIXED EFFECTS MODELS

#subset df by species
mc_pam <- PAM [PAM$species == "m",]
pc_pam <- PAM [PAM$species == "p",]


# Full model

## Mcap
full_model_mc <- lmer(FvFm ~ treatment * temp * week + (1|genotype) + (1| ID), data=mc_pam)
summary(full_model_mc)

emm_mc <- emmeans(full_model_mc, ~treatment)
emm_mc
pair_mc <- contrast (emm_mc, method='pairwise')
summary(pair_mc)

## Pcom
full_model_pc <- lmer(FvFm ~ treatment * temp * week + (1|genotype) + (1| ID), data=pc_pam)
summary(full_model_pc)

emm_pc <- emmeans(full_model_pc, ~treatment)
emm_pc
pair_pc <- contrast (emm_pc, method='pairwise')
summary(pair_pc)


# LME separated by temp treatment 
mc_pam_a <- mc_pam [mc_pam$temp == "Ambient",]
pc_pam_a <- pc_pam [pc_pam$temp == "Ambient",]
mc_pam_h <- mc_pam [mc_pam$temp == "Heated",]
pc_pam_h <- pc_pam [pc_pam$temp == "Heated",]

# Mcap Ambient
mc_pam_a <- lmer(FvFm ~ treatment*week + (1|genotype), data=mc_pam_a)
summary(mc_pam_a)

emm_a_mc <- emmeans(mc_pam_a, ~treatment)
emm_a_mc
pair_a_mc<-contrast(emm_a_mc, method='pairwise')
summary(pair_a_mc)

# Pcom Ambient
pc_pam_a <- lmer(FvFm ~ treatment*week + (1|genotype), data=pc_pam_a)
summary(pc_pam_a)

emm_a_pc <- emmeans(pc_pam_a, ~treatment)
emm_a_pc
pair_a_pc<-contrast(emm_a_pc, method='pairwise')
summary(pair_a_pc)

# Mcap Heated
mc_pam_h <- lmer(FvFm ~ treatment*week + (1|genotype), data=mc_pam_h)
summary(mc_pam_h)

emm_h_mc <- emmeans(mc_pam_h, ~treatment)
emm_h_mc
pair_h_mc<-contrast(emm_h_mc, method='pairwise')
summary(pair_h_mc)

# Pcom Heated
pc_pam_h <- lmer(FvFm ~ treatment*week + (1|genotype), data=pc_pam_h)
summary(pc_pam_h)

emm_h_pc <- emmeans(pc_pam_h, ~treatment)
emm_h_pc
pair_h_pc<-contrast(emm_h_pc, method='pairwise')
summary(pair_h_pc)
