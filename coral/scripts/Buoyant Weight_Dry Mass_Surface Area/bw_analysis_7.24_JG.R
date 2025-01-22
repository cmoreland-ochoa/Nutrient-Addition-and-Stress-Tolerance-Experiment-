### BUoyant Weight Analysis ###

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
bw_data <- read.csv("C:\\Users\\jglaz\\Desktop\\Datasets\\BW_final_data.csv")

#make separate df for each species
mc_bw <- bw_data [bw_data$species == "mc",]
pc_bw <- bw_data [bw_data$species == "pc",]

# Mcap T0-T2
ggplot(data=mc_bw,aes(x=treatment, y=delta_wt_norm, fill=treatment)) +
  geom_boxplot(data = . %>% filter(timepoint =="2"), outlier.size = 0)+
  geom_jitter(data = . %>% filter(timepoint =="2"), width = 0.2, alpha = 0.7)+
  facet_wrap(~temp_trt)+
  scale_fill_manual(values=c("blue",
                             "red",
                             "forestgreen",
                             "gold"))+
  ggtitle(expression(paste("Change in buoyant weight from T0 to T2 of", phantom(x), italic("Montipora capitata"))))+
  xlab("Treatment")+
  ylab("Delta Weight")+
  ylim(-0.1, 0.1)


# Mcap T2-T3
ggplot(data=mc_bw,aes(x=treatment, y=delta_wt_norm, fill=treatment)) +
  geom_boxplot(data = . %>% filter(timepoint =="3"), outlier.size = 0)+
  geom_jitter(data = . %>% filter(timepoint =="3"), width = 0.2, alpha = 0.7)+
  facet_wrap(~temp_trt)+
  scale_fill_manual(values=c("blue",
                             "red",
                             "forestgreen",
                             "gold"))+
  ggtitle(expression(paste("Change in buoyant weight from T2 to T3 of", phantom(x), italic("Montipora capitata"))))+
  xlab("Treatment")+
  ylab("Delta Weight")+
  ylim(-0.1, 0.1)

# Pcom T0-T2
ggplot(data=pc_bw,aes(x=treatment, y=delta_wt_norm, fill=treatment)) +
  geom_boxplot(data = . %>% filter(timepoint =="2"), outlier.size = 0)+
  geom_jitter(data = . %>% filter(timepoint =="2"), width = 0.2, alpha = 0.7)+
  facet_wrap(~temp_trt)+
  scale_fill_manual(values=c("blue",
                             "red",
                             "forestgreen",
                             "gold"))+
  ggtitle(expression(paste("Change in buoyant weight from T0 to T2 of", phantom(x), italic("Porites compressa"))))+
  xlab("Treatment")+
  ylab("Delta Weight")+
  ylim(-0.1, 0.3)


# Pcom T2-T3
ggplot(data=pc_bw,aes(x=treatment, y=delta_wt_norm, fill=treatment)) +
  geom_boxplot(data = . %>% filter(timepoint =="3"), outlier.size = 0)+
  geom_jitter(data = . %>% filter(timepoint =="3"), width = 0.2, alpha = 0.7)+
  facet_wrap(~temp_trt)+
  scale_fill_manual(values=c("blue",
                             "red",
                             "forestgreen",
                             "gold"))+
  ggtitle(expression(paste("Change in buoyant weight from T2 to T3 of", phantom(x), italic("Porites compressa"))))+
  xlab("Treatment")+
  ylab("Delta Weight")+
  ylim(-0.1, 0.3)

# Facet by treatment
## Mcap
ggplot(data=mc_bw,aes(x=temp_trt, y=delta_wt_norm, fill=temp_trt)) +
  geom_boxplot(data = . %>% filter(timepoint =="2"), outlier.size = 0)+
  geom_jitter(data = . %>% filter(timepoint =="2"), width = 0.2, alpha = 0.7)+
  facet_grid(~treatment)+
  scale_fill_manual(values=c("skyblue", "red"))+
  ggtitle(expression(paste("Change in buoyant weight from T0 to T2 of", phantom(x), italic("Montipora capitata"))))+
  xlab("Treatment")+
  ylab("Delta Weight")+
  ylim(-0.1, 0.2)

ggplot(data=mc_bw,aes(x=temp_trt, y=delta_wt_norm, fill=temp_trt)) +
  geom_boxplot(data = . %>% filter(timepoint =="3"), outlier.size = 0)+
  geom_jitter(data = . %>% filter(timepoint =="3"), width = 0.2, alpha = 0.7)+
  facet_grid(~treatment)+
  scale_fill_manual(values=c("skyblue", "red"))+
  ggtitle(expression(paste("Change in buoyant weight from T2 to T3 of", phantom(x), italic("Montipora capitata"))))+
  xlab("Treatment")+
  ylab("Delta Weight")+
  ylim(-0.1, 0.1)

##Pcom
ggplot(data=pc_bw,aes(x=temp_trt, y=delta_wt_norm, fill=temp_trt)) +
  geom_boxplot(data = . %>% filter(timepoint =="2"), outlier.size = 0)+
  geom_jitter(data = . %>% filter(timepoint =="2"), width = 0.2, alpha = 0.7)+
  facet_grid(~treatment)+
  scale_fill_manual(values=c("skyblue", "red"))+
  ggtitle(expression(paste("Change in buoyant weight from T0 to T2 of", phantom(x), italic("Porites compressa"))))+
  xlab("Treatment")+
  ylab("Delta Weight")+
  ylim(-0.1, 0.25)


ggplot(data=pc_bw,aes(x=temp_trt, y=delta_wt_norm, fill=temp_trt)) +
  geom_boxplot(data = . %>% filter(timepoint =="3"), outlier.size = 0)+
  geom_jitter(data = . %>% filter(timepoint =="3"), width = 0.2, alpha = 0.7)+
  facet_grid(~treatment)+
  scale_fill_manual(values=c("skyblue", "red"))+
  ggtitle(expression(paste("Change in buoyant weight from T2 to T3 of", phantom(x), italic("Porites compressa"))))+
  xlab("Treatment")+
  ylab("Delta Weight")+
  ylim(-0.1, 0.25)


# Linear Mixed Effects Models
##subset data for T0-T2 and T2-T3
mc_0_2 <- mc_bw [mc_bw$timepoint == "2",]
mc_2_3 <- mc_bw [mc_bw$timepoint == "3",]
pc_0_2 <- pc_bw [pc_bw$timepoint == "2",]
pc_2_3 <- pc_bw [pc_bw$timepoint == "3",]


# Montipora
## Delta 0-2
mc_bw_lme_1 <- lmer(delta_wt_norm ~ treatment * temp_trt + (1|genotype), data=mc_0_2)
summary(mc_bw_lme_1)

emm_mc_1<- emmeans(mc_bw_lme_1, ~ treatment)
emm_mc_1
pair_mc_1<- contrast(emm_mc_1, method='pairwise')
summary(pair_mc_1)

## Delta 2-3
mc_bw_lme_2 <- lmer(delta_wt_norm ~ treatment * temp_trt + (1|genotype), data=mc_2_3)
summary(mc_bw_lme_2)

emm_mc_2<- emmeans(mc_bw_lme_2, ~ treatment)
emm_mc_2
pair_mc_2<- contrast(emm_mc_2, method='pairwise')
summary(pair_mc_2)

#Porites
## Delta 0-2
pc_bw_lme_1 <- lmer(delta_wt_norm ~ treatment * temp_trt + (1|genotype), data=pc_0_2)
summary(pc_bw_lme_1)

emm_pc_1<- emmeans(pc_bw_lme_1, ~ treatment)
emm_pc_1
pair_pc_1<- contrast(emm_pc_1, method='pairwise')
summary(pair_pc_1)

## Delta 2-3
pc_bw_lme_2 <- lmer(delta_wt_norm ~ treatment * temp_trt + (1|genotype), data=pc_2_3)
summary(pc_bw_lme_2)

emm_pc_2<- emmeans(pc_bw_lme_2, ~ treatment)
emm_pc_2
pair_pc_2<- contrast(emm_pc_2, method='pairwise')
summary(pair_pc_2)
