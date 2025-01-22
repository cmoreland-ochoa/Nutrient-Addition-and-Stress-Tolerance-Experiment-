library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(reshape2)
library(fishualize)
library(formattable)
library(qiime2R)

#Update source file
CAL<-read.csv("H:/Shared drives/Donahue Lab Data Drive/Jessica Glazner_Donahue Lab Data Drive/NASTE - Nutrient Addition and Stress Tolerance Experiment/DATA/Odyssey (LIGHT)/NASTE_Light Data_ALL - Calibration.csv")
anova_test(LIGHT..PAR. ~ TANK, data = CAL)
CAL %>%
  ggplot( aes(x=Timepoint, y=LIGHT..PAR., group=TANK, color=TANK)) +
  geom_line() +
  ggtitle("Light Logger Calibration (PAR)") +
  ylab("Light (PAR)")+
  xlab("7/19 24HR Calibration Period")+
  labs(tag = "p=0.957") +
  theme(legend.position="bottom",plot.tag.position = "topright",plot.tag=element_text(size = 10))+
  guides(color = guide_legend(nrow = 1,title="Tank"))+
  scale_color_manual(values = c( "turquoise1","red","turquoise4", "red4"))+
  scale_x_continuous(breaks=c(0,48,96),labels=c('12AM', '12PM', '12AM'))

#Light data only extends to 9/18/24 for t-test because the heated tanks were broken down on this date      
MCAP<-read.csv("H:/Shared drives/Donahue Lab Data Drive/Jessica Glazner_Donahue Lab Data Drive/NASTE - Nutrient Addition and Stress Tolerance Experiment/DATA/Odyssey (LIGHT)/NASTE_Light Data_T-TEST - MCAP.csv")
t.test(LIGHT..PAR. ~ TANK, data = MCAP)
MCAP %>%
  ggplot( aes(x=Timepoint, y=LIGHT..PAR., group=TANK, color=TANK)) +
  geom_line() +
  ggtitle("MCAP Light (PAR)") +
  ylab("Light (PAR)")+
  xlab("Timepoint")+
  scale_color_manual(values = c( "turquoise1","red"))+
 labs(tag = "p=0.1011") +
  theme(legend.position="bottom",plot.tag.position = "topright",plot.tag=element_text(size = 10))+
  guides(color = guide_legend(nrow = 1,title="Tank"))+
  scale_x_continuous(breaks=c(0,2977,4700),labels=c('T0 7/24', 'T1 8/24', 'T2 9/11'))

PCOM<-read.csv("H:/Shared drives/Donahue Lab Data Drive/Jessica Glazner_Donahue Lab Data Drive/NASTE - Nutrient Addition and Stress Tolerance Experiment/DATA/Odyssey (LIGHT)/NASTE_Light Data_T-TEST - PCOM.csv")
t.test(LIGHT..PAR. ~ TANK, data = PCOM)
PCOM %>%
  ggplot( aes(x=Timepoint, y=LIGHT..PAR., group=TANK, color=TANK)) +
  geom_line() +
  ggtitle("PCOM Light (PAR)") +
  ylab("Light (PAR)")+
  xlab("Timepoint")+
  scale_color_manual(values = c( "turquoise3", "red3"))+
  labs(tag = "p=0.3213") +
  theme(legend.position="bottom",plot.tag.position = "topright",plot.tag=element_text(size = 10))+
  guides(color = guide_legend(nrow = 1,title="Tank"))+
  scale_x_continuous(breaks=c(0,2977,4700),labels=c('T0 7/24', 'T1 8/24', 'T2 9/11'))

ALL<-read.csv("H:/Shared drives/Donahue Lab Data Drive/Jessica Glazner_Donahue Lab Data Drive/NASTE - Nutrient Addition and Stress Tolerance Experiment/DATA/Odyssey (LIGHT)/NASTE_Light Data_ALL - MCAP_PCOM.csv")
ALL$Time <- NULL

Pcom<-subset(ALL, TANK!="MCAP AMB")
Pcom<-subset(Pcom, TANK!="MCAP HEAT")
Pcom$Date <- mdy(Pcom$Date)
Pcom<-Pcom %>%
  group_by(Date, TANK) %>%
  summarize(PAR = sum(LIGHT..PAR.)) 
Pcom$DLI <- with(Pcom, (PAR * 900) / 1000000)
important_dates_P <- as.Date(c("2023-07-26", "2023-08-21", "2023-09-11","2023-10-09"))
Pcom %>%
  ggplot( aes(x=Date, y=DLI, color=TANK)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(important_dates_P), 
             color = "lightgreen", linetype = "dashed", size = 1) +
  annotate(x = ymd("2023-07-26"), y = +Inf, label = "T0", vjust = 2, geom = "label")+ 
  annotate(x = ymd("2023-08-21"), y = +Inf, label = "T1", vjust = 2, geom = "label")+
  annotate(x = ymd("2023-09-11"), y = +Inf, label = "T2", vjust = 2, geom = "label")+
  annotate(x = ymd("2023-10-09"), y = +Inf, label = "T3", vjust = 2, geom = "label")+
  ylim(0,12)+
  ggtitle("PCOM Daily Light Integral") +
  ylab("Daily Light Integral (mol m^2 d^1)") +
  xlab("Date")+ 
  scale_color_manual(values = c( "turquoise3", "red3"))+
  theme_q2r()+
  theme(legend.position='bottom')+
  guides(color = guide_legend(nrow = 1,title="Temperature"))

Mcap<-subset(ALL, TANK!="PCOM AMB")
Mcap<-subset(Mcap, TANK!="PCOM HEAT")
Mcap$Date <- mdy(Mcap$Date)
Mcap<-Mcap %>%
  group_by(Date, TANK) %>%
  summarize(PAR = sum(LIGHT..PAR.))
Mcap$DLI <- with(Mcap, (PAR * 900) / 1000000)
important_dates_M <- as.Date(c("2023-07-26", "2023-08-21", "2023-09-08","2023-10-09"))
Mcap %>%
  ggplot( aes(x=Date, y=DLI, color=TANK)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(important_dates_M), 
             color = "lightgreen", linetype = "dashed", size = 1) +
  annotate(x = ymd("2023-07-26"), y = +Inf, label = "T0", vjust = 2, geom = "label")+ 
  annotate(x = ymd("2023-08-21"), y = +Inf, label = "T1", vjust = 2, geom = "label")+
  annotate(x = ymd("2023-09-11"), y = +Inf, label = "T2", vjust = 2, geom = "label")+
  annotate(x = ymd("2023-10-09"), y = +Inf, label = "T3", vjust = 2, geom = "label")+
  ylim(0,12)+
  ggtitle("MCAP Daily Light Integral") +
  ylab("Daily Light Integral (mol m^2 d^1)") +
  xlab("Date")+ 
  scale_color_manual(values = c( "turquoise3", "red3"))+
  theme_q2r()+
  theme(legend.position='bottom')+
  guides(color = guide_legend(nrow = 1,title="Temperature"))
