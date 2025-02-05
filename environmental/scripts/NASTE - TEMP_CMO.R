library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(reshape2)
library(formattable)
library(qiime2R)
library(cowplot)
library(here)

#Calibration Source file 
Cal <- read_csv(here("./data_all/environmental/iButton (TEMP)/ALL_Calibration.csv"))
anova_test(value ~ variable, data = Cal)
Cal %>%
  ggplot( aes(x=Date_Time, y=value, group=variable, color=variable)) +
  geom_line() +
  ggtitle("TEMP CALIBRATION") +
  ylab("Temp (C)")+
  xlab("7/19/2023")+
  labs(tag = "p=0.105") +
  theme(plot.tag.position = "topright",plot.tag=element_text(size = 10),axis.text.x = element_blank(),legend.position='bottom')+
  guides(color = guide_legend(nrow = 6,title="Loggers"))+
  theme_q2r()

CAL<-dcast(Cal,Date_Time~variable)
CAL$Date_Time <- NULL
MEAN<-data.frame(colMeans(CAL))
colnames(MEAN) <- c('Avg')
DELTA<-data.frame(MEAN %>%
  mutate(delta = Avg - last(Avg)))


#PCOM
#Update Source file
Pdelta<-DELTA %>% slice(25:49)
Pdelta$variable <- c("PA1", "PA10", "PA11", "PA12","PA2", "PA3", "PA4", "PA5", "PA6", "PA7", "PA8", "PA9","PCOM.SOURCE", 
                     "PH1","PH10", "PH11", "PH12", "PH2", "PH3", "PH4", "PH5", "PH6", "PH7", "PH8", "PH9" )
#Delete after fixing MCAP
PCOM<-read.csv("D:/GitHub/Nutrient-Addition-and-Stress-Tolerance-Experiment-/data_all/environmental/iButton (TEMP)/PCOM.csv")
PCOM<-melt(PCOM ,  id.vars = 'Date_Time')
write.csv(PCOM,"D:/GitHub/Nutrient-Addition-and-Stress-Tolerance-Experiment-/data_all/environmental/iButton (TEMP)/PCOM_All.csv")

Pcom <- read_csv(here("./data_all/environmental/iButton (TEMP)/PCOM_All.csv"))
Pcom <- merge(Pcom, Pdelta, by = "variable", all.x = TRUE)
Pcom$Avg <- NULL
Pcom<-data.frame(Pcom %>% 
                     mutate(true.value = value+delta))
Pcom$value <- NULL
Pcom$delta <- NULL
Pcom$Date_Time <- mdy_hm(Pcom$Date_Time)
Pcom_hourly_data <- Pcom %>%
  mutate(Date_Time = floor_date(Date_Time, "hour")) %>%  
  group_by(variable,Date_Time) %>%
  summarize(true.value = mean(true.value))
Pcom_hourly_data<-separate(data = Pcom_hourly_data, col = Date_Time, into = c("Date", "Time"), sep = " ")
PCOM.treatment<-Pcom_hourly_data %>%
  mutate(Treatment = case_when(variable %in% c("PA1", "PA2", "PA3") ~ "AMB-Guano",
                               variable %in% c("PA4", "PA5", "PA6") ~ "AMB-Control",
                               variable %in% c("PA7", "PA8", "PA9") ~ "AMB-Inorganic",
                               variable %in% c("PA10", "PA11", "PA12") ~ "AMB-Effluent",
                               variable %in% c("PH1", "PH2", "PH3") ~ "HEAT-Guano",
                               variable %in% c("PH4", "PH5", "PH6") ~ "HEAT-Control",
                               variable %in% c("PH7", "PH8", "PH9") ~ "HEAT-Inorganic",
                               variable %in% c("PH10", "PH11", "PH12") ~ "HEAT-Effluent",
                               variable %in% c("PCOM.SOURCE") ~ "PCOM.Source"
  ))

PCOM.treatment$true.value.30<-PCOM.treatment$true.value-30
PCOM.treatment$DHH = ifelse(PCOM.treatment$"true.value" <= 30,"0","1")
PCOM.treatment <- transform(PCOM.treatment,
                      DHH = as.numeric(DHH))
PCOM.treatment <- na.omit(PCOM.treatment)
PCOM.treatment<-PCOM.treatment %>%
  group_by(variable,Treatment,Date) %>%
  summarize(true.value = mean(true.value),
            DHH = sum(DHH)
            ) %>% 
  mutate(Cumulative_DHH = cumsum(DHH))%>%
  mutate_if(is.numeric, signif, digits=3)

P.true.h=PCOM.treatment[PCOM.treatment$variable %in% c("PH1", "PH2", "PH3", "PH4", "PH5", "PH6", "PH7", "PH8", "PH9", "PH10", "PH11", "PH12"), ]  
P.true.h$true.value.30<-P.true.h$true.value-30
P.true.h$DHScore = ifelse(P.true.h$"true.value.30" <= 0,"0",P.true.h$"true.value.30")
P.true.h <- transform(P.true.h,
                             DHScore = as.numeric(DHScore),
                             DHH = as.numeric(DHH))
P.true.h.sum<-P.true.h %>% 
  group_by(variable) %>%
  summarise_at(c('DHH','DHScore'),sum, na.rm=TRUE )%>%
  mutate_if(is.numeric, signif, digits=4)
P.true.h.sum$DHD<-P.true.h.sum$DHH/24
P.true.h.avg<-P.true.h.sum %>%
  mutate(Treatment = case_when(variable %in% c("PH1", "PH2", "PH3") ~ "Guano",
                               variable %in% c("PH4", "PH5", "PH6") ~ "Control",
                               variable %in% c("PH7", "PH8", "PH9") ~ "Inorganic",
                               variable %in% c("PH10", "PH11", "PH12") ~ "Effluent"
                               ))
P.true.h.avg <- P.true.h.avg %>%
  mutate(across(everything(), ~ replace(., . == 0, mean(., na.rm = TRUE))))%>%
  mutate_if(is.numeric, signif, digits=4)
anova_test(DHH ~ Treatment, data = P.true.h.avg)
P.true.h.Treatment<-P.true.h.avg %>% 
  group_by(Treatment) %>% 
  summarise_at(c('DHH','DHScore','DHD'),mean, na.rm=TRUE )%>%
  mutate_if(is.numeric, signif, digits=4)

#PCOM DAILY HEATING 
date_ranges_PCOM <- data.frame(
  start_date = as.Date(c("2023-07-24")),
  end_date = as.Date(c("2023-09-05", "2023-09-07","2023-09-08","2023-09-09","2023-09-10","2023-09-11"))
)
aggregate_list_PCOM <- list()
for(i in 1:nrow(date_ranges_PCOM)) {
  filtered_data <- P.true.h %>%
    filter(as.Date(Date) >= date_ranges$start_date[i] &
             as.Date(Date) <= date_ranges$end_date[i])
  aggregate_data <- filtered_data %>%
    group_by(variable,Treatment) %>% 
    summarize(   # or use mean(), median(), etc.
      DHScore = sum(DHScore, na.rm = TRUE),
      Cumulative_DHH = max(Cumulative_DHH, na.rm = TRUE)
    )
  aggregate_data$start_date <- date_ranges$start_date[i]
  aggregate_data$end_date <- date_ranges$end_date[i]
  aggregate_list_PCOM[[i]] <- aggregate_data
}
final_aggregate_PCOM <- bind_rows(aggregate_list_PCOM)
final_aggregate_PCOM <- final_aggregate_PCOM %>%
  group_by(Treatment,end_date) %>%
  mutate(DHScore = if_else(DHScore == 0, mean(DHScore[DHScore != 0], na.rm = TRUE), DHScore),
         Cumulative_DHH = if_else(Cumulative_DHH == 0, mean(Cumulative_DHH[Cumulative_DHH != 0], na.rm = TRUE), Cumulative_DHH)
         ) %>%
  ungroup()
write.csv(final_aggregate_PCOM,"D:/GitHub/Nutrient-Addition-and-Stress-Tolerance-Experiment-/data_all/environmental/iButton (TEMP)/PCOM_Aquaria_Temp_Daily.csv")
final_aggregate_PCOM_treatment <- final_aggregate_PCOM %>%
  group_by(Treatment,end_date)%>%
  summarise(DHScore=mean(DHScore),
            Cumulative_DHH=mean(Cumulative_DHH),
            start_date=max(start_date))
write.csv(final_aggregate_PCOM,"D:/GitHub/Nutrient-Addition-and-Stress-Tolerance-Experiment-/data_all/environmental/iButton (TEMP)/PCOM_Treatment_Temp_Daily.csv")

#Line plots for PCOM
important_dates_P <- as.Date(c("2023-07-26", "2023-08-21", "2023-09-11","2023-10-09"))
P.plot1 <-PCOM.treatment %>%
  ggplot( aes(x=Date, y=true.value, color=Treatment)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(important_dates_P), 
             color = "red", linetype = "dashed", size = 1) +
  annotate(x = ymd("2023-07-26"), y = +Inf, label = "T0", vjust = 2, geom = "label")+ 
  annotate(x = ymd("2023-08-21"), y = +Inf, label = "T1", vjust = 2, geom = "label")+
  annotate(x = ymd("2023-09-11"), y = +Inf, label = "T2", vjust = 2, geom = "label")+
  annotate(x = ymd("2023-10-09"), y = +Inf, label = "T3", vjust = 2, geom = "label")+
  ylim(25,32)+
  ggtitle("PCOM DAILY TEMP") +
  ylab("Temp (C)") +
  xlab("Date")+ 
  theme_q2r()+
  theme(legend.position='none',
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
P.plot1
P.plot2<-P.com.treatment %>%
  ggplot( aes(x=Date, y=Cumulative_DHH, color=Treatment)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(important_dates_P), 
             color = "red", linetype = "dashed", size = 1)+
  ylim(0,230)+
  ylab("Cumulative DHH") +
  xlab("Date")+ 
  theme_q2r()+
  theme(legend.position='bottom')+
  guides(color = guide_legend(nrow = 2,title="Treatment"))
P.plot2
plot_grid(P.plot1,P.plot2,nrow=2)
P.true %>%
  ggplot( aes(x=Timepoint, y=true.value, group=variable, color=variable)) +
  geom_line() +
  ylim(22,32.5)+
  ggtitle("PCOM HOURLY TEMP") +
  ylab("Temp (C)") +
  xlab("Timepoints")+ 
  theme_q2r()+
  theme(legend.position='bottom')+
  guides(color = guide_legend(nrow = 3,title="Aquaria"))+
  scale_x_continuous(breaks=c(0,840,1340,1827),labels=c('T0 7/24', 'T1 8/21', 'T2 9/11','T3 10/9'))
P.true.treatment %>%
  ggplot( aes(x=Timepoint, y=true.value, group=Treatment, color=Treatment)) +
  geom_line() +
  ylim(22,32.5)+
  ggtitle("PCOM HOURLY TEMP") +
  ylab("Temp (C)") +
  xlab("Timepoints")+ 
  theme_q2r()+
  theme(legend.position='bottom')+
  guides(color = guide_legend(nrow = 3,title="Aquaria"))+
  scale_x_continuous(breaks=c(0,840,1340,1827),labels=c('T0 7/24', 'T1 8/21', 'T2 9/11','T3 10/9'))

#Bar plots for PCOM-All Aquaria
P.true.h.avg %>%
  ggplot( aes(x=DHH, y=variable, fill=Treatment)) +
  geom_col() +
  ggtitle("PCOM Degree Heating Hours - All Aquaira") +
  xlab("Degree Heating Hours")+ 
  ylab("Aquaria")+
  theme_q2r()
P.true.h.avg %>%
  ggplot( aes(x=DHScore, y=variable, fill=Treatment)) +
  geom_col() +
  ggtitle("PCOM Degree Score - All Aquaria") +
  xlab("Degree Heating Score")+ 
  ylab("Aquaria")+
  theme_q2r()
P.true.h.avg %>%
  ggplot( aes(x=DHD, y=variable, fill=Treatment)) +
  geom_col() +
  ggtitle("PCOM Degree Heating Days - All Aquaria") +
  xlab("Degree Heating Days")+
  ylab("Aquaria")+
  theme_q2r()

#Bar plots for PCOM-Treatment
P.true.h.Treatment %>%
  ggplot( aes(x=DHH, y=Treatment, fill=Treatment)) +
  geom_col() +
  ggtitle("PCOM Degree Heating Hours - Average") +
  xlab("Degree Heating Hours")+ 
  theme_q2r()
P.true.h.Treatment %>%
  ggplot( aes(x=DHScore, y=Treatment, fill=Treatment)) +
  geom_col() +
  ggtitle("PCOM Degree Heating Score - Average") +
  xlab("Degree Heating Score")+ 
  theme_q2r()
P.true.h.Treatment %>%
  ggplot( aes(x=DHD, y=Treatment, fill=Treatment)) +
  geom_col() +
  ggtitle("PCOM Degree Heating Days - Average") +
  xlab("Degree Heating Days")+ 
  theme_q2r()




#MCAP
#Update Source File
Mdelta<-DELTA %>% slice(2:25)
Mdelta$variable <- c("MA1", "MA2", "MA3", "MA4", "MA6", "MA7", "MA8", "MA9", "MA10", "MA11", "MA12",
                     "MH1", "MH2", "MH3", "MH4", "MH5", "MH6", "MH7", "MH8", "MH9", "MH10", "MH11", "MH12","MCAP.SOURCE")
MCAP<-read.csv("H:/Shared drives/Donahue Lab Data Drive/Jessica Glazner_Donahue Lab Data Drive/NASTE - Nutrient Addition and Stress Tolerance Experiment/DATA/iButton (TEMP)/NASTE_Temp Data_ALL - 7.24-10.8.2023 MCAP_AVG.csv")
M.cap <- melt(MCAP ,  id.vars = 'Date')
M.cap<-subset(M.cap, variable!="Hour")
M.cap<-subset(M.cap, variable!="Timepoint")
M.cap<-subset(M.cap, variable!="MCAP.SOURCE")
M.cap <- merge(M.cap, Mdelta, by = "variable", all.x = TRUE)
M.cap$Avg <- NULL
M.cap<-data.frame(M.cap %>% 
                    mutate(true.value = value+delta))
M.cap$Date <- mdy(M.cap$Date)
MCAP.treatment<-M.cap %>%
  mutate(Treatment = case_when(variable %in% c("PA1", "PA2", "PA3") ~ "AMB-Guano",
                               variable %in% c("PA4", "PA5", "PA6") ~ "AMB-Control",
                               variable %in% c("PA7", "PA8", "PA9") ~ "AMB-Inorganic",
                               variable %in% c("PA10", "PA11", "PA12") ~ "AMB-Effluent",
                               variable %in% c("PH1", "PH2", "PH3") ~ "HEAT-Guano",
                               variable %in% c("PH4", "PH5", "PH6") ~ "HEAT-Control",
                               variable %in% c("PH7", "PH8", "PH9") ~ "HEAT-Inorganic",
                               variable %in% c("PH10", "PH11", "PH12") ~ "HEAT-Effluent",
                               variable %in% c("PCOM.SOURCE") ~ "PCOM.Source"
  ))

MCAP.treatment$true.value.30<-MCAP.treatment$true.value-30
MCAP.treatment$DHH = ifelse(MCAP.treatment$"true.value" <= 30,"0","1")
MCAP.treatment <- transform(MCAP.treatment,
                            DHH = as.numeric(DHH))
M.cap.1<-MCAP.treatment %>%
  group_by(Date, variable) %>%
  summarize(true.value = mean(true.value),DHH = sum(DHH)) 
M.cap.treatment<-M.cap.1 %>%
  mutate(Treatment = case_when(variable %in% c("MA1", "MA2", "MA3") ~ "AMB-Guano",
                               variable %in% c("MA4", "MA5", "MA6") ~ "AMB-Control",
                               variable %in% c("MA7", "MA8", "MA9") ~ "AMB-Inorganic",
                               variable %in% c("MA10", "MA11", "MA12") ~ "AMB-Effluent",
                               variable %in% c("MH1", "MH2", "MH3") ~ "HEAT-Guano",
                               variable %in% c("MH4", "MH5", "MH6") ~ "HEAT-Control",
                               variable %in% c("MH7", "MH8", "MH9") ~ "HEAT-Inorganic",
                               variable %in% c("MH10", "MH11", "MH12") ~ "HEAT-Effluent"
  ))
M.cap.treatment <- na.omit(M.cap.treatment)
M.cap.treatment<-M.cap.treatment %>%
  filter( is.na(true.value) == FALSE, is.na(DHH) == FALSE) %>%
  group_by(Treatment,Date) %>%
  summarize(true.value = mean(true.value), DHH = mean(DHH)) %>% 
  mutate(Cumulative_DHH = cumsum(DHH))%>%
  mutate_if(is.numeric, signif, digits=3)
important_dates_M <- as.Date(c("2023-07-26", "2023-08-21", "2023-09-08","2023-10-09"))
M.plot1 <-M.cap.treatment %>%
  ggplot( aes(x=Date, y=true.value, color=Treatment)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(important_dates_M), 
             color = "red", linetype = "dashed", size = 1) +
  annotate(x = ymd("2023-07-26"), y = +Inf, label = "T0", vjust = 2, geom = "label")+ 
  annotate(x = ymd("2023-08-21"), y = +Inf, label = "T1", vjust = 2, geom = "label")+
  annotate(x = ymd("2023-09-11"), y = +Inf, label = "T2", vjust = 2, geom = "label")+
  annotate(x = ymd("2023-10-09"), y = +Inf, label = "T3", vjust = 2, geom = "label")+
  ylim(25,32)+
  ggtitle("MCAP DAILY TEMP") +
  ylab("Temp (C)") +
  xlab("Date")+ 
  theme_q2r()+
  theme(legend.position='none',
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
M.plot1
M.plot2<-M.cap.treatment %>%
  ggplot( aes(x=Date, y=Cumulative_DHH, color=Treatment)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(important_dates_M), 
             color = "red", linetype = "dashed", size = 1)+
  ylim(0,230)+
  ylab("Cumulative DHH") +
  xlab("Date")+ 
  theme_q2r()+
  theme(legend.position='bottom')+
  guides(color = guide_legend(nrow = 2,title="Treatment"))
M.plot2
plot_grid(M.plot1,M.plot2,nrow=2)







MCAP$Date <- NULL
MCAP$Hour <- NULL
Mcap <- melt(MCAP ,  id.vars = 'Timepoint')
M.All <- merge(Mcap, Mdelta, by = "variable", all.x = TRUE)
M.All$Avg <- NULL
M.true<-data.frame(M.All %>% 
                     mutate(true.value = value+delta))
Mcap %>%
  ggplot( aes(x=Timepoint, y=value, group=variable, color=variable)) +
  geom_line() +
  ylim(22,32.5)+
  ggtitle("MCAP HOURLY TEMP") +
  ylab("Temp (C)") +
xlab("Timepoints")+
  theme_q2r()+
  theme(legend.position='bottom')+
  guides(color = guide_legend(nrow = 3,title="Aquaria"))+
  scale_x_continuous(breaks=c(0,840,1340,1827),labels=c('T0 7/24', 'T1 8/21', 'T2 9/8','T3 10/9'))

M.true.h=M.true[M.true$variable %in% c("MH1", "MH2", "MH3", "MH4", "MH5", "MH6", "MH7", "MH8", "MH9", "MH10", "MH11", "MH12"), ]  
M.true.h$true.value.30<-M.true.h$true.value-30
M.true.h$DHH = ifelse(M.true.h$"true.value" <= 30,"0","1")
M.true.h$DHScore = ifelse(M.true.h$"true.value.30" <= 0,"0",M.true.h$"true.value.30")
M.true.h <- transform(M.true.h,
                      DHScore = as.numeric(DHScore),
                      DHH = as.numeric(DHH))
M.true.h.sum<-M.true.h %>% 
  group_by(variable) %>% 
  summarise_at(c('DHH','DHScore'),sum, na.rm=TRUE )%>% 
  mutate_if(is.numeric, signif, digits=4) 
M.true.h.sum$DHD<-M.true.h.sum$DHH/24
M.true.h.avg<-M.true.h.sum %>%
  mutate(Treatment = case_when(variable %in% c("MH1", "MH2", "MH3") ~ "Guano",
                               variable %in% c("MH4", "MH5", "MH6") ~ "Control",
                               variable %in% c("MH7", "MH8", "MH9") ~ "Inorganic",
                               variable %in% c("MH10", "MH11", "MH12") ~ "Effluent"
  ))
M.true.h.avg <- M.true.h.avg %>%
  mutate(across(everything(), ~ replace(., . == 0, mean(., na.rm = TRUE))))%>%
  mutate_if(is.numeric, signif, digits=4)
anova_test(DHH ~ Treatment, data = M.true.h.avg)

#Bar plots for MCAP-All Aquaria
M.true.h.avg %>%
  ggplot( aes(x=DHH, y=variable, fill=Treatment)) +
  geom_col() +
  ggtitle("MCAP Degree Heating Hours - All Aquaira") +
  xlab("Degree Heating Hours")+ 
  ylab("Aquaria")+
  theme_q2r()
M.true.h.avg %>%
  ggplot( aes(x=DHScore, y=variable, fill=Treatment)) +
  geom_col() +
  ggtitle("MCAP Degree Score - All Aquaria") +
  xlab("Degree Heating Score")+ 
  ylab("Aquaria")+
  theme_q2r()
M.true.h.avg %>%
  ggplot( aes(x=DHD, y=variable, fill=Treatment)) +
  geom_col() +
  ggtitle("MCAP Degree Heating Days - All Aquaria") +
  xlab("Degree Heating Days")+
  ylab("Aquaria")+
  theme_q2r()

#Run before moving past the Bar plots
M.true.h.Treatment<-M.true.h.avg %>% 
  group_by(Treatment) %>% 
  summarise_at(c('DHH','DHScore','DHD'),mean, na.rm=TRUE )

#Bar plots for MCAP-Treatment
M.true.h.Treatment %>%
  ggplot( aes(x=DHH, y=Treatment, fill=Treatment)) +
  geom_col() +
  ggtitle("MCAP Degree Heating Hours - Average") +
  xlab("Degree Heating Hours")+ 
  theme_q2r()
M.true.h.Treatment %>%
  ggplot( aes(x=DHScore, y=Treatment, fill=Treatment)) +
  geom_col() +
  ggtitle("MCAP Degree Heating Score - Average") +
  xlab("Degree Heating Score")+ 
  theme_q2r()
M.true.h.Treatment %>%
  ggplot( aes(x=DHD, y=Treatment, fill=Treatment)) +
  geom_col() +
  ggtitle("MCAP Degree Heating Days - Average") +
  xlab("Degree Heating Days")+ 
  theme_q2r()

#MCAP DAILY HEATING 
M.true.h.sum.8.29<-M.true.h %>% 
  group_by(variable) %>%
  filter((Timepoint == (1:882)))%>%
  summarise_at(c('DHH','DHScore'),sum, na.rm=TRUE )%>%
  mutate_if(is.numeric, signif, digits=4)
M.true.h.avg.8.29<-M.true.h.sum.8.29 %>%
  mutate(Treatment = case_when(variable %in% c("MH1", "MH2", "MH3") ~ "Guano",
                               variable %in% c("MH4", "MH5", "MH6") ~ "Control",
                               variable %in% c("MH7", "MH8", "MH9") ~ "Inorganic",
                               variable %in% c("MH10", "MH11", "MH12") ~ "Effluent"
  ))
M.true.h.avg.8.29 <- M.true.h.avg.8.29 %>%
  mutate(across(everything(), ~ replace(., . == 0, mean(., na.rm = TRUE))))%>%
  mutate_if(is.numeric, signif, digits=4)
M.true.h.Treatment.8.29<-M.true.h.avg.8.29 %>% 
  group_by(Treatment) %>% 
  summarise_at(c('DHH','DHScore'),mean, na.rm=TRUE )

M.true.h.sum.9.3<-M.true.h %>% 
  group_by(variable) %>%
  filter((Timepoint == (1:1002)))%>%
  summarise_at(c('DHH','DHScore'),sum, na.rm=TRUE )%>%
  mutate_if(is.numeric, signif, digits=4)
M.true.h.avg.9.3<-M.true.h.sum.9.3 %>%
  mutate(Treatment = case_when(variable %in% c("MH1", "MH2", "MH3") ~ "Guano",
                               variable %in% c("MH4", "MH5", "MH6") ~ "Control",
                               variable %in% c("MH7", "MH8", "MH9") ~ "Inorganic",
                               variable %in% c("MH10", "MH11", "MH12") ~ "Effluent"
  ))
M.true.h.avg.9.3 <- M.true.h.avg.9.3 %>%
  mutate(across(everything(), ~ replace(., . == 0, mean(., na.rm = TRUE))))%>%
  mutate_if(is.numeric, signif, digits=4)
M.true.h.Treatment.9.3<-M.true.h.avg.9.3 %>% 
  group_by(Treatment) %>% 
  summarise_at(c('DHH','DHScore'),mean, na.rm=TRUE )

M.true.h.sum.9.4<-M.true.h %>% 
  group_by(variable) %>%
  filter((Timepoint == (1:1026)))%>%
  summarise_at(c('DHH','DHScore'),sum, na.rm=TRUE )%>%
  mutate_if(is.numeric, signif, digits=4)
M.true.h.avg.9.4<-M.true.h.sum.9.4 %>%
  mutate(Treatment = case_when(variable %in% c("MH1", "MH2", "MH3") ~ "Guano",
                               variable %in% c("MH4", "MH5", "MH6") ~ "Control",
                               variable %in% c("MH7", "MH8", "MH9") ~ "Inorganic",
                               variable %in% c("MH10", "MH11", "MH12") ~ "Effluent"
  ))
M.true.h.avg.9.4 <- M.true.h.avg.9.4 %>%
  mutate(across(everything(), ~ replace(., . == 0, mean(., na.rm = TRUE))))%>%
  mutate_if(is.numeric, signif, digits=4)
M.true.h.Treatment.9.4<-M.true.h.avg.9.4 %>% 
  group_by(Treatment) %>% 
  summarise_at(c('DHH','DHScore'),mean, na.rm=TRUE )

M.true.h.sum.9.5<-M.true.h %>% 
  group_by(variable) %>%
  filter((Timepoint == (1:1040)))%>%
  summarise_at(c('DHH','DHScore'),sum, na.rm=TRUE )%>%
  mutate_if(is.numeric, signif, digits=4)
M.true.h.avg.9.5<-M.true.h.sum.9.5 %>%
  mutate(Treatment = case_when(variable %in% c("MH1", "MH2", "MH3") ~ "Guano",
                               variable %in% c("MH4", "MH5", "MH6") ~ "Control",
                               variable %in% c("MH7", "MH8", "MH9") ~ "Inorganic",
                               variable %in% c("MH10", "MH11", "MH12") ~ "Effluent"
  ))
M.true.h.avg.9.5 <- M.true.h.avg.9.5 %>%
  mutate(across(everything(), ~ replace(., . == 0, mean(., na.rm = TRUE))))%>%
  mutate_if(is.numeric, signif, digits=4)
M.true.h.Treatment.9.5<-M.true.h.avg.9.5 %>% 
  group_by(Treatment) %>% 
  summarise_at(c('DHH','DHScore'),mean, na.rm=TRUE )

M.true.h.sum.9.6<-M.true.h %>% 
  group_by(variable) %>%
  filter((Timepoint == (1:1064)))%>%
  summarise_at(c('DHH','DHScore'),sum, na.rm=TRUE )%>%
  mutate_if(is.numeric, signif, digits=4)
M.true.h.avg.9.6<-M.true.h.sum.9.6 %>%
  mutate(Treatment = case_when(variable %in% c("MH1", "MH2", "MH3") ~ "Guano",
                               variable %in% c("MH4", "MH5", "MH6") ~ "Control",
                               variable %in% c("MH7", "MH8", "MH9") ~ "Inorganic",
                               variable %in% c("MH10", "MH11", "MH12") ~ "Effluent"
  ))
M.true.h.avg.9.6 <- M.true.h.avg.9.6 %>%
  mutate(across(everything(), ~ replace(., . == 0, mean(., na.rm = TRUE))))%>%
  mutate_if(is.numeric, signif, digits=4)
M.true.h.Treatment.9.6<-M.true.h.avg.9.6 %>% 
  group_by(Treatment) %>% 
  summarise_at(c('DHH','DHScore'),mean, na.rm=TRUE )

M.true.h.sum.9.7<-M.true.h %>% 
  group_by(variable) %>%
  filter((Timepoint == (1:1088)))%>%
  summarise_at(c('DHH','DHScore'),sum, na.rm=TRUE )%>%
  mutate_if(is.numeric, signif, digits=4)
M.true.h.avg.9.7<-M.true.h.sum.9.7 %>%
  mutate(Treatment = case_when(variable %in% c("MH1", "MH2", "MH3") ~ "Guano",
                               variable %in% c("MH4", "MH5", "MH6") ~ "Control",
                               variable %in% c("MH7", "MH8", "MH9") ~ "Inorganic",
                               variable %in% c("MH10", "MH11", "MH12") ~ "Effluent"
  ))
M.true.h.avg.9.7 <- M.true.h.avg.9.7 %>%
  mutate(across(everything(), ~ replace(., . == 0, mean(., na.rm = TRUE))))%>%
  mutate_if(is.numeric, signif, digits=4)
M.true.h.Treatment.9.7<-M.true.h.avg.9.7 %>% 
  group_by(Treatment) %>% 
  summarise_at(c('DHH','DHScore'),mean, na.rm=TRUE )%>%
  mutate_if(is.numeric, signif, digits=4)

M.true.h.avg$DHH8.29 = M.true.h.avg.8.29$DHH
M.true.h.avg$DHScore8.29 = M.true.h.avg.8.29$DHScore
M.true.h.avg$DHH9.3 = M.true.h.avg.9.3$DHH
M.true.h.avg$DHScore9.3 = M.true.h.avg.9.3$DHScore
M.true.h.avg$DHH9.4 = M.true.h.avg.9.4$DHH
M.true.h.avg$DHScore9.4 = M.true.h.avg.9.4$DHScore
M.true.h.avg$DHH9.5 = M.true.h.avg.9.5$DHH
M.true.h.avg$DHScore9.5 = M.true.h.avg.9.5$DHScore
M.true.h.avg$DHH9.6 = M.true.h.avg.9.6$DHH
M.true.h.avg$DHScore9.6 = M.true.h.avg.9.6$DHScore
M.true.h.avg$DHH9.7 = M.true.h.avg.9.7$DHH
M.true.h.avg$DHScore9.7 = M.true.h.avg.9.7$DHScore
M.true.h.avg<-M.true.h.avg %>%
  mutate_if(is.numeric, signif, digits=4)
write.csv(M.true.h.avg,"H:/Shared drives/Donahue Lab Data Drive/Jessica Glazner_Donahue Lab Data Drive/NASTE - Nutrient Addition and Stress Tolerance Experiment/DATA/iButton (TEMP)/MCAP All Aquaria Daily DHH and DHScore.csv", row.names = FALSE)

M.true.h.Treatment$DHH8.29 = M.true.h.Treatment.8.29$DHH
M.true.h.Treatment$DHScore8.29 = M.true.h.Treatment.8.29$DHScore
M.true.h.Treatment$DHH9.3 = M.true.h.Treatment.9.3$DHH
M.true.h.Treatment$DHScore9.3 = M.true.h.Treatment.9.3$DHScore
M.true.h.Treatment$DHH9.4 = M.true.h.Treatment.9.4$DHH
M.true.h.Treatment$DHScore9.4 = M.true.h.Treatment.9.4$DHScore
M.true.h.Treatment$DHH9.5 = M.true.h.Treatment.9.5$DHH
M.true.h.Treatment$DHScore9.5 = M.true.h.Treatment.9.5$DHScore
M.true.h.Treatment$DHH9.6 = M.true.h.Treatment.9.6$DHH
M.true.h.Treatment$DHScore9.6 = M.true.h.Treatment.9.6$DHScore
M.true.h.Treatment$DHH9.7 = M.true.h.Treatment.9.7$DHH
M.true.h.Treatment$DHScore9.7 = M.true.h.Treatment.9.7$DHScore
M.true.h.Treatment<-M.true.h.Treatment %>%
  mutate_if(is.numeric, signif, digits=4)
write.csv(M.true.h.Treatment,"H:/Shared drives/Donahue Lab Data Drive/Jessica Glazner_Donahue Lab Data Drive/NASTE - Nutrient Addition and Stress Tolerance Experiment/DATA/iButton (TEMP)/MCAP Daily DHH and DHScore.csv", row.names = FALSE)
