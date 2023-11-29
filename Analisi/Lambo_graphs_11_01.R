library(readxl)
library(tidyverse)
library(plot3D)

data <- read_csv("C:/Users/l.dorsa/Desktop/Lambo/confronto_km2.csv")
data <- data %>% 
  mutate(tot_days = ifelse(is.na(tot_days), 0, tot_days))

VIN_di_TEST <- read_excel("C:/Users/l.dorsa/Downloads/BIG DATA 11-29-2021/VIN di TEST.xlsx")
VIN_di_TEST <- VIN_di_TEST %>% mutate(test = "test") %>% select(`VIN TEST`, test)
colnames(VIN_di_TEST)[1] <- "vin"
data <- data %>%
  left_join(VIN_di_TEST)
data <- data %>%
  mutate(test = ifelse(is.na(test), "not test", test))

data_test_free <- data %>%
  filter(test=="not test")
summary(data_test_free)

data <- data %>%
  left_join(country2)

data$continent <- str_remove(data$country, "/.*")


#-------------------------------------------------------------------------------
cluster_green <- read_csv("cluster_green.csv")
cluster_green <- cluster_green %>%
  select(vin, clusters5)
data <- data %>%
  left_join(cluster_green)
#-------------------------------------------------------------------------------
data %>%
  filter(test=="not test")%>%
  ggplot(aes(tot_days))+
  geom_area(stat="bin", bins=30, fill = "darkblue")+
  xlab("Numero di giorni di utilizzo dell'auto")+
  ylab("")+
  theme_light()
  
data %>%
  mutate(perc_days = ifelse(is.na(perc_days), 0, perc_days))%>%
  filter(test=="not test")%>%
  ggplot(aes(perc_days*100))+
  geom_histogram(stat="bin", bins=20, fill = "darkblue")+
  xlab("Percentuale di giorni di utilizzo dell'auto")+
  ylab("Numero di auto")+
  theme_light()+ 
  theme(axis.text.x = element_text(face="bold", 
                                   size=20),
        axis.text.y = element_text(face="bold",
                                   size=20),
        axis.title=element_text(size=20),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20))+
  theme(plot.background = element_rect(fill = "#EEEEEE"), legend.background =     element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))+
  scale_x_continuous(breaks = round(seq(0, 100, by = 5),1))


#-------------------------------------------------------------------------------
data <- data %>%
  mutate(perc_days = ifelse(is.na(perc_days), 0, perc_days))%>%
  mutate(perc_days100=100*perc_days)%>%
  mutate(perc = ifelse(perc_days100 <10, "0-10%", 
  ifelse(perc_days100>=10 & perc_days100<20, "10-20%", 
  ifelse(perc_days100>=20 & perc_days100<30, "20-30%",
  ifelse(perc_days100>=30 & perc_days100<40, "30-40%",
  ifelse(perc_days100>=40 & perc_days100<50, "40-50%",
  ifelse(perc_days100>=50 & perc_days100<60, "50-60%",
  ifelse(perc_days100>=60 & perc_days100<70, "60-70%", 
  ifelse(perc_days100>=70 & perc_days100<80, "70-80%",
  ifelse(perc_days100>=80 & perc_days100<90, "80-90%",
  "90-100%"))))))))))%>%
  select(!perc_days100)

data %>%
  mutate(perc_days = ifelse(is.na(perc_days), 0, perc_days))%>%
  mutate(perc_days=100*perc_days)%>%
  mutate(perc = ifelse(perc_days <10, "0-10%", 
         ifelse(perc_days>=10 & perc_days<20, "10-20%", 
         ifelse(perc_days>=20 & perc_days<30, "20-30%",
         ifelse(perc_days>=30 & perc_days<40, "30-40%",
         ifelse(perc_days>=40 & perc_days<50, "40-50%",
         ifelse(perc_days>=50 & perc_days<60, "50-60%",
         ifelse(perc_days>=60 & perc_days<70, "60-70%", 
         ifelse(perc_days>=70 & perc_days<80, "70-80%",
         ifelse(perc_days>=80 & perc_days<90, "80-90%",
         "90-100%"))))))))))%>%
  mutate(clusters5 = ifelse(clusters5==1, "Cluster 1", ifelse(clusters5==2, "Cluster 2", ifelse(clusters5==3, "Cluster 3", ifelse(clusters5==4, "Cluster 4", "Cluster 5")))))%>%
  filter(test=="not test")%>%
  filter(!is.na(clusters5))%>%
  ggplot(aes(perc))+
  geom_bar(aes(fill = clusters5))+
  xlab("Percentuale di giorni di utilizzo dell'auto")+
  ylab("Numero di auto")+
  facet_wrap(vars(clusters5))+
  facet_rep_wrap(vars(clusters5), repeat.tick.labels = 'x')+
  theme_light()+ 
  theme(axis.text.x = element_text(face="bold", 
                                   size=16),
        axis.text.y = element_text(face="bold",
                                   size=20),
        axis.title=element_text(size=20),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  theme(plot.background = element_rect(fill = "#EEEEEE"), legend.background =     element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))

data %>%
  mutate(perc_days = ifelse(is.na(perc_days), 0, perc_days))%>%
  mutate(perc_days=100*perc_days)%>%
  mutate(perc = ifelse(perc_days <10, "0-10%", 
                       ifelse(perc_days>=10 & perc_days<20, "10-20%", 
                              ifelse(perc_days>=20 & perc_days<30, "20-30%",
                                     ifelse(perc_days>=30 & perc_days<40, "30-40%",
                                            ifelse(perc_days>=40 & perc_days<50, "40-50%",
                                                   ifelse(perc_days>=50 & perc_days<60, "50-60%",
                                                          ifelse(perc_days>=60 & perc_days<70, "60-70%", 
                                                                 ifelse(perc_days>=70 & perc_days<80, "70-80%",
                                                                        ifelse(perc_days>=80 & perc_days<90, "80-90%",
                                                                               "90-100%"))))))))))%>%
  filter(test=="not test")%>%
  filter(continent %in% c("Europe", "America"))%>%
  ggplot(aes(perc))+
  geom_bar(fill="darkblue")+
  xlab("Percentuale di giorni di utilizzo dell'auto")+
  ylab("Numero di auto")+
  facet_grid(vars(continent), scales = "free_x")+
  theme_light()+ 
  theme(axis.text.x = element_text(face="bold", 
                                   size=16),
        axis.text.y = element_text(face="bold",
                                   size=20),
        axis.title=element_text(size=20),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20))+
  theme(legend.position = "none")+
  theme(plot.background = element_rect(fill = "#EEEEEE"), legend.background =     element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))

#-------------------------------------------------------------------------------
data <- data %>%
  mutate(speed_bins = as.factor(ifelse(avg_daily_max <20, "0-20", 
                      ifelse(avg_daily_max <40, "20-40",
                      ifelse(avg_daily_max < 60, "40-60", 
                      ifelse(avg_daily_max < 80, "60-80", 
                      ifelse(avg_daily_max < 100, "80-100", 
                      ifelse(avg_daily_max < 120, "100-120", 
                      ifelse(avg_daily_max < 140, "120-140", 
                      ifelse(avg_daily_max < 160, "140-160",
                      ifelse(avg_daily_max < 180, "160-180",
                      ifelse(avg_daily_max < 200, "180-200", 
                      ifelse(avg_daily_max < 220, "200-220", 
                      ifelse(avg_daily_max < 240, "220-240",
                      ifelse(avg_daily_max < 260, "240-260", ">260")))))))))))))))
data$speed_bins <- factor(data$speed_bins, levels = c("0-20", 
                                                      "20-40",
                                                      "40-60",
                                                      "60-80", 
                                                      "80-100",
                                                      "100-120",
                                                      "120-140",
                                                      "140-160",
                                                      "160-180",
                                                      "180-200", 
                                                      "200-220", 
                                                      "220-240",
                                                      "240-260",
                                                      ">260"))
library(lemon)
data%>%
  mutate(clusters5 = ifelse(clusters5==1, "Cluster 1", ifelse(clusters5==2, "Cluster 2", ifelse(clusters5==3, "Cluster 3", ifelse(clusters5==4, "Cluster 4", "Cluster 5")))))%>%
  filter(!is.na(avg_daily_max))%>%
  filter(test=="not test")%>%
  filter(!is.na(clusters5))%>%
  ggplot(aes(x = speed_bins))+
  geom_bar(aes(fill=clusters5))+
  xlab("Velocità massima (km/h)")+
  ylab("Numero di auto")+
  theme_light()+ 
  facet_wrap(vars(clusters5))+
  facet_rep_wrap(vars(clusters5), repeat.tick.labels = 'x')+
  theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust=1))+
  theme(axis.text.x = element_text(size=16),
        axis.text.y = element_text(face="bold",
                                   size=20),
        axis.title=element_text(size=20),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20))+
  theme(legend.position = "none")+
  theme(plot.background = element_rect(fill = "#EEEEEE"), legend.background =     element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))

data%>%
  filter(!is.na(avg_daily_max))%>%
  filter(test=="not test")%>%
  filter(continent %in% c("America", "Europe"))%>%
  ggplot(aes(x = speed_bins))+
  geom_bar(fill="darkblue")+
  xlab("Velocità massima (km/h)")+
  ylab("Numero di auto")+
  theme_light()+ 
  facet_grid(vars(continent))+
  theme(axis.text.x = element_text(size=16),
        axis.text.y = element_text(face="bold",
                                   size=20),
        axis.title=element_text(size=20),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20))+
  theme(legend.position = "none")+
  theme(plot.background = element_rect(fill = "#EEEEEE"), legend.background =     element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))


#-------------------------------------------------------------------------------
data <- data %>%
  mutate(km_bins = ifelse(km_percorsi <1000, "0-1",
                   ifelse(km_percorsi <2000, "1-2",
                   ifelse(km_percorsi <3000, "2-3",
                   ifelse(km_percorsi <4000, "3-4",
                   ifelse(km_percorsi <5000, "4-5",
                   ifelse(km_percorsi <6000, "5-6",
                   ifelse(km_percorsi <7000, "6-7",
                   ifelse(km_percorsi <8000, "7-8",
                   ifelse(km_percorsi <9000, "8-9",
                   ifelse(km_percorsi <10000, "9-10",
                   ifelse(km_percorsi <11000, "10-11",  
                   ifelse(km_percorsi <12000, "11-12",
                   ifelse(km_percorsi <13000, "12-13",
                   ifelse(km_percorsi <14000, "13-14",
                   ifelse(km_percorsi <15000, "14-15",
                   ifelse(km_percorsi <16000, "15-16",
                   ifelse(km_percorsi <17000, "16-17",
                   ifelse(km_percorsi <18000, "17-18",
                   ifelse(km_percorsi <19000, "18-19",
                   ifelse(km_percorsi <20000, "19-20",
                   ifelse(km_percorsi <21000, "20-21",
                   ifelse(km_percorsi <22000, "21-22",
                   ifelse(km_percorsi <23000, "22-23",
                   ifelse(km_percorsi <24000, "23-24",
                   ifelse(km_percorsi <25000, "24-25",
                   ifelse(km_percorsi <26000, "25-26",
                   ifelse(km_percorsi <27000, "26-27",
                   ifelse(km_percorsi <28000, "27-28",
                   ifelse(km_percorsi <29000, "28-29",
                   ifelse(km_percorsi <30000, "29-30",
                          ">30")))))))))))))))))))))))))))))))

data$km_bins <- factor(data$km_bins, levels = c("0-1",
                                                "1-2",
                                                "2-3",
                                                "3-4",
                                                "4-5",
                                                "5-6",
                                                "6-7",
                                                "7-8",
                                                "8-9",
                                                "9-10",
                                                "10-11",  
                                                "11-12",
                                                "12-13",
                                                "13-14",
                                                "14-15",
                                                "15-16",
                                                "16-17",
                                                "17-18",
                                                "18-19",
                                                "19-20",
                                                "20-21",
                                                "21-22",
                                                "22-23",
                                                "23-24",
                                                "24-25",
                                                "25-26",
                                                "27-28",
                                                "28-29",
                                                "29-30",
                                                 ">30"))
data %>%
  filter(test=="not test")%>%
  ggplot(aes(km_bins))+
  geom_bar(fill = "darkblue")+
  xlab("Km totali percorsi (migliaia)")+
  ylab("Numero di auto")+
  theme_light()+ 
  theme(axis.text.x = element_text(face="bold", 
                                   size=20),
        axis.text.y = element_text(face="bold",
                                   size=20),
        axis.title=element_text(size=20),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20))+
  theme(plot.background = element_rect(fill = "#EEEEEE"), legend.background =     element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))
data%>%
  mutate(clusters5 = ifelse(clusters5==1, "Cluster 1", ifelse(clusters5==2, "Cluster 2", ifelse(clusters5==3, "Cluster 3", ifelse(clusters5==4, "Cluster 4", "Cluster 5")))))%>%
  filter(!is.na(avg_daily_max))%>%
  filter(test=="not test")%>%
  filter(!is.na(clusters5))%>%
  ggplot(aes(x = km_bins))+
  geom_bar(aes(fill=clusters5))+
  xlab("Km totali percorsi (migliaia)")+
  ylab("Numero di auto")+
  theme_light()+ 
  facet_wrap(vars(clusters5))+
  facet_rep_wrap(vars(clusters5), repeat.tick.labels = 'x')+
  theme(axis.text.x = element_text(size=16),
        axis.text.y = element_text(face="bold",
                                   size=20),
        axis.title=element_text(size=20),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20))+
  theme(legend.position = "none")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  theme(plot.background = element_rect(fill = "#EEEEEE"), legend.background =     element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))

data%>%
  filter(!is.na(avg_daily_max))%>%
  filter(test=="not test")%>%
  filter(continent %in% c("America", "Europe"))%>%
  ggplot(aes(x = km_bins))+
  geom_bar(fill="darkblue")+
  xlab("Km totali percorsi (migliaia)")+
  ylab("Numero di auto")+
  theme_light()+ 
  facet_grid(vars(continent))+
  theme(axis.text.x = element_text(size=16),
        axis.text.y = element_text(face="bold",
                                   size=20),
        axis.title=element_text(size=20),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20))+
  theme(legend.position = "none")+
  theme(plot.background = element_rect(fill = "#EEEEEE"), legend.background =     element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))

#-------------------------------------------------------------------------------
data <- data %>%
  mutate(avg_day_hours = mean_day_length_g/3600)%>%
  mutate(day_bins = ifelse(avg_day_hours <0.25, "0-15m",
                     ifelse(avg_day_hours <0.50, "15-30m", 
                     ifelse(avg_day_hours<0.75, "30-45m",
                     ifelse(avg_day_hours<1, "45m-1h", 
                     ifelse(avg_day_hours<1.25, "1h-1h15m",
                     ifelse(avg_day_hours<1.5, "1h15m-1h30m",
                     ifelse(avg_day_hours<1.75, "1h30m-1h45m", 
                     ifelse(avg_day_hours<2, "1h45m-2h",
                     ifelse(avg_day_hours<2.25, "2h-2h15m",
                     ifelse(avg_day_hours<2.5, "2h15m-2h30m",
                     ifelse(avg_day_hours<2.75, "2h30m-2h45m", 
                     ifelse(avg_day_hours<3, "2h45m-3h",
                     ifelse(avg_day_hours<4, "3h-4h", 
                     ">4h"))))))))))))))      
data$day_bins <- factor(data$day_bins, levels = c("0-15m",
                                                  "15-30m",
                                                  "30-45m",
                                                  "45m-1h", 
                                                  "1h-1h15m",
                                                  "1h15m-1h30m",
                                                  "1h30m-1h45m",
                                                  "1h45m-2h",
                                                  "2h-2h15m",
                                                  "2h15m-2h30m",
                                                  "2h30m-2h45m", 
                                                  "2h45m-3h",
                                                  "3h-4h", 
                                                  ">4h"))
data %>%
  filter(test=="not test")%>%
  filter(!is.na(mean_day_length_g))%>%
  ggplot(aes(day_bins))+
  geom_bar(fill = "darkblue")+
  xlab("Ore di guida medie al giorno")+
  ylab("Numero di auto")+
  theme_light()+ 
  theme(axis.text.x = element_text(face="bold", 
                                   size=18),
        axis.text.y = element_text(face="bold",
                                   size=20),
        axis.title=element_text(size=20),
        strip.text.x = element_text(size = 28, face = "bold"),
        strip.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20))+ 
 # theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust=1))+
  theme(plot.background = element_rect(fill = "#EEEEEE"), legend.background = element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))

data%>%
  mutate(clusters5 = ifelse(clusters5==1, "Cluster 1", ifelse(clusters5==2, "Cluster 2", ifelse(clusters5==3, "Cluster 3", ifelse(clusters5==4, "Cluster 4", "Cluster 5")))))%>%
  filter(!is.na(mean_day_length_g))%>%
  filter(test=="not test")%>%
  filter(!is.na(clusters5))%>%
  ggplot(aes(x = day_bins))+
  geom_bar(aes(fill=clusters5))+
  xlab("Tempo di guida medie al giorno")+
  ylab("Numero di auto")+
  theme_light()+ 
  facet_wrap(vars(clusters5))+
  facet_rep_wrap(vars(clusters5), repeat.tick.labels = 'x')+
  theme(axis.text.x = element_text(size=16),
        axis.text.y = element_text(face="bold",
                                   size=20),
        axis.title=element_text(size=20),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20))+
  theme(legend.position = "none")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  theme(plot.background = element_rect(fill = "#EEEEEE"), legend.background =     element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))

data%>%
  filter(!is.na(avg_day_hours))%>%
  filter(test=="not test")%>%
  filter(continent %in% c("America", "Europe"))%>%
  ggplot(aes(x = day_bins))+
  geom_bar(fill="darkblue")+
  xlab("Tempo di guida medio al giorno")+
  ylab("Numero di auto")+
  theme_light()+ 
  facet_grid(vars(continent))+
  theme(axis.text.x = element_text(size=16),
        axis.text.y = element_text(face="bold",
                                   size=20),
        axis.title=element_text(size=20),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20))+
  theme(legend.position = "none")+
  theme(plot.background = element_rect(fill = "#EEEEEE"), legend.background =     element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))

#-------------------------------------------------------------------------------
data <- data %>%
  mutate(speed_bins_avg = as.factor(ifelse(avg_daily_mean <10, "0-10",
                                    ifelse(avg_daily_mean <20, "10-20",
                                    ifelse(avg_daily_mean <30, "20-30",
                                    ifelse(avg_daily_mean <40, "30-40",
                                    ifelse(avg_daily_mean <50, "40-50",
                                    ifelse(avg_daily_mean <60, "50-60", 
                                    ifelse(avg_daily_mean <70, "60-70",
                                    ifelse(avg_daily_mean <80, "70-80",
                                    ifelse(avg_daily_mean <90, "80-90",
                                    ifelse(avg_daily_mean <100, "90-100", 
                                    ifelse(avg_daily_mean <110, "100-110",
                                    ifelse(avg_daily_mean <120, "110-120",
                                    ifelse(avg_daily_mean <130, "120-130","130-140")))))))))))))))
data$speed_bins_avg <- factor(data$speed_bins_avg, levels = c("0-10", 
                                                          "10-20", 
                                                          "20-30",
                                                          "30-40",
                                                          "40-50",
                                                          "50-60",
                                                          "60-70", 
                                                          "70-80", 
                                                          "80-90",
                                                          "90-100", 
                                                          "100-110",
                                                          "110-120", 
                                                          "120-130",
                                                          "130-140"))
data%>%
  filter(!is.na(avg_daily_mean))%>%
  filter(test=="not test")%>%
  ggplot(aes(x = speed_bins_avg))+
  geom_bar(fill = "darkblue")+
  xlab("Velocità media (km/h)")+
  ylab("Numero di auto")+
  theme_light()+ 
  theme(axis.text.x = element_text(face="bold", 
                                   size=20),
        axis.text.y = element_text(face="bold",
                                   size=20),
        axis.title=element_text(size=20),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20))+
  theme(plot.background = element_rect(fill = "#EEEEEE"), legend.background =     element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))

data%>%
  mutate(clusters5 = ifelse(clusters5==1, "Cluster 1", ifelse(clusters5==2, "Cluster 2", ifelse(clusters5==3, "Cluster 3", ifelse(clusters5==4, "Cluster 4", "Cluster 5")))))%>%
  filter(!is.na(avg_daily_max))%>%
  filter(test=="not test")%>%
  filter(!is.na(clusters5))%>%
  ggplot(aes(x = speed_bins_avg))+
  geom_bar(aes(fill=clusters5))+
  xlab("Velocità media (km/h)")+
  ylab("Numero di auto")+
  theme_light()+ 
  facet_wrap(vars(clusters5))+
  facet_rep_wrap(vars(clusters5), repeat.tick.labels = 'x')+
  theme(axis.text.x = element_text(size=16),
        axis.text.y = element_text(face="bold",
                                   size=20),
        axis.title=element_text(size=20),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  theme(plot.background = element_rect(fill = "#EEEEEE"), legend.background =     element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))

data%>%
  filter(!is.na(avg_daily_max))%>%
  filter(test=="not test")%>%
  filter(continent %in% c("America", "Europe"))%>%
  ggplot(aes(x = speed_bins_avg))+
  geom_bar(fill="darkblue")+
  xlab("Velocità media (km/h)")+
  ylab("Numero di auto")+
  theme_light()+ 
  facet_grid(vars(continent))+
  theme(axis.text.x = element_text(size=16),
        axis.text.y = element_text(face="bold",
                                   size=20),
        axis.title=element_text(size=20),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20))+
  theme(legend.position = "none")+
  theme(plot.background = element_rect(fill = "#EEEEEE"), legend.background =     element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))


#-------------------------------------------------------------------------------
data %>%
  filter(test=="not test")%>%
  ggplot(aes(avg_daily_mean))+
  geom_area(stat="bin", bins=30, fill = "darkblue")+
  xlab("Velocità media")+
  ylab("")+
  theme_light()

data %>%
  filter(test=="not test")%>%
  ggplot(aes(mean_day_length_g/3600))+
  geom_area(stat="bin", bins=30, fill = "darkblue")+
  xlab("Tempo medio passato in auto in un giorno")+
  ylab("")+
  theme_light()

data %>%
  filter(test=="not test")%>%
  ggplot(aes(mean_drive_length_g/3600))+
  geom_area(stat="bin", bins=30, fill = "darkblue")+
  xlab("Durata media di guida")+
  ylab("")+
  theme_light()

cluster_green <- read_csv("C:/Users/l.dorsa/Desktop/Lambo/cluster_green.csv")
cluster_green <- cluster_green %>% 
  select(c(clusters5, vin))
data <- data %>% 
  left_join(cluster_green)

data %>%
  filter(test=="not test")%>%
  filter(!is.na(clusters5))%>%
  mutate(clusters5 = ifelse(clusters5==1, "Cluster 1", ifelse(clusters5==2, "Cluster 2", ifelse(clusters5==3, "Cluster 3", ifelse(clusters5==4, "Cluster 4", "Cluster 5")))))%>%
  ggplot(aes(tot_days))+
  geom_area(aes(fill = clusters5), stat="bin", bins=30)+
  xlab("Numero di giorni di utilizzo dell'auto")+
  ylab("Numero di auto")+
  facet_wrap(vars(clusters5))+
  theme_light()+
  theme(legend.position = "none")+ 
  theme(axis.text.x = element_text(face="bold", 
                                   size=12),
        axis.text.y = element_text(face="bold",
                                   size=12),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12))+
  theme(plot.background = element_rect(fill = "#EEEEEE"), legend.background =     element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))


#-------------------------------------------------------------------------------
ctry <- data %>%
  select(country)
ctry <- ctry %>%
  filter(!is.na(country))%>%
  group_by(country)%>%
  count()
ctry <- read_xlsx("ctry.xlsx")
library(rworldmap)
ctry <- ctry %>%
  filter(!is.na(Longitude))
worldmap <- getMap(resolution = "low")
plot(worldmap, col = "#EEEEEE", 
     fill = T, border = "darkgray",
     xlim = c(-180, 180), ylim = c(-90, 90),
     bg = "aliceblue",
     asp = 1, wrap=c(-180,180))
points(ctry$Longitude, ctry$Latitude,
       # define colors as transparent
       col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5),
       # define size as number of flights div. by 50
       cex = ctry$n/30, pch = 20)

#------------------------------------------------------------------------------
eu <- data %>% filter(continent=="Europe")
am <- data %>% filter(continent=="America")

eu <- eu %>%
  mutate(avg_hours = mean_day_length_g/3600)%>%
  filter(test=="not test")%>%
  filter(!is.na(clusters5))
am <- am %>%
  mutate(avg_hours = mean_day_length_g/3600)%>%
  filter(test=="not test")%>%
  filter(!is.na(clusters5))


library(factoextra)
library(ggfortify)
library(GGally)

ggpairs(eu[c(2, 7, 4, 18, 13)], 
        columnLabels = c("Percentage of days car was used", "Average speed", "Average daily maximum speed","Average time spent driving in a day (hours)", "Total kilometers travelled"), aes(color =as.factor(eu$clusters5)))+
  theme_light()+ 
  theme(axis.text.x = element_text(face="bold", 
                                   size=12),
        axis.text.y = element_text(face="bold",
                                   size=12),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12, face = "bold"))

ggpairs(am[c(2, 7, 4, 18, 13)], 
        columnLabels = c("Percentage of days car was used", "Average speed", "Average daily maximum speed","Average time spent driving in a day (hours)", "Total kilometers travelled"), aes(color =as.factor(am$clusters5)))+
  theme_light()+ 
  theme(axis.text.x = element_text(face="bold", 
                                   size=12),
        axis.text.y = element_text(face="bold",
                                   size=12),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12, face = "bold"))
  

#-------------------------------------------------------------------------------

perc <- data %>%
  group_by(clusters5, perc_days)%>%
  count()%>%
  filter(!is.na(clusters5))

z <- perc %>%
  pivot_wider(names_from = perc, values_from = n)

library(plotly)

dens <- with(data, tapply(perc_days, INDEX = clusters5, density))
df <- data.frame(
  x = unlist(lapply(dens, "[[", "x")),
  y = unlist(lapply(dens, "[[", "y")),
  clusters5 = rep(names(dens), each = length(dens[[1]]$x)))

plot_ly(df, x = ~x, y = ~y, z = ~clusters5, type = 'scatter3d', mode = 'lines', color = ~clusters5)

library(plotly)
plot_ly(perc, x=~perc_days, y=~as.factor(clusters5), 
        z=~n, color=~as.factor(clusters5)) %>%
  add_markers(size=1) 

library(ggridges)
data%>%
  filter(!is.na(clusters5))%>%
  ggplot(aes(x = perc_days*100, y = as.factor(clusters5), fill=as.factor(clusters5))) +
  geom_density_ridges(scale = 4, alpha = 0.8) + 
  scale_y_discrete(expand = c(0, 0)) +     # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) +   # for both axes to remove unneeded padding
  coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
  theme_ridges()+
  xlab("")+ 
  ylab("Numero di auto")+
  theme(axis.title.x = element_text(hjust=0.5),
        axis.title.y = element_text(hjust=0.5))+
  ggtitle("")+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(face="bold", 
                                   size=20),
        axis.text.y = element_text(face="bold",
                                   size=12),
        axis.title=element_text(size=20))+
  theme(plot.background = element_rect(fill = "#EEEEEE"), legend.background = element_rect(fill = "#EEEEEE"))


data%>%
  filter(!is.na(clusters5))%>%
  ggplot(aes(x = avg_daily_max, y = as.factor(clusters5), fill=as.factor(clusters5))) +
  geom_density_ridges(scale = 4, alpha = 0.8) + 
  scale_y_discrete(expand = c(0, 0)) +     # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) +   # for both axes to remove unneeded padding
  coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
  theme_ridges()+
  xlab("")+ 
  ylab("Numero di auto")+
  theme(axis.title.x = element_text(hjust=0.5),
        axis.title.y = element_text(hjust=0.5))+
  ggtitle("")+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(face="bold", 
                                   size=20),
        axis.text.y = element_text(face="bold",
                                   size=12),
        axis.title=element_text(size=20))+
  theme(plot.background = element_rect(fill = "#EEEEEE"), legend.background = element_rect(fill = "#EEEEEE"))


data%>%
  filter(!is.na(clusters5))%>%
  ggplot(aes(x = overall_mean, y = as.factor(clusters5), fill=as.factor(clusters5))) +
  geom_density_ridges(scale = 4, alpha = 0.8) + 
  scale_y_discrete(expand = c(0, 0)) +     # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) +   # for both axes to remove unneeded padding
  coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
  theme_ridges()+
  xlab("")+ 
  ylab("Numero di auto")+
  theme(axis.title.x = element_text(hjust=0.5),
        axis.title.y = element_text(hjust=0.5))+
  ggtitle("")+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(face="bold", 
                                   size=20),
        axis.text.y = element_text(face="bold",
                                   size=12),
        axis.title=element_text(size=20))+
  theme(plot.background = element_rect(fill = "#EEEEEE"), legend.background = element_rect(fill = "#EEEEEE"))


data%>%
  filter(!is.na(clusters5))%>%
  ggplot(aes(x = mean_day_length_g/3600, y = as.factor(clusters5), fill=as.factor(clusters5))) +
  geom_density_ridges(scale = 4, alpha = 0.8) + 
  scale_y_discrete(expand = c(0, 0)) +     # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) +   # for both axes to remove unneeded padding
  coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
  theme_ridges()+
  xlab("")+ 
  ylab("Numero di auto")+
  theme(axis.title.x = element_text(hjust=0.5),
        axis.title.y = element_text(hjust=0.5))+
  ggtitle("")+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(face="bold", 
                                   size=20),
        axis.text.y = element_text(face="bold",
                                   size=12),
        axis.title=element_text(size=20))+
  theme(plot.background = element_rect(fill = "#EEEEEE"), legend.background = element_rect(fill = "#EEEEEE"))



data%>%
  filter(!is.na(clusters5))%>%
  ggplot(aes(x = km_percorsi, y = as.factor(clusters5), fill=as.factor(clusters5))) +
  geom_density_ridges(scale = 4, alpha = 0.8) + 
  scale_y_discrete(expand = c(0, 0)) +     # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) +   # for both axes to remove unneeded padding
  coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
  theme_ridges()+
  xlab("")+ 
  ylab("Numero di auto")+
  theme(axis.title.x = element_text(hjust=0.5),
        axis.title.y = element_text(hjust=0.5))+
  ggtitle("")+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(face="bold", 
                                   size=20),
        axis.text.y = element_text(face="bold",
                                   size=12),
        axis.title=element_text(size=20))+
  theme(plot.background = element_rect(fill = "#EEEEEE"), legend.background = element_rect(fill = "#EEEEEE"))

