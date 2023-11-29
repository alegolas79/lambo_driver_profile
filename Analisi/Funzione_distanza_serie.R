#setwd("C:/Users/l.dorsa/Desktop/Lambo/Lambo_complete")
myfiles = list.files(pattern="*.csv", full.names=TRUE)
myfiles


calculate_stop_length <- function(i) { 
  #-------------------------------------------------------------------------------  
  
  #Lettura file
  df <- fread(i, select = c("vin","time","latitude","longitude","altitude","speed"))
  df$time <- ymd_hms(df$time)
  
  #-------------------------------------------------------------------------------  
  
  #Parametri x selezione serie
  df_speed <- df %>% 
    filter(speed>0) %>%
    mutate(mean_speed = mean(speed), max_speed = max(speed), sd_speed = sd(speed))
  df_speed <- df_speed[1,]
  x <- 7200
  y <- (df_speed$mean_speed + df_speed$sd_speed)
  
  #-------------------------------------------------------------------------------  
  
  #aggiungi zeri nei punti di dataset mancanti
  df <- df %>%
    mutate(speed = ifelse(is.na(speed), 0, speed))
  df1 <- df %>% dplyr::select(time, speed) %>%
    filter(!is.na(speed))
  df1 <- df1[order(df1$time),]
  df1$speed_pre <- lag(df1$speed, 1)
  df1 <- df1 %>%  mutate(series = ifelse(speed>0 & speed_pre==0, "start",
                                         ifelse(speed==0 & speed_pre >0, "stop",NA)))
  df1 <- df1 %>%
    mutate(series2 = series)%>%
    fill(series2, .direction = "down")
  df1$time_succ <- lead(df1$time, 1)
  df1$series3 <- lead(df1$series2, 1)
  df1 <- df1 %>%
    mutate(time_diff = time_succ - time)%>%
    mutate(flag = ifelse(series2=="start" & time_diff>=60,1,0))
  df1 <- df1 %>% filter(flag==1)
  vin1 <- as.character(as.vector(df[1,1]))
  df1 <- df1 %>% select(speed, time)%>% mutate(vin = vin1, time = time+1, latitude = NA, longitude = NA, altitude = NA, speed=0)
  df1 <- df1[,c(3,2,4,5,6,1)]
  df <- df %>% rbind(df1)
  df <- df[order(df$time),]
  rm(df1)
  
  #-------------------------------------------------------------------------------
  
  #seleziona e colora serie
  df <- df %>% mutate(id = 1:nrow(df))
  df <- df %>% mutate(speed = ifelse(is.na(speed), 0, speed))
  df1 <- df %>% dplyr::select(time, speed, id) %>%
    filter(!is.na(speed))
  df1 <- df1[order(df1$time),]
  df1$speed_pre <- lag(df1$speed, 1)
  df1 <- df1 %>%  mutate(series = ifelse(speed>0 & speed_pre==0, "start",
                                         ifelse(speed==0 & speed_pre >0, "stop",
                                                NA)))
  df1 <- df1 %>% filter(!is.na(series))
  df1$time_stop_pred <- lag(df1$time, 1)
  df1$length_stop_pred <- ifelse(df1$series=="start", df1$time - df1$time_stop_pred, NA)
  df1$time_start_next <- lead(df1$time)
  df1$length_stop_next <- ifelse(df1$series=="stop", (df1$time_start_next - df1$time), NA)
  df1 <- df1[order(df1$time),]
  df1 <- df1 %>% filter(ifelse(series=="start", length_stop_pred >= x, length_stop_next>=x))
  df1$id_series <- ifelse(df1$series == "start", 1:nrow(df1), "stop") 
  df1 <- df1 %>% select(time, speed, series,id_series, id)
  df <- df %>% left_join(df1)
  df <- df[order(df$time),]  
  df <-df %>%
    fill(id_series, .direction = "down")
  df <- df %>%
    mutate(id_series = ifelse(id_series == "stop", NA, id_series))%>%
    group_by(id_series)%>%
    mutate(max_series = max(speed, na.rm = TRUE))%>%
    mutate(max_series = ifelse(is.na(id_series), 0, max_series))
  df <- df %>%
    ungroup()%>%
    mutate(col = ifelse(max_series <= y, "Red", "Green"))
  rm(df1)
  
  df1 <- df %>%
    filter(!is.na(series))
  df1 <- df1[order(df1$time),]
  df1$time_stop <- lag(df1$time)
  df1 <- df1 %>%
    filter(series=="start")
  df1 <- df1 %>%
    filter(date(time_stop)==date(time))%>%
    mutate(stop_length = (time - time_stop))%>%
    mutate(avg_stop_length = mean(stop_length, na.rm = TRUE))
  
df <- data.frame(vin = vin1, avg_stop_length = df1[1,14])
}

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

stop_2h <- lapply(myfiles,calculate_stop_length)
do.call(rbind, stop_2h)
stop_2h <- do.call(rbind.data.frame, stop_2h)

#write_csv(stop_2h, "stop_2h.csv")
#stop <- read_csv("stop_2h.csv")
cluster_green <- read_csv("C:/Users/l.dorsa/Desktop/Lambo/cluster_green.csv")
country2 <- read_csv("C:/Users/l.dorsa/Desktop/Lambo/country2.csv")
stop <- stop %>%
  select(vin, avg_stop_length)
cluster_green <- cluster_green %>%
  left_join(stop)%>%
  left_join(country2)
cluster_green$continent <- str_remove(cluster_green$country, "/.*")

cluster_green %>%
  filter(continent =="Europe")%>%
  ggplot(aes(as.factor(clusters5), avg_stop_length/3600, color = as.factor(clusters5)))+
  geom_boxplot(lwd = 1)+
  theme_light()+
  xlab("Cluster")+
  ylab("Average length of time between series (hours)")+
  stat_summary(geom="text", fun.y=median,
               aes(label=sprintf("%1.1f", ..y..), color=factor(clusters5)),
               position=position_nudge(x=0.5), size=5)+
  stat_summary(geom="text", fun.y=min,
               aes(label=sprintf("%1.1f", ..y..), color=factor(clusters5)),
               position=position_nudge(x=0.5), size=5)+
  stat_summary(geom="text", fun.y=max,
               aes(label=sprintf("%1.1f", ..y..), color=factor(clusters5)),
               position=position_nudge(x=0.5), size=5)+
  theme(axis.text.x = element_text(face="bold", 
                                   size=12),
        axis.text.y = element_text(face="bold",
                                   size=12),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(face="bold",size=22))+
  theme(legend.position="none",plot.background = element_rect(fill = "#EEEEEE"), legend.background =     element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))

km_giorno <- read_csv("C:/Users/l.dorsa/Desktop/Lambo/km_giorno.csv")
cluster_green <- cluster_green %>%
  left_join(km_giorno)
cluster_green %>%
  filter(continent =="Europe")%>%
  ggplot(aes(as.factor(clusters5), avg_daily_km, color = as.factor(clusters5)))+
  geom_boxplot(lwd = 1)+
  theme_light()+
  xlab("Cluster")+
  ylab("Average km travelled in a day")+
  stat_summary(geom="text", fun.y=median,
               aes(label=sprintf("%1.1f", ..y..), color=factor(clusters5)),
               position=position_nudge(x=0.5), size=5)+
  stat_summary(geom="text", fun.y=min,
               aes(label=sprintf("%1.1f", ..y..), color=factor(clusters5)),
               position=position_nudge(x=0.5), size=5)+
  stat_summary(geom="text", fun.y=max,
               aes(label=sprintf("%1.1f", ..y..), color=factor(clusters5)),
               position=position_nudge(x=0.5), size=5)+
  theme(axis.text.x = element_text(face="bold", 
                                   size=12),
        axis.text.y = element_text(face="bold",
                                   size=12),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(face="bold",size=22))+
  theme(legend.position="none",plot.background = element_rect(fill = "#EEEEEE"), legend.background =     element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))

confronto_km <- read_csv("C:/Users/l.dorsa/Desktop/Lambo/confronto_km.csv")
confronto_km <- confronto_km %>%
  select(c(vin, mean_drive_length_r_g, mean_day_length_r_g))

cluster_green <- cluster_green %>%
  left_join(confronto_km)

cluster_green %>%
  filter(continent =="Europe")%>%
  ggplot(aes(as.factor(clusters5), mean_day_length_r_g/3600, color = as.factor(clusters5)))+
  geom_boxplot(lwd = 1)+
  theme_light()+
  xlab("Cluster")+
  ylab("Average time spent travelling in a day (hours)")+
  stat_summary(geom="text", fun.y=median,
               aes(label=sprintf("%1.1f", ..y..), color=factor(clusters5)),
               position=position_nudge(x=0.5), size=5)+
  stat_summary(geom="text", fun.y=min,
               aes(label=sprintf("%1.1f", ..y..), color=factor(clusters5)),
               position=position_nudge(x=0.5), size=5)+
  stat_summary(geom="text", fun.y=max,
               aes(label=sprintf("%1.1f", ..y..), color=factor(clusters5)),
               position=position_nudge(x=0.5), size=5)+
  theme(axis.text.x = element_text(face="bold", 
                                   size=12),
        axis.text.y = element_text(face="bold",
                                   size=12),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(face="bold",size=22))+
  theme(legend.position="none",plot.background = element_rect(fill = "#EEEEEE"), legend.background =     element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))

cluster_green %>%
  filter(continent =="Europe")%>%
  ggplot(aes(as.factor(clusters5), mean_drive_length_r_g/60, color = as.factor(clusters5)))+
  geom_boxplot(lwd = 1)+
  theme_light()+
  xlab("Cluster")+
  ylab("Average series length (minutes)")+
  stat_summary(geom="text", fun.y=median,
               aes(label=sprintf("%1.1f", ..y..), color=factor(clusters5)),
               position=position_nudge(x=0.5), size=5)+
  stat_summary(geom="text", fun.y=min,
               aes(label=sprintf("%1.1f", ..y..), color=factor(clusters5)),
               position=position_nudge(x=0.5), size=5)+
  stat_summary(geom="text", fun.y=max,
               aes(label=sprintf("%1.1f", ..y..), color=factor(clusters5)),
               position=position_nudge(x=0.5), size=5)+
  theme(axis.text.x = element_text(face="bold", 
                                   size=12),
        axis.text.y = element_text(face="bold",
                                   size=12),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(face="bold",size=22))+
  theme(legend.position="none",plot.background = element_rect(fill = "#EEEEEE"), legend.background =     element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))





length_2h <- read_csv("length_2h.csv")
cluster_green <- read_csv("C:/Users/l.dorsa/Desktop/Lambo/cluster_green.csv")
country2 <- read_csv("C:/Users/l.dorsa/Desktop/Lambo/country2.csv")
cluster_green <- cluster_green %>%
  select(!mean_day_length_g)
length_2h <- length_2h %>%
  left_join(cluster_green)%>%
  filter(!is.na(clusters5))%>%
  left_join(country2)
length_2h$continent <- str_remove(length_2h$country, "/.*")

length_2h %>%
  filter(continent =="Europe")%>%
  ggplot(aes(as.factor(clusters5), mean_drive_length_r_g/3600, color = as.factor(clusters5)))+
  geom_boxplot(lwd = 1)+
  theme_light()+
  xlab("Cluster")+
  ylab("Average series length (hours)")+
  stat_summary(geom="text", fun.y=median,
               aes(label=sprintf("%1.1f", ..y..), color=factor(clusters5)),
               position=position_nudge(x=0.5), size=5)+
  stat_summary(geom="text", fun.y=min,
               aes(label=sprintf("%1.1f", ..y..), color=factor(clusters5)),
               position=position_nudge(x=0.5), size=5)+
  stat_summary(geom="text", fun.y=max,
               aes(label=sprintf("%1.1f", ..y..), color=factor(clusters5)),
               position=position_nudge(x=0.5), size=5)+
  theme(axis.text.x = element_text(face="bold", 
                                   size=12),
        axis.text.y = element_text(face="bold",
                                   size=12),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(face="bold",size=22))+
  theme(legend.position="none",plot.background = element_rect(fill = "#EEEEEE"), legend.background =     element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))
