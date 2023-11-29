library(tidyverse)
library(data.table)
library(lubridate)
library(hms)
#setwd("C:/Users/l.dorsa/Desktop/Lambo/Lambo_complete")
myfiles = list.files(pattern="*.csv", full.names=TRUE)
myfiles

calculate_km_per_series <- function(i) { 
  #Lettura file
  df <- fread(i, select = c("vin","time","latitude","longitude","km_tachometer","speed"))
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
  df1 <- df1 %>% select(speed, time)%>% mutate(vin = vin1, time = time+1, latitude = NA, longitude = NA, km_tachometer = NA, speed=0)
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
  
  #-----------------------------------------------------------------------------
  df <- df %>%
    select(time, km_tachometer, series)
  df <- df[order(df$time),]  
  df <- df %>%
    fill(km_tachometer, .direction = "down")%>%
    mutate(km_tachometer = ifelse(km_tachometer < 1048573, km_tachometer, NA))%>%
    filter(!is.na(km_tachometer))%>%
    filter(!is.na(series))
  df <- df %>%
    mutate(km_start = ifelse(series =="stop", lag(km_tachometer), NA))%>%
    filter(series=="stop")%>%
    mutate(km_series = (km_tachometer - km_start))%>%
    filter(km_series > 0)
 df <- df %>%
   mutate(avg_km_series = mean(km_series, na.rm = TRUE))
 df <- data.frame(vin = vin1, avg_km_series = df[1,6])
}

km_series2 <- lapply(myfiles, calculate_km_per_series)
do.call(rbind, km_series2)
km_series2 <- do.call(rbind.data.frame, km_series2)

write.csv(km_series2, "C:/Users/l.dorsa/Desktop/Lambo/km_series2.csv")
cluster_green <- read_csv("C:/Users/l.dorsa/Desktop/Lambo/cluster_green.csv")
cluster_green <- cluster_green %>%
  left_join(km_series2)

cluster_green %>%
  ggplot(aes(x=as.factor(clusters5), y=avg_km_series))+
  geom_boxplot(aes(color = as.factor(clusters5)), lwd = 1)+
  theme_light()+ 
  xlab("Cluster")+
  ylab("Average kilometers per series")+
  stat_summary(geom="text", fun.y=quantile,
               aes(label=sprintf("%1.1f", ..y..), color=factor(clusters5)),
               position=position_nudge(x=0.5), size=5)+
  theme(axis.text.x = element_text(face="bold", 
                                   size=12),
        axis.text.y = element_text(face="bold",
                                   size=12),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(size=22, face = "bold"))+
  theme(plot.background = element_rect(fill = "#EEEEEE"), 
        legend.position = "none", legend.background =     element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))
