library(tidyverse)
library(data.table)
library(lubridate)

cluster <- read.csv("C:/Users/l.dorsa/Desktop/Lambo/clusters.csv")
colnames(cluster)[1] <- "cluster"
summary(cluster)

setwd("C:/Users/l.dorsa/Desktop/Lambo/Lambo_complete")
myfiles = list.files(pattern="*.csv", full.names=TRUE)
myfiles
#Tyre pressure: sempre -6.3 o inesistente


variables_function <- function(i) {
  df <- fread(i, select = c("brake_pressure","throttle", "activ_launch_control",
                            "vin", "time", "roof_open", "roof_intermediate",
                            "coolant_temperature", "oil_temperature", "speed",
                            "acc_longitudinal", "acc_lateral",
                            "direction", "angular_speed", "torque",
                            "power_instant", "torque_instant", "rpm"))
  
  vin2 <- as.character(as.vector(df[1,4]))
  df$time <- ymd_hms(df$time)
  df1 <- df %>%
    filter(speed>0)
  
  if(nrow(df1)>0) {
  
    #Fault values
    df <- df %>%
    mutate(brake_pressure = ifelse(brake_pressure==276.9, NA, brake_pressure))%>%
    mutate(throttle= ifelse(throttle ==102, NA, throttle))%>%
    mutate(oil_temperature = ifelse(oil_temperature >= 193, NA, oil_temperature))%>%
    mutate(acc_longitudinal = ifelse(acc_longitudinal>1.627, NA, acc_longitudinal))%>%
    mutate(acc_lateral = ifelse(acc_lateral==1.28, NA, acc_lateral))%>%
    mutate(direction = ifelse(direction==819, NA, direction))%>%
    mutate(angular_speed = ifelse(angular_speed ==2550, NA, angular_speed ))%>%
    mutate(torque = ifelse(torque > 2040, NA, torque ))%>%
    mutate(power_instant = ifelse(power_instant > 1227, NA, power_instant))%>%
    mutate(torque_instant = ifelse(torque_instant > 2045, NA, torque_instant))%>%
    mutate(coolant_temperature = ifelse(coolant_temperature > 142, NA, coolant_temperature))
    
    #Daily maximums
    df <- df%>%
    mutate(date = date(time))%>%
    group_by(date)%>%
    mutate(daily_max_brake_pressure = max(brake_pressure, na.rm = TRUE), 
           daily_max_throttle = max(throttle, na.rm = TRUE),
           daily_max_oil_temp = max(oil_temperature, na.rm = TRUE),
           daily_max_acc_longitudinal = max(abs(acc_longitudinal), na.rm = TRUE),
           daily_max_acc_lateral = max(abs(acc_lateral), na.rm = TRUE),
           daily_max_direction = max(direction, na.rm = TRUE),
           daily_max_angular_speed = max(abs(angular_speed), na.rm = TRUE),
           daily_max_torque = max(abs(torque), na.rm = TRUE),
           daily_max_power_instant = max(abs(torque), na.rm = TRUE),
           daily_max_torque_instant = max(torque_instant, na.rm = TRUE),
           daily_max_rpm = max(rpm, na.rm = TRUE))%>%
    ungroup()%>%
      mutate(daily_max_brake_pressure = ifelse(is.finite(daily_max_brake_pressure)==TRUE, daily_max_brake_pressure, NA), 
             daily_max_throttle = ifelse(is.finite(daily_max_throttle)==TRUE, daily_max_throttle, NA),
             daily_max_oil_temp = ifelse(is.finite(daily_max_oil_temp)==TRUE, daily_max_oil_temp, NA),
             daily_max_acc_longitudinal = ifelse(is.finite(daily_max_acc_longitudinal)==TRUE, daily_max_acc_longitudinal, NA),
             daily_max_acc_lateral = ifelse(is.finite(daily_max_acc_lateral)==TRUE, daily_max_acc_lateral, NA),
             daily_max_direction = ifelse(is.finite(daily_max_direction)==TRUE, daily_max_direction, NA),
             daily_max_angular_speed = ifelse(is.finite(daily_max_angular_speed)==TRUE, daily_max_angular_speed, NA),
             daily_max_torque = ifelse(is.finite(daily_max_torque)==TRUE, daily_max_torque, NA),
             daily_max_power_instant = ifelse(is.finite(daily_max_power_instant)==TRUE, daily_max_power_instant, NA),
             daily_max_torque_instant = ifelse(is.finite(daily_max_torque_instant)==TRUE, daily_max_torque_instant, NA),
             daily_max_rpm = ifelse(is.finite(daily_max_rpm)==TRUE, daily_max_rpm, NA))%>%
    mutate(avg_daily_max_brake_pressure = mean(daily_max_brake_pressure, na.rm = TRUE),
           avg_daily_max_throttle = mean(daily_max_throttle, na.rm = TRUE),
           avg_daily_max_oil_temp = mean(daily_max_oil_temp, na.rm = TRUE),
           avg_daily_max_acc_longitudinal = mean(daily_max_acc_longitudinal, na.rm = TRUE),
           avg_daily_max_acc_lateral = mean(daily_max_acc_lateral, na.rm = TRUE),
           avg_daily_max_direction = mean(daily_max_direction, na.rm = TRUE),
           avg_daily_max_angular_speed = mean(daily_max_angular_speed, na.rm = TRUE),
           avg_daily_max_torque = mean(daily_max_torque, na.rm = TRUE),
           avg_daily_max_power_instant = mean(daily_max_power_instant, na.rm = TRUE),
           avg_daily_max_torque_instant = mean(daily_max_torque_instant, na.rm = TRUE),
           avg_daily_max_rpm = mean(daily_max_rpm, na.rm = TRUE)
           )
  
df <- df %>%
  mutate(mean_brake_pressure = mean(brake_pressure, na.rm = TRUE),
         mean_oil_temp = mean(oil_temperature, na.rm = TRUE),
         mean_coolant_temperature = mean(coolant_temperature, na.rm = TRUE),
         mean_acc_longitudinal = mean(abs(acc_longitudinal), na.rm = TRUE),
         mean_acc_lateral = mean(abs(acc_lateral), na.rm = TRUE),
         mean_direction = mean(direction, na.rm = TRUE),
         mean_rpm = mean(rpm, na.rm = TRUE))
 
 
 #Throttle
 df1 <- df %>%
   filter(throttle>0)%>%
   mutate(mean_throttle = mean(throttle, na.rm = TRUE))%>%
   select(mean_throttle)
 df1 <- (df1 [1,])
 df <- df %>%
   mutate(mean_throttle = df1$mean_throttle)
 
 df1 <- df %>%
   filter(angular_speed>0)%>%
   mutate(mean_angular_speed = mean(angular_speed, na.rm = TRUE))%>%
   select(mean_angular_speed)
 df1 <- (df1 [1,])
 df <- df %>%
   mutate(mean_angular_speed = df1$mean_angular_speed)
 
 df1 <- df %>%
   filter(torque>0)%>%
   mutate(mean_torque = mean(torque, na.rm = TRUE))%>%
   select(mean_torque)
 df1 <- (df1 [1,])
 df <- df %>%
   mutate(mean_torque = df1$mean_torque)
 
 df1 <- df %>%
   filter(power_instant>0)%>%
   mutate(mean_power_instant = mean(power_instant, na.rm = TRUE))%>%
   select(mean_power_instant)
 df1 <- (df1 [1,])
 df <- df %>%
   mutate(mean_power_instant = df1$mean_power_instant)
 
 df1 <- df %>%
   filter(torque_instant>0)%>%
   mutate(mean_torque_instant = mean(torque_instant, na.rm = TRUE))%>%
   select(mean_torque_instant)
 df1 <- (df1 [1,])
 df <- df %>%
   mutate(mean_torque_instant = df1$mean_torque_instant)

 
 #TOtal days 
   df1 <- df %>% 
    filter(!is.na(speed))%>% 
    filter(speed>0)%>% 
    mutate(date = date(time))%>% 
    select(date, vin)
  df1 <- base::unique(df1)
  
  df <- df %>% mutate(tot_days = nrow(df1))
  
  #% active launch control
  df1 <- df %>%
    mutate(date = date(time))%>%
    filter(activ_launch_control==1)%>%
    select(date)
  df1 <- unique(df1)
  df <- df %>% mutate(perc_active_launch_control = ifelse(nrow(df1)==0, 0, nrow(df1)/tot_days))
  
  #%roof
  df1 <- df %>%
    mutate(date = date(time))%>%
    filter(roof_open==1 | roof_intermediate)%>%
    select(date)
  df1 <- unique(df1)
  df <- df %>% mutate(perc_roof_open = ifelse(nrow(df1)==0, 0, nrow(df1)/tot_days))
  
  df <- df %>%
    select(vin, 
           avg_daily_max_brake_pressure, 
           avg_daily_max_oil_temp, 
           avg_daily_max_throttle,
           perc_active_launch_control, 
           perc_roof_open, 
           avg_daily_max_acc_longitudinal,
           avg_daily_max_acc_lateral,
           avg_daily_max_direction,
           avg_daily_max_angular_speed,
           avg_daily_max_torque,
           avg_daily_max_power_instant,
           avg_daily_max_torque_instant,
           avg_daily_max_rpm,
           mean_brake_pressure,
           mean_oil_temp,
           mean_coolant_temperature,
           mean_acc_longitudinal,
           mean_acc_lateral,
           mean_direction,
           mean_rpm,
           mean_torque_instant,
           mean_torque,
           mean_power_instant,
           mean_angular_speed,
           mean_throttle
           )
  df <-df[1,]
  }
  else {
    df <- data.frame(vin = vin2, 
                     avg_daily_max_brake_pressure = NA, 
                     avg_daily_max_oil_temp = NA, 
                     avg_daily_max_throttle = NA,
                     perc_active_launch_control = NA, 
                     perc_roof_open = NA, 
                     avg_daily_max_acc_longitudinal = NA,
                     avg_daily_max_acc_lateral = NA,
                     avg_daily_max_direction = NA,
                     avg_daily_max_angular_speed = NA,
                     avg_daily_max_torque = NA,
                     avg_daily_max_power_instant = NA,
                     avg_daily_max_torque_instant = NA,
                     avg_daily_max_rpm = NA,
                     mean_brake_pressure = NA,
                     mean_oil_temp = NA,
                     mean_coolant_temperature = NA,
                     mean_acc_longitudinal = NA,
                     mean_acc_lateral = NA,
                     mean_direction = NA,
                     mean_rpm = NA,
                     mean_torque_instant = NA,
                     mean_torque = NA,
                     mean_power_instant = NA,
                     mean_angular_speed = NA,
                     mean_throttle = NA)
  }
  
}

vr <- lapply(myfiles,variables_function)
do.call(rbind, vr)
vr <- do.call(rbind.data.frame, vr)
#write.csv(vr, "vr.csv")
#vr <- read_csv("C:/Users/l.dorsa/Desktop/Lambo/vr.csv")
vr <- vr %>%
  left_join(cluster)

vr <- vr %>% 
  filter(!is.na(cluster))

#Pressione su freni
vr %>%
 ggplot(aes(x=as.factor(cluster), y=avg_daily_max_brake_pressure))+
  geom_boxplot()
vr %>%
  ggplot(aes(x=as.factor(cluster), y=mean_brake_pressure))+
  geom_boxplot()

#Pressione acceleratore
vr %>%
  ggplot(aes(x=as.factor(cluster), y=avg_daily_max_throttle))+
  geom_boxplot(aes(color = as.factor(cluster)), lwd = 1)+
  theme_light()+ 
  xlab("Cluster")+
  ylab("Maximum throttle")+
  theme(axis.text.x = element_text(face="bold", 
                                   size=12),
        axis.text.y = element_text(face="bold",
                                   size=12),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"))+
  theme(plot.background = element_rect(fill = "#EEEEEE"), legend.background =     element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"),
        legend.position = "none")
  
vr %>%
  ggplot(aes(x=as.factor(cluster), y=mean_throttle))+
  geom_boxplot()

#Temperatura olio
vr %>%
  ggplot(aes(x=as.factor(cluster), y=avg_daily_max_oil_temp))+
  geom_boxplot()
vr %>%
  ggplot(aes(x=as.factor(cluster), y=mean_oil_temp))+
  geom_boxplot(aes(color = as.factor(cluster)), lwd = 1)+
  theme_light()+ 
  xlab("Cluster")+
  ylab("Average oil temperature")+
  theme(axis.text.x = element_text(face="bold", 
                                   size=12),
        axis.text.y = element_text(face="bold",
                                   size=12),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"))+
  theme(plot.background = element_rect(fill = "#EEEEEE"), legend.position = "none", legend.background =     element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))



#Active launch control
vr %>%
  ggplot(aes(x=as.factor(cluster), y=perc_active_launch_control))+
  geom_boxplot(aes(color = as.factor(cluster)), lwd = 1)+
  theme_light()+ 
  xlab("Cluster")+
  ylab("Percentage of days")+
  theme(axis.text.x = element_text(face="bold", 
                                   size=12),
        axis.text.y = element_text(face="bold",
                                   size=12),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"))+
  theme(plot.background = element_rect(fill = "#EEEEEE"), legend.position = "none",legend.background =     element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))


#% roof
vr %>%
  ggplot(aes(x=as.factor(cluster), y=perc_roof_open))+
  geom_boxplot()

#Liquido di raffreddamento
vr %>%
  ggplot(aes(x=as.factor(cluster), y=mean_coolant_temperature))+
  geom_boxplot()

#Accelerazione longitudinale
vr %>%
  ggplot(aes(x=as.factor(cluster), y=mean_acc_longitudinal))+
  geom_boxplot()
vr %>%
  ggplot(aes(x=as.factor(cluster), y=avg_daily_max_acc_longitudinal))+
  geom_boxplot()

#Accelerazione laterale
vr %>%
  ggplot(aes(x=as.factor(cluster), y=mean_acc_lateral))+
  geom_boxplot()
vr %>%
  ggplot(aes(x=as.factor(cluster), y=avg_daily_max_acc_lateral))+
  geom_boxplot()

#Angolo di sterzata
vr %>%
  ggplot(aes(x=as.factor(cluster), y=mean_direction))+
  geom_boxplot()
vr %>%
  ggplot(aes(x=as.factor(cluster), y=avg_daily_max_direction))+
  geom_boxplot()

#Rotazioni per minuto
vr %>%
  ggplot(aes(x=as.factor(cluster), y=mean_rpm))+
  geom_boxplot()
vr %>%
  ggplot(aes(x=as.factor(cluster), y=avg_daily_max_rpm))+
  geom_boxplot(aes(color = as.factor(cluster)), lwd = 1)+
  theme_light()+ 
  xlab("Cluster")+
  ylab("Maximum rpm")+
  theme(axis.text.x = element_text(face="bold", 
                                   size=12),
        axis.text.y = element_text(face="bold",
                                   size=12),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"))+
  theme(plot.background = element_rect(fill = "#EEEEEE"), legend.position = "none", legend.background =     element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))




#Coppia istantanea
vr %>%
  ggplot(aes(x=as.factor(cluster), y=mean_torque_instant))+
  geom_boxplot()
vr %>%
  ggplot(aes(x=as.factor(cluster), y=avg_daily_max_torque_instant))+
  geom_boxplot(aes(color = as.factor(cluster)), lwd = 1)+
  theme_light()+ 
  xlab("Cluster")+
  ylab("Maximum torque")+
  theme(axis.text.x = element_text(face="bold", 
                                   size=12),
        axis.text.y = element_text(face="bold",
                                   size=12),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"))+
  theme(legend.position = "none", plot.background = element_rect(fill = "#EEEEEE"), legend.background =     element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))


#Coppia
vr %>%
  ggplot(aes(x=as.factor(cluster), y=mean_torque))+
  geom_boxplot()
vr %>%
  ggplot(aes(x=as.factor(cluster), y=avg_daily_max_torque))+
  geom_boxplot()

#Power instant
vr %>%
  ggplot(aes(x=as.factor(cluster), y=mean_power_instant))+
  geom_boxplot()
vr %>%
  ggplot(aes(x=as.factor(cluster), y=avg_daily_max_power_instant))+
  geom_boxplot()+
  theme_light()+ 
  xlab("Cluster")+
  ylab("Maximum instant power")+
  theme(axis.text.x = element_text(face="bold", 
                                   size=12),
        axis.text.y = element_text(face="bold",
                                   size=12),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"))+
  theme(plot.background = element_rect(fill = "#EEEEEE"), legend.background =     element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))



#Angular speed
vr %>%
  ggplot(aes(x=as.factor(cluster), y=mean_angular_speed))+
  geom_boxplot()
vr %>%
  ggplot(aes(x=as.factor(cluster), y=avg_daily_max_angular_speed))+
  geom_boxplot()




drive_mode_function <- function(i) {

df <- fread(i, select = c("speed", "driving_mode","vin", "time"))

vin2 <- as.character(as.vector(df[1,3]))

df <- df %>%
  filter(!(is.na(speed) & is.na(driving_mode)))
df <- df[order(df$time),]
df <- df %>%
  mutate(drive_mode_pred = lag(driving_mode)) %>%
  mutate(driving_mode = ifelse(is.na(driving_mode), drive_mode_pred, driving_mode))
  

#rpm, speed, driving mode
#("1 - Charisma Strada, 2 - Charisma Sport, 3 - Charisma Corsa")

df <- df %>% 
  group_by(driving_mode)%>%
  filter(!is.na(driving_mode))%>%
  filter(speed>0)%>%
  mutate(mean_speed_dm = mean(speed, na.rm = TRUE))%>%
  select(!c(speed, time, drive_mode_pred))

df <- unique(df)
df1 <- df %>% filter(driving_mode==1)
df2 <- df %>% filter(driving_mode==2)
df3 <- df %>% filter(driving_mode==3)
mean_speed_1 <- as.vector(df1$mean_speed_dm)
mean_speed_2 <- as.vector(df2$mean_speed_dm)
mean_speed_3 <- as.vector(df3$mean_speed_dm)

rm(df1, df2, df3)

df <- data.frame(vin = vin2, 
                 mean_speed_1 = ifelse(length(mean_speed_1) ==0, NA, mean_speed_1),
                 mean_speed_2 = ifelse(length(mean_speed_2) ==0, NA, mean_speed_2),
                 mean_speed_3 = ifelse(length(mean_speed_3) ==0, NA, mean_speed_3))
}

dm <- lapply(myfiles,drive_mode_function)
do.call(rbind, dm)
dm <- do.call(rbind.data.frame, dm)
#write_csv(dm, "dm.csv")

dm <- dm %>%
  left_join(cluster)
dm <- dm %>%
  filter(!is.na(cluster))

dm %>%
  ggplot(aes(x=as.factor(cluster), y=mean_speed_1))+
  geom_boxplot()+
  ggtitle("Charisma strada")
dm %>%
  ggplot(aes(x=as.factor(cluster), y=mean_speed_2))+
  geom_boxplot()+
  ggtitle("Charisma sport")
dm %>%
  ggplot(aes(x=as.factor(cluster), y=mean_speed_3))+
  geom_boxplot()+
  ggtitle("Charisma Corsa")

perc_mode_function <- function(i) {
  
  df <- fread(i, select = c("speed", "driving_mode","vin", "time"))
  
  vin2 <- as.character(as.vector(df[1,3]))
  df$time <- ymd_hms(df$time)
  #Total days 
  df1 <- df %>% 
    filter(!is.na(speed))%>% 
    filter(speed>0)%>% 
    mutate(date = date(time))%>% 
    select(date, vin)
  df1 <- base::unique(df1)
  df <- df %>% mutate(tot_days = nrow(df1))
  
  df <- df %>%
    filter(!(is.na(speed) & is.na(driving_mode)))
  df <- df[order(df$time),]
  df <- df %>%
    mutate(drive_mode_pred = lag(driving_mode)) %>%
    mutate(driving_mode = ifelse(is.na(driving_mode), drive_mode_pred, driving_mode))
  
  
  #("1 - Charisma Strada, 2 - Charisma Sport, 3 - Charisma Corsa")
  
  df1 <- df %>%
    filter(speed>0)%>%
    mutate(date = date(time))%>%
    filter(driving_mode==1)%>%
    select(date)
  df1 <- unique(df1)
  df <- df %>% mutate(perc_dm_1 = ifelse(nrow(df1)==0, 0, nrow(df1)/tot_days))
  
  df1 <- df %>%
    filter(speed>0)%>%
    mutate(date = date(time))%>%
    filter(driving_mode==2)%>%
    select(date)
  df1 <- unique(df1)
  df <- df %>% mutate(perc_dm_2 = ifelse(nrow(df1)==0, 0, nrow(df1)/tot_days))
  
  df1 <- df %>%
    filter(speed>0)%>%
    mutate(date = date(time))%>%
    filter(driving_mode==3)%>%
    select(date)
  df1 <- unique(df1)
  df <- df %>% mutate(perc_dm_3 = ifelse(nrow(df1)==0, 0, nrow(df1)/tot_days))
  
  df <- df %>% select(vin, perc_dm_1, perc_dm_2, perc_dm_3)
  df <- df[1,]
}

dm2 <- lapply(myfiles,perc_mode_function)
do.call(rbind, dm2)
dm2 <- do.call(rbind.data.frame, dm2)
#write_csv(dm2, "dm2.csv")
#dm2 <- read_csv("dm2.csv")
dm2 <- dm2 %>%
  left_join(cluster)%>%
  filter(!is.na(cluster))

dm2 %>%
  ggplot(aes(x=as.factor(cluster), y=perc_dm_1))+
  geom_boxplot(aes(color = as.factor(cluster)), lwd = 1)+
  ggtitle("Charisma strada")+
  theme_light()+ 
  xlab("Cluster")+
  ylab("Percentage of days")+
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

dm2 %>%
  ggplot(aes(x=as.factor(cluster), y=perc_dm_2))+
  geom_boxplot(aes(color = as.factor(cluster)), lwd = 1)+
  ggtitle("Charisma sport")+
  theme_light()+ 
  xlab("Cluster")+
  ylab("Percentage of days")+
  theme(axis.text.x = element_text(face="bold", 
                                   size=12),
        axis.text.y = element_text(face="bold",
                                   size=12),
        axis.title=element_text(face="bold",size=12),
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(face="bold",size=22))+
  theme(plot.background = element_rect(fill = "#EEEEEE"), legend.position="none",legend.background =     element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))

dm2 %>%
  ggplot(aes(x=as.factor(cluster), y=perc_dm_3))+
  geom_boxplot(aes(color = as.factor(cluster)), lwd = 1)+
  ggtitle("Charisma Corsa")+
  theme_light()+ 
  xlab("Cluster")+
  ylab("Percentage of days")+
  theme(axis.text.x = element_text(face="bold", 
                                   size=12),
        axis.text.y = element_text(face="bold",
                                   size=12),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(face="bold",size=22))+
  theme(legend.position="none",plot.background = element_rect(fill = "#EEEEEE"), legend.background =     element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))
