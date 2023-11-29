#setwd("C:/Users/l.dorsa/Desktop/Lambo/Lambo_complete")
myfiles = list.files(pattern="*.csv", full.names=TRUE)
myfiles

variables_function <- function(i) {
  
  #Lettura file
  df <- fread(i, select = c("vin","time","latitude","longitude","activ_launch_control","speed"))
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
    select(time, activ_launch_control, series)
  df <- df[order(df$time),]  
  df <- df %>%
    fill(activ_launch_control, .direction = "down")%>%
    filter(!is.na(activ_launch_control))%>%
    filter((activ_launch_control==1))%>%
    filter(!is.na(series))
  df <- df %>%
    mutate(km_start = ifelse(series =="stop", lag(km_tachometer), NA))%>%
    filter(series=="stop")%>%
    mutate(km_series = (km_tachometer - km_start))%>%
    filter(km_series > 0)
  df <- df %>%
    mutate(avg_km_series = mean(km_series, na.rm = TRUE))
  df <- data.frame(vin = vin1, avg_km_series = df[1,6])

  
  
  
  
  
  
  
  
  
  
  
  df <- fread(i, select = c("activ_launch_control",
                            "vin", "time", "speed"))
  
  vin2 <- as.character(as.vector(df[1,2]))
  df$time <- ymd_hms(df$time)
  df1 <- df %>%
    filter(speed>0)
  
  if(nrow(df1)>0) {

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
    
    
    df <- df %>%
      select(vin,
             perc_active_launch_control)
    df <-df[1,]
  }
  else {
    df <- data.frame(vin = vin2, 
                     perc_active_launch_control = NA)
  }
  
}