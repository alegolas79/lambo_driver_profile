library(data.table)
library(tidyverse)
library(lubridate)
library(hms)
library(lutz)

#Mettere tutti i dataset lambo in una directory unica e usarla come working directory
#setwd("C:/Users/l.dorsa/Desktop/Lambo/Lambo_complete")
myfiles = list.files(pattern="*.csv", full.names=TRUE)
myfiles

calculate_drive_length <- function(i) { 
  #-------------------------------------------------------------------------------  
  
  #Lettura file
  df <- fread(i, select = c("vin","time","latitude","longitude","altitude","speed"))
  #Assicurarsi che time sia un oggetto datetime (se no dà errore)
  df$time <- ymd_hms(df$time)
  
  #-------------------------------------------------------------------------------  
  
  #Parametri x selezione serie
  df_speed <- df %>% 
    filter(speed>0) %>%
    mutate(mean_speed = mean(speed), max_speed = max(speed), sd_speed = sd(speed))
  df_speed <- df_speed[1,]
  #Parametro temporale
  x <- 300
  #Parametro di velocità
  y <- (df_speed$mean_speed + df_speed$sd_speed)
  
  #-------------------------------------------------------------------------------  
  
  #aggiungi zeri nei punti di dataset mancanti
  df <- df %>%
    mutate(speed = ifelse(is.na(speed), 0, speed))
  df1 <- df %>% dplyr::select(time, speed) %>%
    filter(!is.na(speed))
  df1 <- df1[order(df1$time),]
  df1$speed_pre <- lag(df1$speed, 1)
  #Trova momenti di start e di stop
  df1 <- df1 %>%  mutate(series = ifelse(speed>0 & speed_pre==0, "start",
                                         ifelse(speed==0 & speed_pre >0, "stop",NA)))
  df1 <- df1 %>%
    mutate(series2 = series)%>%
    fill(series2, .direction = "down")
  df1$time_succ <- lead(df1$time, 1)
  #Trova la differenza di tempo tra un'osservazione e quella successiva
  #Se è superiore a 60 secondi e l'osservazione precedente fa parte di una serie, aggiungi bandierina
  df1 <- df1 %>%
    mutate(time_diff = time_succ - time)%>%
    mutate(flag = ifelse(series2=="start" & time_diff>=60,1,0))
  #Filtra osservazioni con bandierina e crea delle osservazioni a un secondo di distanza a velocità zero, da aggiungere al dataset iniziale
  #Questo serve a creare artificialmente dei punti di "stop" dove mancano parti di dataset
  df1 <- df1 %>% filter(flag==1)
  vin1 <- as.character(as.vector(df[1,1]))
  df1 <- df1 %>% select(speed, time)%>% mutate(vin = vin1, time = time+1, latitude = NA, longitude = NA, altitude = NA, speed=0)
  df1 <- df1[,c(3,2,4,5,6,1)]
  df <- df %>% rbind(df1)
  df <- df[order(df$time),]
  rm(df1)
  
  #-------------------------------------------------------------------------------
  
  #seleziona e colora serie 
  #Stessa funzione di prima, l'unica differenza è che tengo l'informazione di start e stop nel dataset
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
  
  #-------------------------------------------------------------------------------
  
  #Funzione che misura la durata delle serie
  df1 <- df %>% select(id_series, time, col, series)
  #Seleziono solo momenti di start e stop
  df1 <- df1 %>% filter(!is.na(series))
  df1 <- df1[order(df1$time),]
  df1 <- df1 %>%
    mutate(time_start = lag(time))
  #Trovo la durata della serie
  df1 <- df1%>%
    mutate(drive_length = ifelse(series=="stop", (time - time_start), NA))%>%
    mutate(day_start = day(time_start), month_start = month(time_start))%>% 
    mutate(weekday_start = weekdays(time_start))
  df1 <- df1%>%
    mutate(col = ifelse(series=="stop", NA, col))
  #Riempio i colori per serie, per qualche ragione altrimenti il momento di stop mi viene segnato come rosso
  df1 <- df1 %>% 
    fill(col, .direction = "down")%>% 
    fill(id_series, .direction = "down")
  #
  #
  #Trovo la durata media serie rosse e verdi e solo verdi
  mean_drive_length_r_g <- as.vector(mean(df1$drive_length, na.rm=TRUE))
  max_drive_lngth <- as.vector(max(df1$drive_length, na.rm=TRUE))
  df3 <- df1 %>%
    filter(col=="Green")
  mean_drive_length_g <- as.vector(mean(df3$drive_length, na.rm=TRUE))
  
  rm(df3)
  
  #-------------------------------------------------------------------------------
  
  #Funzione che calcola la durata media giornaliera delle serie
  #E gestisco il problema di guide che vanno oltre la mezzanotte
  df2 <- df1 %>%
    filter(series == "stop")%>%
    mutate(secs_from_midnight = ifelse(day(time)!= day(time_start), as_hms(time), 0)) %>%
    mutate(drive_length_start = drive_length - secs_from_midnight) %>%
    mutate(date = date(time_start))
  #Seleziono solo i casi in cui l'utente ha iniziato a guidare prima della mezzanotte e ha continuato dopo la mezzanotte
  df3 <- df2 %>% 
    ungroup()%>%
    filter(secs_from_midnight>0)
  #Creo un dataset dove raccolgo il tempo in cui si è guidato oltre la mezzanotte e assegno le osservazioni al giorno successivo
  df3 <- df3 %>%
    mutate(date = date + 1) %>%
    mutate(drive_length_start = secs_from_midnight)%>%
    mutate(secs_from_midnight = 0)%>%
    mutate(weekday_start = weekdays(date))%>%
    mutate(day_start = day(date), month_start = month(date))
  #Riunisco i dataset
  df2 <- df2 %>%
    rbind(df3)
  #Raggruppo per data e calcolo durata media di guida
  #Il valore che uso per calcolare la durata di guida giornaliera è la durata della serie meno i secondi passati oltre la mezzanotte, se no li conto due volte
  df2 <- df2 %>%
    group_by(month_start, day_start)%>%
    mutate(total_drive_start = sum(drive_length_start))
  df2 <- df2 %>% select(day_start, month_start, weekday_start, total_drive_start, date)
  df2 <- base::unique(df2)
  rm(df3)
  #
  #
  mean_day_length_r_g <- as.vector(mean(df2$total_drive_start, na.rm=TRUE))
  
  #-------------------------------------------------------------------------------
  
  #Durata media giornaliera delle serie verdi
  #Stessa funzione di sopra ma solo per le serie verdi
  df2 <- df1 %>%
    filter(series == "stop")%>%
    filter(col == "Green")%>%
    mutate(secs_from_midnight = ifelse(day(time)!= day(time_start), as_hms(time), 0)) %>%
    mutate(drive_length_start = drive_length - secs_from_midnight) %>%
    mutate(date = date(time_start))
  df3 <- df2 %>% 
    ungroup()%>%
    filter(secs_from_midnight>0)
  df3 <- df3 %>%
    mutate(date = date + 1) %>%
    mutate(drive_length_start = secs_from_midnight)%>%
    mutate(secs_from_midnight = 0)%>%
    mutate(weekday_start = weekdays(date))%>%
    mutate(day_start = day(date), month_start = month(date))
  df2 <- df2 %>%
    rbind(df3)
  df2 <- df2 %>%
    group_by(month_start, day_start)%>%
    mutate(total_drive_start = sum(drive_length_start))
  df2 <- df2 %>% select(day_start, month_start, weekday_start, total_drive_start, date)
  df2 <- base::unique(df2)
  rm(df3)
  #
  #
  mean_day_length_g <- as.vector(mean(df2$total_drive_start, na.rm=TRUE))
  rm(df, df1, df2, df_speed, x, y)
  
  data_length <- data.frame(vin = vin1, 
                            mean_dat_length_g = mean_dat_length_g, 
                            mean_dat_length_r_g = mean_dat_length_r_g,
                            mean_drive_length_r_g = mean_drive_length_r_g,
                            mean_drive_length_g = mean_drive_length_g,
                            max_drive_lngth = max_drive_lngth)
  
}

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

length <- lapply(myfiles,calculate_drive_length)
do.call(rbind, length)
length <- do.call(rbind.data.frame, length)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#Funzione calcolo velocità
find_speed <- function(i) {
  
  df <- fread(i, select = c("time", "speed", "vin"))
  df$time <- ymd_hms(df$time)
  vin2 <- as.character(as.vector(df[1,3]))
  df <- df %>%
    filter(!is.na(speed))%>%
    filter(speed > 0)
  df <- df%>%
    mutate(month = month(time), day = day(time))%>%
    group_by(month, day)%>%
    mutate(daily_max = max(speed), daily_mean = mean(speed))%>%
    ungroup()%>%
    mutate(avg_daily_max = mean(daily_max), avg_daily_mean = mean(daily_mean), 
           overall_max = max(speed), overall_mean = mean(speed))%>%
    select(vin, avg_daily_max, avg_daily_mean, overall_max, overall_mean)
  
  df <- df[1,]
  df <- df %>%
    mutate(vin = vin2)
  
}

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

speed <- lapply(myfiles,find_speed)
do.call(rbind, speed)
speed <- do.call(rbind.data.frame, speed)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#Funzione calcolo giorni
#Trovo il numero di giorni in cui l'auto è stata usata a velocità>0
#E ne trovo la % rispetto ai giorni potenziali di guida (ovvero dal primo giorno del dataset all'ultima osservazione a noi disponibile)
days_function <- function(i) {
  df <- fread(i, select = c("time", "speed", "vin"))
  vin2 <- as.character(as.vector(df[1,3]))
  
  if(all(is.na(df$time))==FALSE) {
    df$time <- ymd_hms(df$time)
    df <- df %>% 
      mutate(date = as.Date(time, format = "%Y-%m-%d"))
    min_date <- min(df$date, na.rm = TRUE)
    df <- df %>% 
      filter(!is.na(speed))%>% 
      filter(speed>0)%>% 
      mutate(date = date(time))%>% 
      select(date, vin)%>%
      mutate(weekdays = weekdays(date))
    df <- base::unique(df)
    df <- df[order(df$date),]
    #Percentuale giorni guidati
    max_date <- ymd("2021-11-29")
    possible_dates <- seq(min_date, max_date, by="days")
    df <- df %>%
      mutate(tot_days = nrow(df))%>%
      mutate(perc_days = ifelse(nrow(df)>0, nrow(df)/length(possible_dates), 0))
    df <- df %>%
      select(vin, perc_days, tot_days)
    df <- df[1,]
    df <- df %>%
      mutate(vin = vin2)}
  else
  {df <- data.frame(vin = vin2, perc_days = 0, tot_days = 0)}
  
}

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

days <- lapply(myfiles,days_function)
do.call(rbind, days)
days <- do.call(rbind.data.frame, days)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
km_function <- function(i) {
  df <- fread(i, select = c("time", "km_tachometer", "vin", "speed"))
  vin2 <- as.character(as.vector(df[1,3]))
  df <- df %>% 
    filter(km_tachometer < 1000000)%>%
    filter(km_tachometer > 0)%>%
    filter(speed>0)
  min <- min(df$km_tachometer)
  max <- max(df$km_tachometer)
  
  if(nrow(df) < 2) {
    df <- data.frame(vin = vin2, km_percorsi = 0)
  }
  else{
    df <- data.frame(vin = vin2, km_percorsi = (max - min))
  }
}

km <- lapply(myfiles, km_function)
do.call(rbind, km)
km <- do.call(rbind.data.frame, km)




#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
country_function <- function(i) {
  #Trovo (approssimativamente) la zona dell'auto
  df <- fread(i, select = c("latitude", "longitude", "vin"))
  vin2 <- as.character(as.vector(df[1,3]))
  
  if(all(is.na(df$latitude))==FALSE) {
    df <- df %>% filter(!is.na(latitude))
    df$country <- tz_lookup_coords(df$latitude, df$longitude, method = "fast")
    df <- df %>% group_by(country)%>%count()
    df <- df %>% 
      slice_max(n, n = 1) %>% 
      mutate(vin=vin2)%>%
      select(!n)
    df <- df[1,]
  }
  
  else {
    df <- data.frame(country = NA, vin = vin2)
  }
}

country <- lapply(myfiles, country_function)
do.call(rbind, country)
country <- do.call(rbind.data.frame, country)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
data <- days %>%
  left_join(speed)%>%
  left_join(length)%>%
  left_join(km)%>%
  left_join(days)%>%
  left_join(country)

#write_csv(data, "confronto_km.csv")


