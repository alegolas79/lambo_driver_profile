#funzioni possibili applicazioni
library(lubridate)
library(tidyverse)
library(hms)
library(data.table)


#setwd("C:/Users/l.dorsa/Desktop/Lambo/Lambo_complete")
myfiles = list.files(pattern="*.csv", full.names=TRUE)

#Trova osservazioni entro 10 km dagli autodromi
funzione_autodromi <- function(i){
  df <- fread(i, select = c("vin","latitude", "longitude", "speed", "time"))
  vin2 <- as.character(as.vector(df[1,1]))
  autodromi_ita <- data.frame(
    autodromo = c(
      "Spa-Francorchamps",
      "Jules Tacheny Mettet",
      "Zolder",
      "la Sarthe",
      "Charade",
      "Croix-en-Ternois",
      "Faleyras",
      "Lédenon",
      "Nevers Magny-Cours",
      "Mas du Clos",
      "Val de Vienne",
      "Paul Armagnac",
      "Pau-Arnos",
      "Paul Ricard",
      "Dijon-Prenois",
      "EuroSpeedway Lausitz",
      "Hockenheimring",
      "Oschersleben",
      "Nürburgring",
      "Sachsenring",
      "Mondello park",
      "Adria",
      "Umbria",
      "Franciacorta",
      "Mores",
      "Pergusa",
      "Imola",
      "Monza",
      "Vallelunga",
      "Paletti",
      "Misano",
      "Mugello",
      "Goodyear",
      "Findel",
      "Monaco",
      "Zandvoort",
      "Venray",
      "Assen",
      "Estoril",
      "Algarve",
      "Vasco Sameiro",
      "Barcelona-Catalunya",
      "Tormo",
      "Calafat",
      "Albacete",
      "Guadix",
      "Jerez",
      "Jarama",
      "Navarra",
      "MotorLand Aragón",
      "Monteblanco",
      "Buochs",
      "Hittnau",
      "Lignières",
      "Anglesey",
      "Bedford",
      "Brands Hatch",
      "Cadwell Park",
      "Castle Combe",
      "Croft",
      "Donington Park",
      "Goodwood",
      "Lydden",
      "Mallory Park",
      "Silverstone Circuit"
    ),
    latitude = c(
      50.437222,
      50.30403,
      50.988889,
      47.937694,
      45.747222,
      50.379028,
      44.751944,
      43.923611,
      46.863242,
      45.936,
      46.195833,
      43.768056,
      43.444167,
      43.250556,
      47.3625,
      51.533333,
      49.327778,
      52.027222,
      50.335556,
      50.791667,
      53.2575,
      45.044167,
      43.131111,
      45.511667,
      40.508889,
      37.514444,
      44.341111,
      45.620556,
      42.160833,
      44.681111,
      43.961389,
      43.9975,
      49.795,
      49.629722,
      43.734722,
      52.388819,
      51.520853,
      52.961667,
      38.750833,
      37.232,
      41.58637,
      41.57,
      39.485833,
      40.933889,
      39.007817,
      37.401667,
      36.708333,
      40.617111,
      42.561058,
      41.078333,
      37.359169,
      46.966667,
      47.366667,
      47.083333,
      53.190042,
      52.233611,
      51.356667,
      53.308889,
      51.489722,
      54.455833,
      52.829806,
      50.859444,
      51.177386,
      52.598611,
      52.078611
    ),
    longitude = c(
      5.971389,
      4.6575,
      5.255556,
      0.225611,
      3.038889,
      2.296778,
      0.241111,
      4.505833,
      3.164228,
      2.301944,
      0.631944,
      -0.038056,
      -0.530556,
      5.791667,
      4.899167,
      13.919444,
      8.565833,
      11.278611,
      6.9475,
      12.688889,
      -6.745,
      12.1475,
      12.239444,
      10.005278,
      8.8325,
      14.306389,
      11.713333,
      9.289444,
      12.369167,
      10.0225,
      12.683333,
      11.371944,
      6.103,
      6.205556,
      7.420556,
      4.540922,
      5.877931,
      6.523333,
      -9.394167,
      -8.632,
      -8.44497,
      2.261111,
      -0.628056,
      0.841667,
      -1.7959,
      -3.074722,
      -6.034167,
      -3.585583,
      -2.163614,
      -0.2075,
      -6.565647,
      8.416667,
      8.816667,
      7.066667,
      -4.497661,
      -0.472222,
      0.2625,
      -0.063056,
      -2.2125,
      -1.562778,
      -1.379556,
      -0.759167,
      1.199636,
      -1.337778,
      -1.016944
    ))
  autodromi_ita$latitude <- trunc(autodromi_ita$latitude*10)/10
  autodromi_ita$longitude <- trunc(autodromi_ita$longitude*10)/10
  df$latitude <- trunc(df$latitude*10)/10
  df$longitude <- trunc(df$longitude*10)/10
  df <- df%>%
    left_join(autodromi_ita)
  df <- df%>%
    filter(!is.na(autodromo))
  df <- data.frame(vin = vin2, n_autodromo = nrow(df))
}

auto <- lapply(myfiles,funzione_autodromi)
do.call(rbind, auto)
auto <- do.call(rbind.data.frame, auto)
#write_csv(auto, "C:/Users/l.dorsa/Desktop/Lambo/auto.csv")


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#Funzione di calcolo chilometri percorsi per serie, parametro impostato a due ore

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
  #Trova km
  
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

#write.csv(km_series2, "C:/Users/l.dorsa/Desktop/Lambo/km_series2.csv")

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#Funzione di calcolo periodi di "ricarica" giornalieri
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
  
  #-----------------------------------------------------------------------------
  #Calcolo durata stop
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


stop_2h <- lapply(myfiles,calculate_stop_length)
do.call(rbind, stop_2h)
stop_2h <- do.call(rbind.data.frame, stop_2h)
#write_csv(stop_2h, "stop_2h.csv")

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------