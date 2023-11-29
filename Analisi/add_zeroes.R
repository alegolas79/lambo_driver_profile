df <- read_csv("ZHWCF5ZF7MLA17456.csv")
df <- df %>% select(time, latitude, longitude, altitude, speed)


#-----------------------------------------------------------------------------
#Seleziono solo orario con timezone CET e velocità, tengo solo valori con speed diverso da NA
add_zeroes <- function(df) { df <- df %>%
  mutate(speed = ifelse(is.na(speed), 0, speed))
df1 <- df %>% dplyr::select(time, speed) %>%
  filter(!is.na(speed))


#Ordino secondo data e orario e creo una variabile che indichi la velocità nella riga precedente
df1 <- df1[order(df1$time),]
df1$speed_pre <- lag(df1$speed, 1)

#Sulla base della variabile creata, confrontata con velocità, indico momenti di inizio e fine serie
df1 <- df1 %>%  mutate(series = ifelse(speed>0 & speed_pre==0, "start",
                                       ifelse(speed==0 & speed_pre >0, "stop",NA)))

#Riempio la variabile serie e trovo i punti che fanno parte di serie, il cui punto successivo fa anch'esso parte di una serie
#Se il punto successivo è più distante di 60 secondi lo segno
df1 <- df1 %>%
    mutate(series2 = series)%>%
    fill(series2, .direction = "down")
  df1$time_succ <- lead(df1$time, 1)
  df1$series3 <- lead(df1$series2, 1)
  df1 <- df1 %>%
    mutate(flag = ifelse(series2=="start" & 
                           series3=="start" & 
                           (time_succ - time)>=60,
                         1,
                         0
                       ))
  
#Raccolgo i punti individuati prima
  df1 <- df1 %>% 
    filter(flag==1)
  #Creo delle righe a 1s di distanza dai punti individuati sopra con velocità zero
  df1 <- df1 %>%
    select(speed, time)%>%
    mutate(speed=0, time = time+1, latitude = NA, longitude = NA, altitude = NA)
  df1 <- df1[,c(2,3,4,5,1)]
  
  #Unisco al dataset originale
  df <- df %>%
    rbind(df1)
  df <- df[order(df$time),]
}
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
df <- df %>% mutate(id = 1:nrow(df))
#-----------------------------------------------------------------------------  
  
  setup_start_stop <- function(df, x, y) {
    
    #Seleziono solo orario con timezone CET e velocità, tengo solo valori con speed diverso da NA
    df <- df %>%
      mutate(speed = ifelse(is.na(speed), 0, speed))
    df1 <- df %>% dplyr::select(time, speed, id) %>%
      filter(!is.na(speed))
    
    
    #Ordino secondo data e orario e creo una variabile che indichi la velocità nella riga precedente
    df1 <- df1[order(df1$time),]
    df1$speed_pre <- lag(df1$speed, 1)
    
    #Sulla base della variabile creata, confrontata con velocità, indico momenti di inizio e fine serie
    df1 <- df1 %>%  mutate(series = ifelse(speed>0 & speed_pre==0, "start",
                                           ifelse(speed==0 & speed_pre >0, "stop",
                                                  NA)))
  #Seleziono solo momenti di inizio e fine serie
  df1 <- df1 %>% filter(!is.na(series))
  
  #Creo variabile che dia l'orario della riga precedente
  df1$time_stop_pred <- lag(df1$time, 1)
  
  
  #Trovo la lunghezza dello stop antecedente a start, solo per le variabili start
  df1$length_stop_pred <- ifelse(df1$series=="start", df1$time - df1$time_stop_pred, NA)
  
  #Trovo lunghezza dello stop successivo a stop
  df1$time_start_next <- lead(df1$time)
  df1$length_stop_next <- ifelse(df1$series=="stop", (df1$time_start_next - df1$time), NA)
  
  df1 <- df1[order(df1$time),]
  
  
  #Seleziono start e stop sulla base del parametro di tempo, in modo che le serie attaccate tra di loro risultino come la stessa serie
  df1 <- df1 %>% filter(ifelse(series=="start", length_stop_pred >= x, length_stop_next>=x))
  
  #Genero ID di inizio serie per gli start, metto "stop" per gli stop  
  df1$id_series <- ifelse(df1$series == "start", 1:nrow(df1), "stop") 
  
  #Unisco le info sugli start al file generale
  df1 <- df1 %>% select(time, speed, series,id_series, id)
  df <- df %>% left_join(df1)
  df <- df[order(df$time),]  
  #Riempio le id
  df <-df %>%
    fill(id_series, .direction = "down")
  
  #Trasformo le id stop in NA, poi calcolo la velocità massima raggiunta nel corso della serie ignorando gli NA
  df <- df %>%
    mutate(id_series = ifelse(id_series == "stop", NA, id_series))%>%
    group_by(id_series)%>%
    mutate(max_series = max(speed, na.rm = TRUE))%>%
    mutate(max_series = ifelse(is.na(id_series), 0, max_series))
  
  #Sulla base della velocità massima serie confrontata con parametro velocità creo variabile che indichi se il periodo ci è interessante o no 
  df <- df %>%
    ungroup()%>%
    mutate(col = ifelse(
      max_series <= y, "Red", "Green"))
}


#---------------------------------------------------------------------------------------------------------------
#Parametri
#x = tempo che deve intercorrere tra due velocità perchè siano considerate serie separate espresso in secondi
x <- 300
#y = velocità sotto la quale la serie non è considerata di interesse
y <- (df_speed$mean_speed + df_speed$sd_speed)
#---------------------------------------------------------------------------------------------------------------
#Chiamare Series_functions
df <- setup_start_stop(df, x, y)