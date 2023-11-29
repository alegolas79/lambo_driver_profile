x <- 300
#y = velocità sotto la quale la serie non è considerata di interesse
y <- 58.31536
#setup_start_stop riempie le velocità mancanti con zero, crea un secondo dataset dove vengono individuati momenti di start e di stop
setup_start_stop <- function(df, x, y) {
  
  #Seleziono solo orario con timezone CET e velocità, tengo solo valori con speed diverso da NA
  df <- df %>%
    mutate(speed = ifelse(is.na(speed), 0, speed))
  df1 <- df %>% dplyr::select(time_ita, speed, id) %>%
    filter(!is.na(speed))
  
  
  #Ordino secondo data e orario e creo una variabile che indichi la velocità nella riga precedente
  df1 <- df1[order(df1$time_ita),]
  df1$speed_pre <- lag(df1$speed, 1)
  
  #Sulla base della variabile creata, confrontata con velocità, indico momenti di inizio e fine serie
  df1 <- df1 %>%  mutate(series = ifelse(speed>0 & speed_pre==0, "start",
                           ifelse(speed==0 & speed_pre >0, "stop",
                                  NA)))
  #Seleziono solo momenti di inizio e fine serie
  df1 <- df1 %>% filter(!is.na(series))
  
  #Creo variabile che dia l'orario della riga precedente
  df1$time_stop_pred <- lag(df1$time_ita, 1)


  #Trovo la lunghezza dello stop antecedente a start, solo per le variabili start
  df1$length_stop_pred <- ifelse(df1$series=="start", df1$time_ita - df1$time_stop_pred, NA)
  
  #Trovo lunghezza dello stop successivo a stop
  df1$time_start_next <- lead(df1$time_ita)
  df1$length_stop_next <- ifelse(df1$series=="stop", (df1$time_start_next - df1$time_ita), NA)
  
  df1 <- df1[order(df1$time_ita),]

  
  #Seleziono start e stop sulla base del parametro di tempo, in modo che le serie attaccate tra di loro risultino come la stessa serie
  df1 <- df1 %>% filter(ifelse(series=="start", length_stop_pred >= x, length_stop_next>=x))

#Genero ID di inizio serie per gli start, metto "stop" per gli stop  
  df1$id_series <- ifelse(df1$series == "start", 1:nrow(df1), "stop") 

#Unisco le info sugli start al file generale
df1 <- df1 %>% select(time_ita, speed, id_series, id)
df <- df %>% left_join(df1)
df <- df[order(df$time_ita),]  
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


