#Problematiche dovute al fatto che in certi punti mancano i valori = 0 
drive_length <- function(df) {

  df1 <- df %>%
  select(id_series, time_ita, col)

  df1 <- df1[order(df1$time_ita),]
  
  df1 <- df1 %>%
  #trasmformo l'id in numerico
  mutate(id_series = as.numeric(as_factor(id_series)))%>%
  #seleziono soltanto le osservazioni che sono parte di una serie
  filter(!is.na(id_series))%>%
  #Creo variabili che indichino l'id dell'osservazione precedente e di quella successiva
  mutate(lag = lag(id_series))%>%
  mutate(lead = lead(id_series))%>%
  #Sulla base degli id identifico i momenti in cui la vettura parte e quelli in cui si ferma 
  mutate(series = ifelse(id_series > lag, "start", 
                         ifelse(id_series < lead, "stop", NA)))%>%
  select(!c(lag, lead))%>%
  filter(!is.na(series))

  df1 <- df1[order(df1$time_ita),]

  df1 <- df1 %>%
  #Creo il tempo dell'osservazione precedente
  mutate(time_start = lag(time_ita))%>%
  #Creo la durata della serie, uguale alla differenza tra il tempo dello start precendente e il tempo dello stop
  mutate(drive_length = ifelse(series=="stop", (time_ita - time_start), NA))%>%
  #Creo variabili che indicano il giorno e il mese dello start
  mutate(day_start = day(time_start), month_start = month(time_start))%>%
  #Creo variabile che indica il giorno della settimana 
  mutate(weekday_start = weekdays(time_start))

}



#Sistemare questione secondi dalla mezzanotte


#total_drive_length include anche le serie che abbiamo identificato come rosse

total_drive_length <- function(df1) {
  df2 <- df1 %>%
  #z è un parametro per l'eliminazione di serie troppo lunghe e quindi poco credibili, sul notebook l'ho settato come:
  #x <- mean(df1$drive_length, na.rm = TRUE)
  #y <- sd(df1$drive_length, na.rm = TRUE)
  #z <- x + (3*y)  
  #Ovvero elimino i valori che siano oltre tre deviazioni standard dalla media
  filter(drive_length < (z)) %>%
  #Il caso dei giorni in cui si guida da prima della mezzanotte a dopo la mezzanotte sono un po' problematici per il conteggio
  #Qui calcolo  i minuti che mancano alla mezzanotte dal momento dello start
  mutate(secs_to_midnight = ifelse(day(time_ita)!= day(time_start), 86400 + (00:00:00 - as_hms(time_start)), 0))%>%
  #Qui calcolo i minuti che sono passati dalla mezzanotte al momento dello start
  mutate(secs_from_midnight = ifelse(day(time_ita)!= day(time_start), as_hms(time_ita), 0)) %>%
  #Creo una variabile che indichi la guida meno i secondi passati dalla mezzanotte
  mutate(drive_length_start = drive_length - secs_from_midnight) %>%
  #Creo una variabile che indichi la data senza indicare il tempo
  mutate(date = date(time_start))
  
  #Per aggiungere i secondi passati dalla mezzanotte al conteggio della durata di guida del giorno dopo, creo un altro dateset
  df3 <- df2 %>% 
    ungroup()%>%
    #Seleziono solo i casi dove si è guidato da prima a dopo la mezzanotte
    filter(secs_from_midnight>0)
  
  df3 <- df3 %>%
    #Modifico la data in quella del giorno dopo 
    mutate(date = date + 1) %>%
    #Metto la durata di guida uguale al tempo trascorso dalla mezzanotte in poi
    mutate(drive_length_start = secs_from_midnight)%>%
    #Metto i secondi passati dalla mezzanotte uguali a zero
    mutate(secs_from_midnight = 0)%>%
    #Modifico giorni della settimana e date secondo la data del giorno successivo 
    mutate(weekday_start = weekdays(date))%>%
    mutate(day_start = day(date), month_start = month(date))
  
  #Riunisco i dataset
  df2 <- df2 %>%
    rbind(df3)
  
  df2 <- df2 %>%
  #Raggruppo per giorno e mese
  group_by(month_start, day_start)%>%
  #Calcolo il tempo di guida giornaliero
  mutate(total_drive_start = sum(drive_length_start))

  df2 <- df2 %>%
  select(day_start, month_start, weekday_start, total_drive_start, date)

  #Tengo solo le osservazioni uniche 
  df2 <- base::unique(df2)

}


#total_drive_length_green include solo le serie che abbiamo identificato come verdi
#La funzione è identica a quella di prima ma seleziono solo le serie verdi
total_drive_length_green <- function(df1) {
  df2 <- df1 %>%
    #z è un parametro per l'eliminazione di serie troppo lunghe e quindi poco credibili, sul notebook l'ho settato come:
    #x <- mean(df1$drive_length, na.rm = TRUE)
    #y <- sd(df1$drive_length, na.rm = TRUE)
    #z <- x + (3*y)  
    #Ovvero elimino i valori che siano oltre tre deviazioni standard dalla media
    filter(drive_length < (z)) %>%
    filter(col == "Green")%>%
    #Il caso dei giorni in cui si guida da prima della mezzanotte a dopo la mezzanotte sono un po' problematici per il conteggio
    #Qui calcolo  i minuti che mancano alla mezzanotte dal momento dello start
    mutate(secs_to_midnight = ifelse(day(time_ita)!= day(time_start), 86400 + (00:00:00 - as_hms(time_start)), 0))%>%
    #Qui calcolo i minuti che sono passati dalla mezzanotte al momento dello start
    mutate(secs_from_midnight = ifelse(day(time_ita)!= day(time_start), as_hms(time_ita), 0)) %>%
    #Creo una variabile che indichi la guida meno i secondi passati dalla mezzanotte
    mutate(drive_length_start = drive_length - secs_from_midnight) %>%
    #Creo una variabile che indichi la data senza indicare il tempo
    mutate(date = date(time_start))
  
  #Per aggiungere i secondi passati dalla mezzanotte al conteggio della durata di guida del giorno dopo, creo un altro dateset
  df3 <- df2 %>% 
    ungroup()%>%
    #Seleziono solo i casi dove si è guidato da prima a dopo la mezzanotte
    filter(secs_from_midnight>0)
  
  df3 <- df3 %>%
    #Modifico la data in quella del giorno dopo 
    mutate(date = date + 1) %>%
    #Metto la durata di guida uguale al tempo trascorso dalla mezzanotte in poi
    mutate(drive_length_start = secs_from_midnight)%>%
    #Metto i secondi passati dalla mezzanotte uguali a zero
    mutate(secs_from_midnight = 0)%>%
    #Modifico giorni della settimana e date secondo la data del giorno successivo 
    mutate(weekday_start = weekdays(date))%>%
    mutate(day_start = day(date), month_start = month(date))
  
  #Riunisco i dataset
  df2 <- df2 %>%
    rbind(df3)
  
  df2 <- df2 %>%
    #Raggruppo per giorno e mese
    group_by(month_start, day_start)%>%
    #Calcolo il tempo di guida giornaliero
    mutate(total_drive_start = sum(drive_length_start))
  
  df2 <- df2 %>%
    select(day_start, month_start, weekday_start, total_drive_start, date)
  
  #Tengo solo le osservazioni uniche 
  df2 <- base::unique(df2)}


