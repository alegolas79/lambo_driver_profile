library(tidyverse)
library(data.table)
library(readxl)



setwd("C:/Users/l.dorsa/Desktop/Lambo/Lambo_complete")
myfiles = list.files(pattern="*.csv", full.names=TRUE)

funzione_autodromi <- function(i){
  autodromi_ita <- read_excel("C:/Users/l.dorsa/Desktop/Lambo/Autodromi_ita.xlsx")
  df <- fread(i, select = c("vin","latitude", "longitude", "speed", "time"))
  
  autodromi_ita$Latitudine <- str_remove(autodromi_ita$Latitudine, "0000.*")
  autodromi_ita$Latitudine <- str_remove(autodromi_ita$Latitudine, "9999.*")
  autodromi_ita$Longitudine <- str_remove(autodromi_ita$Longitudine, "0000.*")
  autodromi_ita$Longitudine <- str_remove(autodromi_ita$Longitudine, "9999.*")
  autodromi_ita$Latitudine <- as.numeric(as.character(autodromi_ita$Latitudine))
  autodromi_ita$Longitudine <- as.numeric(as.character(autodromi_ita$Longitudine))
  
  autodromi_ita <-  autodromi_ita %>%
    mutate(Longitudine = ifelse(Autodromo=="Monza",  9.289444, Longitudine))%>%
    mutate(Latitudine = ifelse(Autodromo=="Cremona",  45.133333, Latitudine))
  
  colnames(autodromi_ita)[2] <- "lat"
  colnames(autodromi_ita)[3] <- "long"
  
  autodromi_ita$lat <- trunc(autodromi_ita$lat*100)/100
  autodromi_ita$long <- trunc(autodromi_ita$long*100)/100
  df$lat <- trunc(df$latitude*100)/100
  df$long <- trunc(df$longitude*100)/100
  df <- df%>%
    left_join(autodromi_ita)
  df <- df%>%
    filter(!is.na(Autodromo))
}
#auto <- read_csv("auto.csv")
auto <- lapply(myfiles,funzione_autodromi)
do.call(rbind, auto)
auto <- do.call(rbind.data.frame, auto)

cluster_green <- read_csv("C:/Users/l.dorsa/Desktop/Lambo/cluster_green.csv")
cluster_green <- cluster_green %>%
  select(vin, clusters5)

auto <- auto %>%
  left_join(cluster_green)

df <- auto %>%
  filter(speed>50)%>%
  select(c(vin, clusters5))
df <- unique(df)
df <- df %>%
  group_by(clusters5)%>%
  count()

df %>% 
  ggplot(aes(clusters5, n))+
  geom_bar(stat="identity", aes(fill=as.factor(clusters5)))+
  xlab
  

autodromi_ita <- read_excel("C:/Users/l.dorsa/Desktop/Lambo/Autodromi_ita.xlsx")
autodromi_ita$Latitudine <- str_remove(autodromi_ita$Latitudine, "0000.*")
autodromi_ita$Latitudine <- str_remove(autodromi_ita$Latitudine, "9999.*")
autodromi_ita$Longitudine <- str_remove(autodromi_ita$Longitudine, "0000.*")
autodromi_ita$Longitudine <- str_remove(autodromi_ita$Longitudine, "9999.*")
autodromi_ita$Latitudine <- as.numeric(as.character(autodromi_ita$Latitudine))
autodromi_ita$Longitudine <- as.numeric(as.character(autodromi_ita$Longitudine))
autodromi_ita <-  autodromi_ita %>%
  mutate(Longitudine = ifelse(Autodromo=="Monza",  9.289444, Longitudine))%>%
  mutate(Latitudine = ifelse(Autodromo=="Cremona",  45.133333, Latitudine))

df1 <- auto %>%
  filter(speed>50)%>%
  select(c(vin, Autodromo))
df1 <- unique(df1)
df1 <- df1 %>%
  group_by(Autodromo)%>%
  count()

autodromi_ita <- autodromi_ita %>%
  left_join(df1)



autodromi_ita <- autodromi_ita %>%
  filter(!(Autodromo=="Modena" & Latitudine==44.63315))
italy_map <- map_data("italy")
ggplot() +
  geom_polygon(italy_map, mapping = aes(x = long, y = lat, group = group), 
               fill="wheat2", colour = NA)+
  geom_point(autodromi_ita, mapping = aes(x=Longitudine, y=Latitudine, size = n), 
             color = "red", alpha=0.2)+ 
  scale_size_continuous(range = c(5,100))+
  geom_text(autodromi_ita, mapping = aes(x=Longitudine, y=Latitudine, label = Autodromo), 
            color = "gray20", check_overlap = FALSE, size = 20)+
  xlim(7.05809, 18.37819)+
  ylim(36.71703, 46.99623)+
  xlab("")+
  ylab("")+
  labs(size = "Numero di vetture:")+
  theme_light()+ 
  theme(panel.border = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank())+
  theme(panel.background = element_rect(fill = "aliceblue",
                                                           colour = "aliceblue"))+ 
  theme(axis.text.x =  element_blank(),
        axis.text.y =  element_blank(),
        axis.ticks =  element_blank(),
        legend.text = element_text(size=12), 
        legend.title = element_text(size=12))+
  theme(plot.background = element_rect(fill = "#EEEEEE"), legend.background = element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))+
  theme(legend.position = "none")



ggplot() +
  geom_polygon(italy_map, mapping = aes(x = long, y = lat, group = group), 
               fill="wheat2", colour = NA)+
  geom_point(autodromi_ita, mapping = aes(x=Longitudine, y=Latitudine, size = n), 
             color = "red", alpha=0.2)+ 
  scale_size_continuous(range = c(5,100))+
  geom_text(autodromi_ita, mapping = aes(x=Longitudine, y=Latitudine, label = Autodromo), 
            color = "gray20", check_overlap = FALSE, size = 20)+
  xlim(7.05809, 18.37819)+
  ylim(42, 46.99623)+
  xlab("")+
  ylab("")+
  labs(size = "Numero di vetture:")+
  theme_light()+ 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(panel.background = element_rect(fill = "aliceblue",
                                        colour = "aliceblue"))+ 
  theme(axis.text.x =  element_blank(),
        axis.text.y =  element_blank(),
        axis.ticks =  element_blank(),
        legend.text = element_text(size=12), 
        legend.title = element_text(size=12))+
  theme(plot.background = element_rect(fill = "#EEEEEE"), legend.background = element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))+
  theme(legend.position = "none")


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

setwd("C:/Users/l.dorsa/Desktop/Lambo/Lambo_complete")
myfiles = list.files(pattern="*.csv", full.names=TRUE)

funzione_misano <- function(i){
  df <- fread(i, select = c("vin","latitude", "longitude", "speed", "time"))
 
  df <- df %>%
    filter(latitude <= 43.9677 & latitude >= 43.9571 & longitude >= 12.6743 & longitude <= 12.6922)
}
#auto <- read_csv("auto.csv")
auto <- lapply(myfiles,funzione_misano)
do.call(rbind, auto)
auto <- do.call(rbind.data.frame, auto)

cluster_green <- read_csv("C:/Users/l.dorsa/Desktop/Lambo/cluster_green.csv")
cluster_green <- cluster_green %>%
  select(vin, clusters5)

auto <- auto %>%
  left_join(cluster_green)

write_csv(auto, "misano3.csv")

auto %>% 
  group_by(clusters5)%>%
  count(vin) %>%
  ggplot(aes(as.factor(clusters5), fill = as.factor(clusters5)))+
  geom_bar()+
  theme_light()+
  theme(legend.position = "none")+
  xlab("Cluster")+
  ylab("Numero di auto")

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

funzione_vallelunga <- function(i){
  df <- fread(i, select = c("vin","latitude", "longitude", "speed", "time"))
  
  df <- df %>%
    filter(latitude <= 42.1633 & latitude >= 42.1527 & longitude >= 12.3606 & longitude <= 12.3861)
}
#auto <- read_csv("auto.csv")
auto <- lapply(myfiles,funzione_vallelunga)
do.call(rbind, auto)
auto <- do.call(rbind.data.frame, auto)

cluster_green <- read_csv("C:/Users/l.dorsa/Desktop/Lambo/cluster_green.csv")
cluster_green <- cluster_green %>%
  select(vin, clusters5)

auto <- auto %>%
  left_join(cluster_green)

write_csv(auto, "vallelunga.csv")

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

funzione_monza <- function(i){
  df <- fread(i, select = c("vin","latitude", "longitude", "speed", "time"))
  
  df <- df %>%
    filter(latitude <= 45.6293 & latitude >= 45.6093 & longitude >= 9.2671 & longitude <= 9.3180)
}
#auto <- read_csv("auto.csv")
auto <- lapply(myfiles,funzione_monza)
do.call(rbind, auto)
auto <- do.call(rbind.data.frame, auto)

cluster_green <- read_csv("C:/Users/l.dorsa/Desktop/Lambo/cluster_green.csv")
cluster_green <- cluster_green %>%
  select(vin, clusters5)

auto <- auto %>%
  left_join(cluster_green)

write_csv(auto, "monza.csv")