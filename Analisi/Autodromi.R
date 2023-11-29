library(tidyverse)
library(data.table)


setwd("C:/Users/l.dorsa/Desktop/Lambo/Lambo_complete")
myfiles = list.files(pattern="*.csv", full.names=TRUE)

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
#auto <- read_csv("auto.csv")
auto <- lapply(myfiles,funzione_autodromi)
do.call(rbind, auto)
auto <- do.call(rbind.data.frame, auto)
write_csv(auto, "C:/Users/l.dorsa/Desktop/Lambo/auto.csv")
cluster_green <- read_csv("C:/Users/l.dorsa/Desktop/Lambo/cluster_green.csv")
auto <- auto %>%
  left_join(cluster_green)

autodromo <- auto %>%
  filter(n_autodromo>0)%>%
  select(n_autodromo, clusters5, perc_days, overall_mean, avg_daily_max, mean_day_length_g, km_percorsi)

auto %>%
  filter(n_autodromo>0)%>%
  ggplot(aes(avg_daily_max, n_autodromo, color = as.factor(clusters5)))+
 geom_point(size=7)+
  xlab("Maximum speed")+
  ylab("Number of observations near car track")+
  labs(color = "Cluster")+
  theme_light()+
  theme(axis.text.y = element_text(face="bold",
                                   size=12),
        axis.text.x = element_text(face="bold",
                                   size=12),
        axis.title=element_text(face="bold",size=12),
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(face="bold",size=14))+
  theme(plot.background = element_rect(fill = "#EEEEEE"), 
        legend.background = element_rect(fill = "#EEEEEE"), 
        legend.key = element_rect(fill = "#EEEEEE"))

