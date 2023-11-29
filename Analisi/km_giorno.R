setwd("C:/Users/l.dorsa/Desktop/Lambo/Lambo_complete")
myfiles = list.files(pattern="*.csv", full.names=TRUE)
myfiles

daily_km_function <- function(i) {
df <- fread(i, select = c("vin", "time", "km_tachometer"))
df$time <- ymd_hms(df$time)
vin2 <- as.character(as.vector(df[1,1]))
df <- df %>%
  mutate(date = date(time))%>%
  mutate(km_tachometer = ifelse(km_tachometer>=1048574, NA, km_tachometer))

df <- df %>%
  group_by(date)%>%
  mutate(km_min = min(km_tachometer, na.rm = TRUE),
         km_max = max(km_tachometer, na.rm = TRUE))%>%
  mutate(km_min = ifelse(is.finite(km_min), km_min, 0),
         km_max = ifelse(is.finite(km_max), km_max, 0))%>%
mutate(daily_km = (km_max - km_min))

df <- df %>%  
  ungroup() %>%
  mutate(avg_daily_km = mean(daily_km, na.rm = TRUE))
avg_daily_km <- df[1,8]
df <- data.frame(vin = vin2, avg_daily_km = avg_daily_km)
}

km_giorno <- lapply(myfiles,daily_km_function)
do.call(rbind, km_giorno)
km_giorno <- do.call(rbind.data.frame, km_giorno)

data <- read_csv("C:/Users/l.dorsa/Desktop/Lambo/confronto_km2.csv")
data <- data %>% 
  mutate(tot_days = ifelse(is.na(tot_days), 0, tot_days))

VIN_di_TEST <- read_excel("C:/Users/l.dorsa/Downloads/BIG DATA 11-29-2021/VIN di TEST.xlsx")
VIN_di_TEST <- VIN_di_TEST %>% mutate(test = "test") %>% select(`VIN TEST`, test)
colnames(VIN_di_TEST)[1] <- "vin"
data <- data %>%
  left_join(VIN_di_TEST)
data <- data %>%
  mutate(test = ifelse(is.na(test), "not test", test))

data <- data %>%
  left_join(km_giorno)

country2 <- read_csv("C:/Users/l.dorsa/Desktop/Lambo/country2.csv")
data <- data %>%
  left_join(country2)
data$continent <- str_remove(data$country, "/.*")

cluster_green <- read_csv("C:/Users/l.dorsa/Desktop/Lambo/cluster_green.csv")
cluster_green <- cluster_green %>%
  select(vin, clusters5)
data <- data %>%
  left_join(cluster_green)

#-------------------------------------------------------------------------------
data%>%
  filter(!is.na(clusters5))%>%
  ggplot(aes(x = avg_daily_km, y = as.factor(clusters5), fill=as.factor(clusters5))) +
  geom_density_ridges(scale = 4, alpha = 0.8) + 
  scale_y_discrete(expand = c(0, 0)) +     # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) +   # for both axes to remove unneeded padding
  coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
  theme_ridges()+
  ggtitle("")+
  xlab("")+
  theme(legend.position = "none")+ 
  ylab("Numero di auto")+
  theme(axis.title.x = element_text(hjust=0.5),
        axis.title.y = element_text(hjust=0.5))+
  theme(axis.text.x = element_text(face="bold", 
                                   size=20),
        axis.text.y = element_text(face="bold",
                                   size=12),
        axis.title=element_text(size=20))+
  theme(plot.background = element_rect(fill = "#EEEEEE"), legend.background = element_rect(fill = "#EEEEEE"))

#-------------------------------------------------------------------------------
data <- data %>%
  mutate(km_bins = ifelse(avg_daily_km <=25, "0-25",
                   ifelse(avg_daily_km <=50, "25-50",
                   ifelse(avg_daily_km <=75, "50-75",
                   ifelse(avg_daily_km <=100, "75-100",
                   ifelse(avg_daily_km <=125, "100-125",
                   ifelse(avg_daily_km <=150, "125-150",
                   ifelse(avg_daily_km <=175, "150-175",
                   ifelse(avg_daily_km <=200, "175-200",
                   ifelse(avg_daily_km <=250, "200-250",
                   ifelse(avg_daily_km <=300, "250-300",
                   ifelse(avg_daily_km <=350, "300-350",
                   ifelse(avg_daily_km <=400, "350-400",  
                   ifelse(avg_daily_km <=450, "400-450",
                   ">450"))))))))))))))

data$km_bins <- factor(data$km_bins, levels = c("0-25",
                                                "25-50",
                                                "50-75",
                                                "75-100",
                                                "100-125",
                                                "125-150",
                                                "150-175",
                                                "175-200",
                                                "200-250",
                                                "250-300",
                                                "300-350",
                                                "350-400",
                                                "400-450",
                                                ">450"))
data %>%
  filter(test=="not test")%>%
  filter(continent %in% c("Europe", "America"))%>%
  ggplot(aes(km_bins))+
  geom_bar(fill = "darkblue")+
  xlab("Km percorsi al giorno")+
  ylab("Numero di auto")+
  theme_light()+
  facet_grid(vars(continent))+
  theme(axis.text.x = element_text(face="bold", 
                                   size=20),
        axis.text.y = element_text(face="bold",
                                   size=20),
        axis.title=element_text(size=20),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20))+
  theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust=1))+
  theme(plot.background = element_rect(fill = "#EEEEEE"), legend.background =     element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))

am <- data_test_free %>%
  filter(continent=="America")
summary(am)
eu <- data_test_free %>%
  filter(continent=="Europe")
summary(eu)
#-------------------------------------------------------------------------------

km_giorno <- km_giorno %>%
  mutate(km_bins = ifelse(avg_daily_km <=25, "0-25",
                          ifelse(avg_daily_km <=50, "25-50",
                                 ifelse(avg_daily_km <=75, "50-75",
                                        ifelse(avg_daily_km <=100, "75-100",
                                               ifelse(avg_daily_km <=125, "100-125",
                                                      ifelse(avg_daily_km <=150, "125-150",
                                                             ifelse(avg_daily_km <=175, "150-175",
                                                                    ifelse(avg_daily_km <=200, "175-200",
                                                                           ifelse(avg_daily_km <=250, "200-250",
                                                                                  ifelse(avg_daily_km <=300, "250-300",
                                                                                         ifelse(avg_daily_km <=350, "300-350",
                                                                                                ifelse(avg_daily_km <=400, "350-400",  
                                                                                                       ifelse(avg_daily_km <=450, "400-450",
                                                                                                              ">450"))))))))))))))

km_giorno$km_bins <- factor(km_giorno$km_bins, levels = c("0-25",
                                                "25-50",
                                                "50-75",
                                                "75-100",
                                                "100-125",
                                                "125-150",
                                                "150-175",
                                                "175-200",
                                                "200-250",
                                                "250-300",
                                                "300-350",
                                                "350-400",
                                                "400-450",
                                                ">450"))