stop_2h <- read_csv("stop_2h.csv")
length_2h <- read_csv("length_2h.csv")
km_series2 <- read_csv("km_series2.csv")
cluster_green <- read_csv("cluster_green.csv")
cluster_green <- cluster_green %>%
  select(clusters5, vin)
stop_2h <- stop_2h %>%
  select(avg_stop_length, vin)
km_series2 <- km_series2 %>%
  select(avg_km_series, vin)

cluster <- stop_2h %>%
  left_join(cluster_green)%>%
  left_join(km_series2)%>%
  left_join(length_2h)

cluster %>%
  filter(!is.na(clusters5))%>%
  ggplot(aes(clusters5, avg_km_s, color = as.factor(clusters5)))+
  geom_boxplot(lwd = 1)+
  theme_light()+
  xlab("Cluster")+
  ylab("Average daily km")+
  stat_summary(geom="text", fun.y=median,
               aes(label=sprintf("%1.1f", ..y..), color=factor(clusters5)),
               position=position_nudge(x=0.5), size=5)+
  stat_summary(geom="text", fun.y=min,
               aes(label=sprintf("%1.1f", ..y..), color=factor(clusters5)),
               position=position_nudge(x=0.5), size=5)+
  stat_summary(geom="text", fun.y=max,
               aes(label=sprintf("%1.1f", ..y..), color=factor(clusters5)),
               position=position_nudge(x=0.5), size=5)+
  theme(axis.text.x = element_text(face="bold", 
                                   size=12),
        axis.text.y = element_text(face="bold",
                                   size=12),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(face="bold",size=22))+
  theme(legend.position="none",plot.background = element_rect(fill = "#EEEEEE"), legend.background =     element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))

  
cluster %>%
  ggplot(aes(clusters5, mean_drive_length_r_g/3600, color = as.factor(clusters5)))+
  geom_boxplot(lwd = 1)+
  theme_light()+
  xlab("Cluster")+
  ylab("Average length of drive (hours)")+
  stat_summary(geom="text", fun.y=median,
               aes(label=sprintf("%1.1f", ..y..), color=factor(clusters5)),
               position=position_nudge(x=0.5), size=5)+
  stat_summary(geom="text", fun.y=min,
               aes(label=sprintf("%1.1f", ..y..), color=factor(clusters5)),
               position=position_nudge(x=0.5), size=5)+
  stat_summary(geom="text", fun.y=max,
               aes(label=sprintf("%1.1f", ..y..), color=factor(clusters5)),
               position=position_nudge(x=0.5), size=5)+
  theme(axis.text.x = element_text(face="bold", 
                                   size=12),
        axis.text.y = element_text(face="bold",
                                   size=12),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(face="bold",size=22))+
  theme(legend.position="none",plot.background = element_rect(fill = "#EEEEEE"), legend.background =     element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))



cluster %>%
  ggplot(aes(clusters5, mean_day_length_r_g/3600, color = as.factor(clusters5)))+
  geom_boxplot(lwd = 1)+
  theme_light()+
  xlab("Cluster")+
  ylab("Average time spent driving in a day (hours)")+
  stat_summary(geom="text", fun.y=quantile,
               aes(label=sprintf("%1.1f", ..y..), color=factor(clusters5)),
               position=position_nudge(x=0.5), size=5)+
  theme(axis.text.x = element_text(face="bold", 
                                   size=12),
        axis.text.y = element_text(face="bold",
                                   size=12),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(face="bold",size=22))+
  theme(legend.position="none",plot.background = element_rect(fill = "#EEEEEE"), legend.background =     element_rect(fill = "#EEEEEE"), legend.key = element_rect(fill = "#EEEEEE"))

