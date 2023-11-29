cluster <- read_csv("cluster1612.csv")
cluster %>%
  ggplot(aes(clusters5, avg_daily_km, color = as.factor(clusters5)))+
  geom_boxplot(lwd = 1)+
  theme_light()+
  xlab("Cluster")+
  ylab("Average daily km")+
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

  
cluster %>%
  ggplot(aes(clusters5, mean_drive_length_r_g/3600, color = as.factor(clusters5)))+
  geom_boxplot(lwd = 1)+
  theme_light()+
  xlab("Cluster")+
  ylab("Average length of drive (hours)")+
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

