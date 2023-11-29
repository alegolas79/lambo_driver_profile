data <- data %>%
  mutate(perc_days = ifelse(is.na(perc_days), 0, perc_days))

VIN_di_TEST <- read_excel("C:/Users/l.dorsa/Downloads/BIG DATA 11-29-2021/VIN di TEST.xlsx")

VIN_di_TEST <- VIN_di_TEST %>% mutate(test = "test") %>% select(`VIN TEST`, test)

colnames(VIN_di_TEST)[1] <- "vin"
data <- data %>%
  left_join(VIN_di_TEST)

data <- data %>%
  mutate(test = ifelse(is.na(test), "not test", test))

data%>%
  ggplot(aes(perc_days))+
  geom_histogram(binwidth = 0.01)+
  theme_light()+
  ylab("")+
  xlab("Percentage of days car used")

data%>%
  filter(test == "not test") %>%
  ggplot(aes(perc_days))+
  geom_histogram(binwidth = 0.01)+
  theme_light()+
  ylab("")+
  xlab("Percentage of days car used")

data%>%
  filter(test == "test") %>%
  ggplot(aes(perc_days))+
  geom_histogram(binwidth = 0.01)+
  theme_light()+
  ylab("")+
  xlab("Percentage of days car used")

#-------------------------------------------------------------------------------

data <- data %>%
  mutate(mean_day_length_r_g = ifelse(is.na(mean_day_length_r_g), 0, mean_day_length_r_g))
data%>%
  ggplot(aes(mean_day_length_r_g/3600))+
  geom_histogram(binwidth = 0.1)+
  theme_light()+
  ylab("")+
  xlab("Average time spent using car daily")

data%>%
  filter(test == "not test") %>%
  ggplot(aes(mean_day_length_r_g/3600))+
  geom_histogram(binwidth = 0.1)+
  theme_light()+
  ylab("")+
  xlab("Average time spent using car daily")

data%>%
  filter(test == "test") %>%
  ggplot(aes(mean_day_length_r_g/3600))+
  geom_histogram(binwidth = 0.1)+
  theme_light()+
  ylab("")+
  xlab("Average time spent using car daily")

#-------------------------------------------------------------------------------
data <- data %>%
  mutate(mean_day_length_g = ifelse(is.na(mean_day_length_g), 0, mean_day_length_g))

data%>%
  filter(test == "not test") %>%
  ggplot()+
  geom_histogram(aes(mean_day_length_r_g/3600), color = "red", binwidth = 0.1)+
  geom_histogram(aes(mean_day_length_g/3600), color = "green", binwidth = 0.1)+
  theme_light()+
  ylab("")+
  xlab("Average time spent using car daily")


#-------------------------------------------------------------------------------

data <- data %>%
  mutate(overall_max = ifelse(is.na(overall_max), 0, overall_max))

data%>%
  filter(test == "not test") %>%
  ggplot(aes(overall_max))+
  geom_histogram(binwidth = 10)+
  theme_light()+
  ylab("")+
  xlab("Maximum speed")

#-------------------------------------------------------------------------------

data <- data %>%
  mutate(overall_mean = ifelse(is.na(overall_mean), 0, overall_mean))

data%>%
  filter(test == "not test") %>%
  ggplot(aes(overall_mean))+
  geom_histogram(binwidth = 5)+
  theme_light()+
  ylab("")+
  xlab("Average speed")

#-------------------------------------------------------------------------------

data %>% 
  filter(test == "not test") %>%
  filter(overall_mean>0)%>%
  ggplot(aes(x = fct_reorder(vin, overall_mean), color = perc_days, size = mean_day_length_r_g/3600))+
  geom_point(aes(y=overall_mean))+
  geom_point(aes(y=avg_daily_max))+
  coord_flip() + 
  #scale_size_continuous(range = c(3, 7))+
  theme_light()

#-------------------------------------------------------------------------------

data <- data %>% mutate(user_type = ifelse(perc_days==0, "Uso limitato", NA))





