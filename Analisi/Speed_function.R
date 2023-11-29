library(data.table)
library(tidyverse)
library(lubridate)

myfiles = list.files(pattern="*.csv", full.names=TRUE)
myfiles

speed <- function(i) {
  
  df <- fread(i, select = c("time", "speed", "vin"))
  df$time <- ymd_hms(df$time)
  vin2 <- as.character(as.vector(df[1,3]))
  df <- df %>%
    filter(!is.na(speed))%>%
    filter(speed > 0)
  df <- df%>%
    mutate(month = month(time), day = day(time))%>%
    group_by(month, day)%>%
    mutate(daily_max = max(speed), daily_mean = mean(speed))%>%
    ungroup()%>%
    mutate(avg_daily_max = mean(daily_max), avg_daily_mean = mean(daily_mean), 
           overall_max = max(speed), overall_mean = mean(speed))%>%
    select(vin, avg_daily_max, avg_daily_mean, overall_max, overall_mean)
  
  df <- df[1,]
  df <- df %>%
    mutate(vin = vin2)
  
}

O <- lapply(myfiles,speed)
do.call(rbind, O)
x <- do.call(rbind.data.frame, O)



