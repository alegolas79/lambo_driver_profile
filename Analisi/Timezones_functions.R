library(tidyverse)
library(lubridate)
library(hms)
library(rworldmap)
library(data.table)
library(lutz)
#df must contain, in this order, c("time","latitude","longitude","altitude","speed"))

#initial_setup: filters NAs, orders by time and fills latitude and longitude so as to be able to convert all available dates and times
initial_setup <- function(df) {
  df <- df %>%
    filter(!(is.na(latitude) & is.na(speed)))
  
  df <- df[order(df$time),]
  df <- df %>% mutate(id = 1:nrow(df))
  df <- df %>%
    fill(latitude, .direction = "down")%>%
    fill(longitude, .direction = "down")
}



#timezone_cet splits the dataset by location, sets timezones based on location and converts time to CET

timezone_cet <- function(df, names, x){
  if(length(x)==1) {
  X<-split(df, df$names)  
  a <- as.data.frame(X[[1]])
  a$a <- a$time
  timezone_a <- a[1,7]
  a$a <- force_tz(a$a, tzone = timezone_a)
  a$time_ita <- with_tz(a$a, tzone ="CET")
  a <- a %>% select(longitude, latitude, time, time_ita, id)
  df <- df %>%
    left_join(a)}
  else if (length(x)==2) {
    X<-split(df, df$names)
    a <- as.data.frame(X[[1]])
    a$a <- a$time
    timezone_a <- a[1,7]
    a$a <- force_tz(a$a, tzone = timezone_a)
    a$time_ita <- with_tz(a$a, tzone ="CET")
    a <- a %>% select(longitude, latitude, time, time_ita, id)
    b <- as.data.frame(X[[2]])
    b$a <- b$time
    timezone_b <- b[1,7]
    b$a <- force_tz(b$a, tzone = timezone_b)
    b$time_ita <- with_tz(b$a, tzone ="CET")
    b <- b %>% select(longitude, latitude, time, time_ita, id)
    a <- a %>%
      rbind(b)
    df <- df %>%
      left_join(a)}
  else if (length(x)==3) {
  X<-split(df, df$names)
  a <- as.data.frame(X[[1]])
  a$a <- a$time
  timezone_a <- a[1,7]
  a$a <- force_tz(a$a, tzone = timezone_a)
  a$time_ita <- with_tz(a$a, tzone ="CET")
  a <- a %>% select(longitude, latitude, time, time_ita, id)
  b <- as.data.frame(X[[2]])
  b$a <- b$time
  timezone_b <- b[1,7]
  b$a <- force_tz(b$a, tzone = timezone_b)
  b$time_ita <- with_tz(b$a, tzone ="CET")
  b <- b %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(b)
  c <- as.data.frame(X[[3]])
  c$a <- c$time
  timezone_c <- c[1,7]
  c$a <- force_tz(c$a, tzone = timezone_c)
  c$time_ita <- with_tz(c$a, tzone ="CET")
  c <- c %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(c)
  df <- df %>%
    left_join(a)}
else if (length(x)==4) {
  X<-split(df, df$names)
  a <- as.data.frame(X[[1]])
  a$a <- a$time
  timezone_a <- a[1,7]
  a$a <- force_tz(a$a, tzone = timezone_a)
  a$time_ita <- with_tz(a$a, tzone ="CET")
  a <- a %>% select(longitude, latitude, time, time_ita, id)
  b <- as.data.frame(X[[2]])
  b$a <- b$time
  timezone_b <- b[1,7]
  b$a <- force_tz(b$a, tzone = timezone_b)
  b$time_ita <- with_tz(b$a, tzone ="CET")
  b <- b %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(b)
  c <- as.data.frame(X[[3]])
  c$a <- c$time
  timezone_c <- c[1,7]
  c$a <- force_tz(c$a, tzone = timezone_c)
  c$time_ita <- with_tz(c$a, tzone ="CET")
  c <- c %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(c)
  d <- as.data.frame(X[[4]])
  d$a <- d$time
  timezone_d <- d[1,7]
  d$a <- force_tz(d$a, tzone = timezone_d)
  d$time_ita <- with_tz(d$a, tzone ="CET")
  d <- d %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(d)
  df <- df %>%
    left_join(a)}
else if (length(x)==5) {
  X<-split(df, df$names)
  a <- as.data.frame(X[[1]])
  a$a <- a$time
  timezone_a <- a[1,7]
  a$a <- force_tz(a$a, tzone = timezone_a)
  a$time_ita <- with_tz(a$a, tzone ="CET")
  a <- a %>% select(longitude, latitude, time, time_ita, id)
  b <- as.data.frame(X[[2]])
  b$a <- b$time
  timezone_b <- b[1,7]
  b$a <- force_tz(b$a, tzone = timezone_b)
  b$time_ita <- with_tz(b$a, tzone ="CET")
  b <- b %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(b)
  c <- as.data.frame(X[[3]])
  c$a <- c$time
  timezone_c <- c[1,7]
  c$a <- force_tz(c$a, tzone = timezone_c)
  c$time_ita <- with_tz(c$a, tzone ="CET")
  c <- c %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(c)
  d <- as.data.frame(X[[4]])
  d$a <- d$time
  timezone_d <- d[1,7]
  d$a <- force_tz(d$a, tzone = timezone_d)
  d$time_ita <- with_tz(d$a, tzone ="CET")
  d <- d %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(d)
  e <- as.data.frame(X[[5]])
  e$a <- e$time
  timezone_e <- e[1,7]
  e$a <- force_tz(e$a, tzone = timezone_e)
  e$time_ita <- with_tz(e$a, tzone ="CET")
  e <- e %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(e)
  df <- df %>%
    left_join(a)}
else if (length(x)==6) {
  X<-split(df, df$names)
  a <- as.data.frame(X[[1]])
  a$a <- a$time
  timezone_a <- a[1,7]
  a$a <- force_tz(a$a, tzone = timezone_a)
  a$time_ita <- with_tz(a$a, tzone ="CET")
  a <- a %>% select(longitude, latitude, time, time_ita, id)
  b <- as.data.frame(X[[2]])
  b$a <- b$time
  timezone_b <- b[1,7]
  b$a <- force_tz(b$a, tzone = timezone_b)
  b$time_ita <- with_tz(b$a, tzone ="CET")
  b <- b %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(b)
  c <- as.data.frame(X[[3]])
  c$a <- c$time
  timezone_c <- c[1,7]
  c$a <- force_tz(c$a, tzone = timezone_c)
  c$time_ita <- with_tz(c$a, tzone ="CET")
  c <- c %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(c)
  d <- as.data.frame(X[[4]])
  d$a <- d$time
  timezone_d <- d[1,7]
  d$a <- force_tz(d$a, tzone = timezone_d)
  d$time_ita <- with_tz(d$a, tzone ="CET")
  d <- d %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(d)
  e <- as.data.frame(X[[5]])
  e$a <- e$time
  timezone_e <- e[1,7]
  e$a <- force_tz(e$a, tzone = timezone_e)
  e$time_ita <- with_tz(e$a, tzone ="CET")
  e <- e %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(e)
  f <- as.data.frame(X[[6]])
  f$a <- f$time
  timezone_f <- f[1,7]
  f$a <- force_tz(f$a, tzone = timezone_f)
  f$time_ita <- with_tz(f$a, tzone ="CET")
  f <- f %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(f)
  df <- df %>%
    left_join(a)}
else if (length(x)==7) {
  X<-split(df, df$names)
  a <- as.data.frame(X[[1]])
  a$a <- a$time
  timezone_a <- a[1,7]
  a$a <- force_tz(a$a, tzone = timezone_a)
  a$time_ita <- with_tz(a$a, tzone ="CET")
  a <- a %>% select(longitude, latitude, time, time_ita, id)
  b <- as.data.frame(X[[2]])
  b$a <- b$time
  timezone_b <- b[1,7]
  b$a <- force_tz(b$a, tzone = timezone_b)
  b$time_ita <- with_tz(b$a, tzone ="CET")
  b <- b %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(b)
  c <- as.data.frame(X[[3]])
  c$a <- c$time
  timezone_c <- c[1,7]
  c$a <- force_tz(c$a, tzone = timezone_c)
  c$time_ita <- with_tz(c$a, tzone ="CET")
  c <- c %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(c)
  d <- as.data.frame(X[[4]])
  d$a <- d$time
  timezone_d <- d[1,7]
  d$a <- force_tz(d$a, tzone = timezone_d)
  d$time_ita <- with_tz(d$a, tzone ="CET")
  d <- d %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(d)
  e <- as.data.frame(X[[5]])
  e$a <- e$time
  timezone_e <- e[1,7]
  e$a <- force_tz(e$a, tzone = timezone_e)
  e$time_ita <- with_tz(e$a, tzone ="CET")
  e <- e %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(e)
  f <- as.data.frame(X[[6]])
  f$a <- f$time
  timezone_f <- f[1,7]
  f$a <- force_tz(f$a, tzone = timezone_f)
  f$time_ita <- with_tz(f$a, tzone ="CET")
  f <- f %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(f)
  g <- as.data.frame(X[[7]])
  g$a <- g$time
  timezone_g <- f[1,7]
  g$a <- force_tz(g$a, tzone = timezone_g)
  g$time_ita <- with_tz(g$a, tzone ="CET")
  g <- g %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(g)
  df <- df %>%
    left_join(a)}
else if (length(x)==8) {
  X<-split(df, df$names)
  a <- as.data.frame(X[[1]])
  a$a <- a$time
  timezone_a <- a[1,7]
  a$a <- force_tz(a$a, tzone = timezone_a)
  a$time_ita <- with_tz(a$a, tzone ="CET")
  a <- a %>% select(longitude, latitude, time, time_ita, id)
  b <- as.data.frame(X[[2]])
  b$a <- b$time
  timezone_b <- b[1,7]
  b$a <- force_tz(b$a, tzone = timezone_b)
  b$time_ita <- with_tz(b$a, tzone ="CET")
  b <- b %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(b)
  c <- as.data.frame(X[[3]])
  c$a <- c$time
  timezone_c <- c[1,7]
  c$a <- force_tz(c$a, tzone = timezone_c)
  c$time_ita <- with_tz(c$a, tzone ="CET")
  c <- c %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(c)
  d <- as.data.frame(X[[4]])
  d$a <- d$time
  timezone_d <- d[1,7]
  d$a <- force_tz(d$a, tzone = timezone_d)
  d$time_ita <- with_tz(d$a, tzone ="CET")
  d <- d %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(d)
  e <- as.data.frame(X[[5]])
  e$a <- e$time
  timezone_e <- e[1,7]
  e$a <- force_tz(e$a, tzone = timezone_e)
  e$time_ita <- with_tz(e$a, tzone ="CET")
  e <- e %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(e)
  f <- as.data.frame(X[[6]])
  f$a <- f$time
  timezone_f <- f[1,7]
  f$a <- force_tz(f$a, tzone = timezone_f)
  f$time_ita <- with_tz(f$a, tzone ="CET")
  f <- f %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(f)
  g <- as.data.frame(X[[7]])
  g$a <- g$time
  timezone_g <- g[1,7]
  g$a <- force_tz(g$a, tzone = timezone_g)
  g$time_ita <- with_tz(g$a, tzone ="CET")
  g <- g %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(g)
  h <- as.data.frame(X[[8]])
  h$a <- h$time
  timezone_h <- h[1,7]
  h$a <- force_tz(h$a, tzone = timezone_h)
  h$time_ita <- with_tz(h$a, tzone ="CET")
  h <- h %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(h)
  df <- df %>%
    left_join(a)}
else if (length(x)==9) {
  X<-split(df, df$names)
  a <- as.data.frame(X[[1]])
  a$a <- a$time
  timezone_a <- a[1,7]
  a$a <- force_tz(a$a, tzone = timezone_a)
  a$time_ita <- with_tz(a$a, tzone ="CET")
  a <- a %>% select(longitude, latitude, time, time_ita, id)
  b <- as.data.frame(X[[2]])
  b$a <- b$time
  timezone_b <- b[1,7]
  b$a <- force_tz(b$a, tzone = timezone_b)
  b$time_ita <- with_tz(b$a, tzone ="CET")
  b <- b %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(b)
  c <- as.data.frame(X[[3]])
  c$a <- c$time
  timezone_c <- c[1,7]
  c$a <- force_tz(c$a, tzone = timezone_c)
  c$time_ita <- with_tz(c$a, tzone ="CET")
  c <- c %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(c)
  d <- as.data.frame(X[[4]])
  d$a <- d$time
  timezone_d <- d[1,7]
  d$a <- force_tz(d$a, tzone = timezone_d)
  d$time_ita <- with_tz(d$a, tzone ="CET")
  d <- d %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(d)
  e <- as.data.frame(X[[5]])
  e$a <- e$time
  timezone_e <- e[1,7]
  e$a <- force_tz(e$a, tzone = timezone_e)
  e$time_ita <- with_tz(e$a, tzone ="CET")
  e <- e %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(e)
  f <- as.data.frame(X[[6]])
  f$a <- f$time
  timezone_f <- f[1,7]
  f$a <- force_tz(f$a, tzone = timezone_f)
  f$time_ita <- with_tz(f$a, tzone ="CET")
  f <- f %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(f)
  g <- as.data.frame(X[[7]])
  g$a <- g$time
  timezone_g <- g[1,7]
  g$a <- force_tz(g$a, tzone = timezone_g)
  g$time_ita <- with_tz(g$a, tzone ="CET")
  g <- g %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(g)
  h <- as.data.frame(X[[8]])
  h$a <- h$time
  timezone_h <- h[1,7]
  h$a <- force_tz(h$a, tzone = timezone_h)
  h$time_ita <- with_tz(h$a, tzone ="CET")
  h <- h %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(h)
  i <- as.data.frame(X[[9]])
  i$a <- i$time
  timezone_i <- i[1,7]
  i$a <- force_tz(i$a, tzone = timezone_i)
  i$time_ita <- with_tz(i$a, tzone ="CET")
  i <- i %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(i)
  df <- df %>%
    left_join(a)}
else if (length(x)==10) {
  X<-split(df, df$names)
  a <- as.data.frame(X[[1]])
  a$a <- a$time
  timezone_a <- a[1,7]
  a$a <- force_tz(a$a, tzone = timezone_a)
  a$time_ita <- with_tz(a$a, tzone ="CET")
  a <- a %>% select(longitude, latitude, time, time_ita, id)
  b <- as.data.frame(X[[2]])
  b$a <- b$time
  timezone_b <- b[1,7]
  b$a <- force_tz(b$a, tzone = timezone_b)
  b$time_ita <- with_tz(b$a, tzone ="CET")
  b <- b %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(b)
  c <- as.data.frame(X[[3]])
  c$a <- c$time
  timezone_c <- c[1,7]
  c$a <- force_tz(c$a, tzone = timezone_c)
  c$time_ita <- with_tz(c$a, tzone ="CET")
  c <- c %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(c)
  d <- as.data.frame(X[[4]])
  d$a <- d$time
  timezone_d <- d[1,7]
  d$a <- force_tz(d$a, tzone = timezone_d)
  d$time_ita <- with_tz(d$a, tzone ="CET")
  d <- d %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(d)
  e <- as.data.frame(X[[5]])
  e$a <- e$time
  timezone_e <- e[1,7]
  e$a <- force_tz(e$a, tzone = timezone_e)
  e$time_ita <- with_tz(e$a, tzone ="CET")
  e <- e %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(e)
  f <- as.data.frame(X[[6]])
  f$a <- f$time
  timezone_f <- f[1,7]
  f$a <- force_tz(f$a, tzone = timezone_f)
  f$time_ita <- with_tz(f$a, tzone ="CET")
  f <- f %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(f)
  g <- as.data.frame(X[[7]])
  g$a <- g$time
  timezone_g <- g[1,7]
  g$a <- force_tz(g$a, tzone = timezone_g)
  g$time_ita <- with_tz(g$a, tzone ="CET")
  g <- g %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(g)
  h <- as.data.frame(X[[8]])
  h$a <- h$time
  timezone_h <- h[1,7]
  h$a <- force_tz(h$a, tzone = timezone_h)
  h$time_ita <- with_tz(h$a, tzone ="CET")
  h <- h %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(h)
  i <- as.data.frame(X[[9]])
  i$a <- i$time
  timezone_i <- i[1,7]
  i$a <- force_tz(i$a, tzone = timezone_i)
  i$time_ita <- with_tz(i$a, tzone ="CET")
  i <- i %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(i)
  j <- as.data.frame(X[[10]])
  j$a <- j$time
  timezone_j <- j[1,7]
  j$a <- force_tz(j$a, tzone = timezone_j)
  j$time_ita <- with_tz(j$a, tzone ="CET")
  j <- j %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(j)
  df <- df %>%
    left_join(a)}
else if (length(x)==11) {
  X<-split(df, df$names)
  a <- as.data.frame(X[[1]])
  a$a <- a$time
  timezone_a <- a[1,7]
  a$a <- force_tz(a$a, tzone = timezone_a)
  a$time_ita <- with_tz(a$a, tzone ="CET")
  a <- a %>% select(longitude, latitude, time, time_ita, id)
  b <- as.data.frame(X[[2]])
  b$a <- b$time
  timezone_b <- b[1,7]
  b$a <- force_tz(b$a, tzone = timezone_b)
  b$time_ita <- with_tz(b$a, tzone ="CET")
  b <- b %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(b)
  c <- as.data.frame(X[[3]])
  c$a <- c$time
  timezone_c <- c[1,7]
  c$a <- force_tz(c$a, tzone = timezone_c)
  c$time_ita <- with_tz(c$a, tzone ="CET")
  c <- c %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(c)
  d <- as.data.frame(X[[4]])
  d$a <- d$time
  timezone_d <- d[1,7]
  d$a <- force_tz(d$a, tzone = timezone_d)
  d$time_ita <- with_tz(d$a, tzone ="CET")
  d <- d %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(d)
  e <- as.data.frame(X[[5]])
  e$a <- e$time
  timezone_e <- e[1,7]
  e$a <- force_tz(e$a, tzone = timezone_e)
  e$time_ita <- with_tz(e$a, tzone ="CET")
  e <- e %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(e)
  f <- as.data.frame(X[[6]])
  f$a <- f$time
  timezone_f <- f[1,7]
  f$a <- force_tz(f$a, tzone = timezone_f)
  f$time_ita <- with_tz(f$a, tzone ="CET")
  f <- f %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(f)
  g <- as.data.frame(X[[7]])
  g$a <- g$time
  timezone_g <- g[1,7]
  g$a <- force_tz(g$a, tzone = timezone_g)
  g$time_ita <- with_tz(g$a, tzone ="CET")
  g <- g %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(g)
  h <- as.data.frame(X[[8]])
  h$a <- h$time
  timezone_h <- h[1,7]
  h$a <- force_tz(h$a, tzone = timezone_h)
  h$time_ita <- with_tz(h$a, tzone ="CET")
  h <- h %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(h)
  i <- as.data.frame(X[[9]])
  i$a <- i$time
  timezone_i <- i[1,7]
  i$a <- force_tz(i$a, tzone = timezone_i)
  i$time_ita <- with_tz(i$a, tzone ="CET")
  i <- i %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(i)
  j <- as.data.frame(X[[10]])
  j$a <- j$time
  timezone_j <- j[1,7]
  j$a <- force_tz(j$a, tzone = timezone_j)
  j$time_ita <- with_tz(j$a, tzone ="CET")
  j <- j %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(j)
  k <- as.data.frame(X[[11]])
  k$a <- k$time
  timezone_k <- k[1,7]
  k$a <- force_tz(k$a, tzone = timezone_k)
  k$time_ita <- with_tz(k$a, tzone ="CET")
  k <- k %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(k)
  df <- df %>%
    left_join(a)}
else if (length(x)==12) {
  X<-split(df, df$names)
  a <- as.data.frame(X[[1]])
  a$a <- a$time
  timezone_a <- a[1,7]
  a$a <- force_tz(a$a, tzone = timezone_a)
  a$time_ita <- with_tz(a$a, tzone ="CET")
  a <- a %>% select(longitude, latitude, time, time_ita, id)
  b <- as.data.frame(X[[2]])
  b$a <- b$time
  timezone_b <- b[1,7]
  b$a <- force_tz(b$a, tzone = timezone_b)
  b$time_ita <- with_tz(b$a, tzone ="CET")
  b <- b %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(b)
  c <- as.data.frame(X[[3]])
  c$a <- c$time
  timezone_c <- c[1,7]
  c$a <- force_tz(c$a, tzone = timezone_c)
  c$time_ita <- with_tz(c$a, tzone ="CET")
  c <- c %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(c)
  d <- as.data.frame(X[[4]])
  d$a <- d$time
  timezone_d <- d[1,7]
  d$a <- force_tz(d$a, tzone = timezone_d)
  d$time_ita <- with_tz(d$a, tzone ="CET")
  d <- d %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(d)
  e <- as.data.frame(X[[5]])
  e$a <- e$time
  timezone_e <- e[1,7]
  e$a <- force_tz(e$a, tzone = timezone_e)
  e$time_ita <- with_tz(e$a, tzone ="CET")
  e <- e %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(e)
  f <- as.data.frame(X[[6]])
  f$a <- f$time
  timezone_f <- f[1,7]
  f$a <- force_tz(f$a, tzone = timezone_f)
  f$time_ita <- with_tz(f$a, tzone ="CET")
  f <- f %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(f)
  g <- as.data.frame(X[[7]])
  g$a <- g$time
  timezone_g <- g[1,7]
  g$a <- force_tz(g$a, tzone = timezone_g)
  g$time_ita <- with_tz(g$a, tzone ="CET")
  g <- g %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(g)
  h <- as.data.frame(X[[8]])
  h$a <- h$time
  timezone_h <- h[1,7]
  h$a <- force_tz(h$a, tzone = timezone_h)
  h$time_ita <- with_tz(h$a, tzone ="CET")
  h <- h %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(h)
  i <- as.data.frame(X[[9]])
  i$a <- i$time
  timezone_i <- i[1,7]
  i$a <- force_tz(i$a, tzone = timezone_i)
  i$time_ita <- with_tz(i$a, tzone ="CET")
  i <- i %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(i)
  j <- as.data.frame(X[[10]])
  j$a <- j$time
  timezone_j <- j[1,7]
  j$a <- force_tz(j$a, tzone = timezone_j)
  j$time_ita <- with_tz(j$a, tzone ="CET")
  j <- j %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(j)
  k <- as.data.frame(X[[11]])
  k$a <- k$time
  timezone_k <- k[1,7]
  k$a <- force_tz(k$a, tzone = timezone_k)
  k$time_ita <- with_tz(k$a, tzone ="CET")
  k <- k %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(k)
  l <- as.data.frame(X[[12]])
  l$a <- l$time
  timezone_l <- l[1,7]
  l$a <- force_tz(l$a, tzone = timezone_l)
  l$time_ita <- with_tz(l$a, tzone ="CET")
  l <- l %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(l)
  df <- df %>%
    left_join(a)}
else if (length(x)==13) {
  X<-split(df, df$names)
  a <- as.data.frame(X[[1]])
  a$a <- a$time
  timezone_a <- a[1,7]
  a$a <- force_tz(a$a, tzone = timezone_a)
  a$time_ita <- with_tz(a$a, tzone ="CET")
  a <- a %>% select(longitude, latitude, time, time_ita, id)
  b <- as.data.frame(X[[2]])
  b$a <- b$time
  timezone_b <- b[1,7]
  b$a <- force_tz(b$a, tzone = timezone_b)
  b$time_ita <- with_tz(b$a, tzone ="CET")
  b <- b %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(b)
  c <- as.data.frame(X[[3]])
  c$a <- c$time
  timezone_c <- c[1,7]
  c$a <- force_tz(c$a, tzone = timezone_c)
  c$time_ita <- with_tz(c$a, tzone ="CET")
  c <- c %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(c)
  d <- as.data.frame(X[[4]])
  d$a <- d$time
  timezone_d <- d[1,7]
  d$a <- force_tz(d$a, tzone = timezone_d)
  d$time_ita <- with_tz(d$a, tzone ="CET")
  d <- d %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(d)
  e <- as.data.frame(X[[5]])
  e$a <- e$time
  timezone_e <- e[1,7]
  e$a <- force_tz(e$a, tzone = timezone_e)
  e$time_ita <- with_tz(e$a, tzone ="CET")
  e <- e %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(e)
  f <- as.data.frame(X[[6]])
  f$a <- f$time
  timezone_f <- f[1,7]
  f$a <- force_tz(f$a, tzone = timezone_f)
  f$time_ita <- with_tz(f$a, tzone ="CET")
  f <- f %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(f)
  g <- as.data.frame(X[[7]])
  g$a <- g$time
  timezone_g <- g[1,7]
  g$a <- force_tz(g$a, tzone = timezone_g)
  g$time_ita <- with_tz(g$a, tzone ="CET")
  g <- g %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(g)
  h <- as.data.frame(X[[8]])
  h$a <- h$time
  timezone_h <- h[1,7]
  h$a <- force_tz(h$a, tzone = timezone_h)
  h$time_ita <- with_tz(h$a, tzone ="CET")
  h <- h %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(h)
  i <- as.data.frame(X[[9]])
  i$a <- i$time
  timezone_i <- i[1,7]
  i$a <- force_tz(i$a, tzone = timezone_i)
  i$time_ita <- with_tz(i$a, tzone ="CET")
  i <- i %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(i)
  j <- as.data.frame(X[[10]])
  j$a <- j$time
  timezone_j <- j[1,7]
  j$a <- force_tz(j$a, tzone = timezone_j)
  j$time_ita <- with_tz(j$a, tzone ="CET")
  j <- j %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(j)
  k <- as.data.frame(X[[11]])
  k$a <- k$time
  timezone_k <- k[1,7]
  k$a <- force_tz(k$a, tzone = timezone_k)
  k$time_ita <- with_tz(k$a, tzone ="CET")
  k <- k %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(k)
  l <- as.data.frame(X[[12]])
  l$a <- l$time
  timezone_l <- l[1,7]
  l$a <- force_tz(l$a, tzone = timezone_l)
  l$time_ita <- with_tz(l$a, tzone ="CET")
  l <- l %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(l)
  m <- as.data.frame(X[[13]])
  m$a <- m$time
  timezone_m <- m[1,7]
  m$a <- force_tz(m$a, tzone = timezone_m)
  m$time_ita <- with_tz(m$a, tzone ="CET")
  m <- m %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(m)
  df <- df %>%
    left_join(a)}
else if (length(x)==14) {
  X<-split(df, df$names)
  a <- as.data.frame(X[[1]])
  a$a <- a$time
  timezone_a <- a[1,7]
  a$a <- force_tz(a$a, tzone = timezone_a)
  a$time_ita <- with_tz(a$a, tzone ="CET")
  a <- a %>% select(longitude, latitude, time, time_ita, id)
  b <- as.data.frame(X[[2]])
  b$a <- b$time
  timezone_b <- b[1,7]
  b$a <- force_tz(b$a, tzone = timezone_b)
  b$time_ita <- with_tz(b$a, tzone ="CET")
  b <- b %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(b)
  c <- as.data.frame(X[[3]])
  c$a <- c$time
  timezone_c <- c[1,7]
  c$a <- force_tz(c$a, tzone = timezone_c)
  c$time_ita <- with_tz(c$a, tzone ="CET")
  c <- c %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(c)
  d <- as.data.frame(X[[4]])
  d$a <- d$time
  timezone_d <- d[1,7]
  d$a <- force_tz(d$a, tzone = timezone_d)
  d$time_ita <- with_tz(d$a, tzone ="CET")
  d <- d %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(d)
  e <- as.data.frame(X[[5]])
  e$a <- e$time
  timezone_e <- e[1,7]
  e$a <- force_tz(e$a, tzone = timezone_e)
  e$time_ita <- with_tz(e$a, tzone ="CET")
  e <- e %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(e)
  f <- as.data.frame(X[[6]])
  f$a <- f$time
  timezone_f <- f[1,7]
  f$a <- force_tz(f$a, tzone = timezone_f)
  f$time_ita <- with_tz(f$a, tzone ="CET")
  f <- f %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(f)
  g <- as.data.frame(X[[7]])
  g$a <- g$time
  timezone_g <- g[1,7]
  g$a <- force_tz(g$a, tzone = timezone_g)
  g$time_ita <- with_tz(g$a, tzone ="CET")
  g <- g %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(g)
  h <- as.data.frame(X[[8]])
  h$a <- h$time
  timezone_h <- h[1,7]
  h$a <- force_tz(h$a, tzone = timezone_h)
  h$time_ita <- with_tz(h$a, tzone ="CET")
  h <- h %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(h)
  i <- as.data.frame(X[[9]])
  i$a <- i$time
  timezone_i <- i[1,7]
  i$a <- force_tz(i$a, tzone = timezone_i)
  i$time_ita <- with_tz(i$a, tzone ="CET")
  i <- i %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(i)
  j <- as.data.frame(X[[10]])
  j$a <- j$time
  timezone_j <- j[1,7]
  j$a <- force_tz(j$a, tzone = timezone_j)
  j$time_ita <- with_tz(j$a, tzone ="CET")
  j <- j %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(j)
  k <- as.data.frame(X[[11]])
  k$a <- k$time
  timezone_k <- k[1,7]
  k$a <- force_tz(k$a, tzone = timezone_k)
  k$time_ita <- with_tz(k$a, tzone ="CET")
  k <- k %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(k)
  l <- as.data.frame(X[[12]])
  l$a <- l$time
  timezone_l <- l[1,7]
  l$a <- force_tz(l$a, tzone = timezone_l)
  l$time_ita <- with_tz(l$a, tzone ="CET")
  l <- l %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(l)
  m <- as.data.frame(X[[13]])
  m$a <- m$time
  timezone_m <- m[1,7]
  m$a <- force_tz(m$a, tzone = timezone_m)
  m$time_ita <- with_tz(m$a, tzone ="CET")
  m <- m %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(m)
  n <- as.data.frame(X[[14]])
  n$a <- n$time
  timezone_n <- n[1,7]
  n$a <- force_tz(n$a, tzone = timezone_n)
  n$time_ita <- with_tz(n$a, tzone ="CET")
  n <- n %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(n)
  df <- df %>%
    left_join(a)}
else {
  X<-split(df, df$names)
  a <- as.data.frame(X[[1]])
  a$a <- a$time
  timezone_a <- a[1,7]
  a$a <- force_tz(a$a, tzone = timezone_a)
  a$time_ita <- with_tz(a$a, tzone ="CET")
  a <- a %>% select(longitude, latitude, time, time_ita, id)
  b <- as.data.frame(X[[2]])
  b$a <- b$time
  timezone_b <- b[1,7]
  b$a <- force_tz(b$a, tzone = timezone_b)
  b$time_ita <- with_tz(b$a, tzone ="CET")
  b <- b %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(b)
  c <- as.data.frame(X[[3]])
  c$a <- c$time
  timezone_c <- c[1,7]
  c$a <- force_tz(c$a, tzone = timezone_c)
  c$time_ita <- with_tz(c$a, tzone ="CET")
  c <- c %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(c)
  d <- as.data.frame(X[[4]])
  d$a <- d$time
  timezone_d <- d[1,7]
  d$a <- force_tz(d$a, tzone = timezone_d)
  d$time_ita <- with_tz(d$a, tzone ="CET")
  d <- d %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(d)
  e <- as.data.frame(X[[5]])
  e$a <- e$time
  timezone_e <- e[1,7]
  e$a <- force_tz(e$a, tzone = timezone_e)
  e$time_ita <- with_tz(e$a, tzone ="CET")
  e <- e %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(e)
  f <- as.data.frame(X[[6]])
  f$a <- f$time
  timezone_f <- f[1,7]
  f$a <- force_tz(f$a, tzone = timezone_f)
  f$time_ita <- with_tz(f$a, tzone ="CET")
  f <- f %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(f)
  g <- as.data.frame(X[[7]])
  g$a <- g$time
  timezone_g <- g[1,7]
  g$a <- force_tz(g$a, tzone = timezone_g)
  g$time_ita <- with_tz(g$a, tzone ="CET")
  g <- g %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(g)
  h <- as.data.frame(X[[8]])
  h$a <- h$time
  timezone_h <- h[1,7]
  h$a <- force_tz(h$a, tzone = timezone_h)
  h$time_ita <- with_tz(h$a, tzone ="CET")
  h <- h %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(h)
  i <- as.data.frame(X[[9]])
  i$a <- i$time
  timezone_i <- i[1,7]
  i$a <- force_tz(i$a, tzone = timezone_i)
  i$time_ita <- with_tz(i$a, tzone ="CET")
  i <- i %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(i)
  j <- as.data.frame(X[[10]])
  j$a <- j$time
  timezone_j <- j[1,7]
  j$a <- force_tz(j$a, tzone = timezone_j)
  j$time_ita <- with_tz(j$a, tzone ="CET")
  j <- j %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(j)
  k <- as.data.frame(X[[11]])
  k$a <- k$time
  timezone_k <- k[1,7]
  k$a <- force_tz(k$a, tzone = timezone_k)
  k$time_ita <- with_tz(k$a, tzone ="CET")
  k <- k %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(k)
  l <- as.data.frame(X[[12]])
  l$a <- l$time
  timezone_l <- l[1,7]
  l$a <- force_tz(l$a, tzone = timezone_l)
  l$time_ita <- with_tz(l$a, tzone ="CET")
  l <- l %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(l)
  m <- as.data.frame(X[[13]])
  m$a <- m$time
  timezone_m <- m[1,7]
  m$a <- force_tz(m$a, tzone = timezone_m)
  m$time_ita <- with_tz(m$a, tzone ="CET")
  m <- m %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(m)
  n <- as.data.frame(X[[14]])
  n$a <- n$time
  timezone_n <- n[1,7]
  n$a <- force_tz(n$a, tzone = timezone_n)
  n$time_ita <- with_tz(n$a, tzone ="CET")
  n <- n %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(n)
  o <- as.data.frame(X[[15]])
  o$a <- o$time
  timezone_o <- o[1,7]
  o$a <- force_tz(o$a, tzone = timezone_o)
  o$time_ita <- with_tz(o$a, tzone ="CET")
  o <- o %>% select(longitude, latitude, time, time_ita, id)
  a <- a %>%
    rbind(o)
  df <- df %>%
    left_join(a)}}
