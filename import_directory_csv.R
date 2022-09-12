library(tidyverse)

# Old Fashion Way. One file at a time.

df1 <- read.csv("./bike_data/t01.csv")
df2 <- read.csv("./bike_data/t02.csv")
df3 <- read.csv("./bike_data/t03.csv")
df4 <- read.csv("./bike_data/t04.csv")
df5 <- read.csv("./bike_data/t05.csv")
df6 <- read.csv("./bike_data/t06.csv")
df7 <- read.csv("./bike_data/t07.csv")
df8 <- read.csv("./bike_data/t08.csv")
df9 <- read.csv("./bike_data/t09.csv")
df10 <- read.csv("./bike_data/t10.csv")
df11 <- read.csv("./bike_data/t11.csv")
df12 <- read.csv("./bike_data/t12.csv")

bike_rbind <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)
bike_rbind$started_at <- lubridate::ymd_hms(bike_rbind$started_at)
bike_rbind$ended_at <- lubridate::ymd_hms(bike_rbind$ended_at)
