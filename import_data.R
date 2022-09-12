library(tidyverse)
library(janitor)
library(lubridate)
library(readxl)
library(scales)
library(writexl) # Well, This is used to create Excel Sheets
rm(list=ls())
## Get Directory Listing
## R  Code Video:
##  https://youtu.be/HpWce0ovphY
##
## The Long Way ...
##
df1 <- read_csv("./bike_data/t01.csv")
df2 <- read_csv("./bike_data/t02.csv")
df3 <- read_csv("./bike_data/t03.csv")
df4 <- read_csv("./bike_data/t04.csv")
df5 <- read_csv("./bike_data/t05.csv")
df6 <- read_csv("./bike_data/t06.csv")
df7 <- read_csv("./bike_data/t07.csv")
df8 <- read_csv("./bike_data/t08.csv")
df9 <- read_csv("./bike_data/t09.csv")
df10 <- read_csv("./bike_data/t10.csv")
df11 <- read_csv("./bike_data/t11.csv")
df12 <- read_csv("./bike_data/t12.csv")
##
## Combine 12 data.frames into One (1) data.frame
##
bike_rides <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)
bike_rides <- janitor::remove_empty(bike_rides,which = c("cols"))
bike_rides <- janitor::remove_empty(bike_rides,which = c("rows"))
##
## Convert Data/Time stamp to Date/Time ...
##
bike_rides$start_date <- as.Date(bike_rides$started_at)
bike_rides$end_date <- as.Date(bike_rides$ended_at)

bike_rides$started_time <- lubridate::ymd_hms(bike_rides$started_at)
bike_rides$ended_time <- lubridate::ymd_hms(bike_rides$ended_at)

bike_rides$start_hour <- lubridate::hour(bike_rides$started_at)
bike_rides$end_hour <- lubridate::hour(bike_rides$ended_at)

### Calculate Trip Duration

bike_rides$Hours <- difftime(bike_rides$ended_at,bike_rides$started_at,units = c("hours"))

bike_rides$Minutes <- difftime(bike_rides$ended_at,bike_rides$started_at,units = c("mins"))

bikerides1 <- bike_rides %>% filter(Minutes >0) %>% drop_na()

bikerides1<- bikerides1 %>% separate(Minutes,c("Minutes","X"),sep =" ")
bikerides1$Minutes <- as.numeric(bikerides1$Minutes)
bikerides1$Minutes <- round(bikerides1$Minutes)
###
## Summary by Minutes

bikesrides2 <- bikerides1 %>% group_by(Ymd) %>%
  summarise(
    Mins = sum(Minutes),
    Mean = mean(Minutes),
    Median = median(Minutes),
    Max = max(Minutes),
    Min = min(Minutes),
    Count = n()
  ) %>% ungroup()

### Create summary data frame

bikesrides3 <- bikerides1 %>% group_by(Ymd,start_hour) %>%
                summarise(
                  Mins = sum(Minutes),
                  Mean = mean(Minutes),
                  Median = median(Minutes),
                  Max = max(Minutes),
                  Min = min(Minutes),
                  Count = n()
                ) %>% ungroup()

write_xlsx(bikesrides2,"./bikerides1.xlsx")
write_xlsx(bikesrides2,"./bikerides2.xlsx")


### Plot Mean Ride Duration by Day

bikesrides2 %>% ggplot(aes(x=Ymd,y=Mean)) +
  geom_line() + geom_smooth(aes(x=Ymd,y=Mean),method = "lm")

bikesrides2 %>% ggplot(aes(x=Ymd,y=Count)) +
  geom_line() + geom_smooth(aes(x=Ymd,y=Count),method = "gam")

## Plots by Hour

bikesrides3 %>% ggplot(aes(x=start_hour,y=Mean)) +
  geom_col() + geom_smooth(aes(x=start_hour,y=Mean),method = "gam")

bikesrides3 %>% ggplot(aes(x=start_hour,y=Count)) +
  geom_col() + geom_smooth(aes(x=start_hour,y=Count),method = "gam")

## Plots by Bike Type and Day 

bikesrides4 <- bikerides1 %>% group_by(Ymd,rideable_type) %>%
  summarise(
    Mins = sum(Minutes),
    Mean = mean(Minutes),
    Median = median(Minutes),
    Max = max(Minutes),
    Min = min(Minutes),
    Count = n()
  ) %>% ungroup()

bikesrides4 %>% ggplot(aes(x=Ymd,y=Count)) +
  geom_line() + facet_wrap(~rideable_type)