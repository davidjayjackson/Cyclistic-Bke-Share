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
df01 <- read_csv("./bike_data/t01.csv") %>% select(-start_station_id,-end_station_id)
df02 <- read_csv("./bike_data/t02.csv") %>% select(-start_station_id, -end_station_id)
# df03 <- read.csv("./bike_data/t03.csv")
df04 <- read_csv("./bike_data/t04.csv") %>% select(-start_station_id,-end_station_id)
df05 <- read_csv("./bike_data/t05.csv") %>% select(-start_station_id,-end_station_id)
df06 <- read_csv("./bike_data/t06.csv") %>% select(-start_station_id, -end_station_id)
df07 <- read_csv("./bike_data/t07.csv") %>% select(-start_station_id, -end_station_id)
df08 <- read_csv("./bike_data/t08.csv") %>% select(-start_station_id, -end_station_id)
df09 <- read_csv("./bike_data/t09.csv") %>% select(-start_station_id, -end_station_id)
df10 <- read_csv("./bike_data/t10.csv") %>% select(-start_station_id, -end_station_id)
df11 <- read_csv("./bike_data/t11.csv") %>% select(-start_station_id, -end_station_id)
df12 <- read_csv("./bike_data/t12.csv") %>% select(-start_station_id, -end_station_id)
#

##
bike_rides <- bind_rows(df01,df02,df04,df05,df06,df07,df08,df09,df10,df11,df12)
bike_rides <- bike_rides %>%   filter(!is.na(start_station_name))
bike_rides <- janitor::remove_empty(bike_rides,which = c("cols","rows"))

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

bikesrides2 <- bikerides1 %>% group_by(start_date) %>%
  summarise(
    Mins = sum(Minutes),
    Mean = mean(Minutes),
    Median = median(Minutes),
    Max = max(Minutes),
    Min = min(Minutes),
    Count = n()
  ) %>% ungroup()

### Create summary data frame

bikesrides3 <- bikerides1 %>% group_by(start_date,start_hour) %>%
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

bikesrides4 <- bikerides1 %>% group_by(start_date,rideable_type) %>%
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


 
ggplot() + geom_line(aes(x=date,y=count)) +
  scale_y_continuous(labels = comma)