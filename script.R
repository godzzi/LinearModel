
rm(list=ls())
setwd("~")

library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)

setwd("/Users/mariazilli/Desktop/nyc-taxi-trip-duration")
test <- read.csv("test.csv", header=T)
train <- read.csv("train.csv", header=T)
weather <- read.csv("weather_data_nyc_centralpark_2016(1).csv", header=T)

summary(train)
summary(test)
summary(weather)

# Both datasets into one 

train1 <- train

#Separate date from time Pick up 
new <- do.call(rbind, strsplit(as.character(train1$pickup_datetime) , " " ))
new1<- do.call(rbind, strsplit(as.character(train1$dropoff_datetime) , " " ))

train1 <- cbind(train1, pu_date = new[,1] , pu_time = new[,2] )
train1 <- cbind(train1, do_date = new1[,1] , do_time = new1[,2] )

rm(new, new1)

# Add weather data from 2016 since taxi data is from 2016 by date 
# change date on weather to same format 

weather$date <- dmy(weather$date) 

# Change pu_date to date format in order to join data sets
train1$pu_date <- as.Date(train1$pu_date)


train1 <- left_join(train1, weather, by = "pu_date")

# Clean the dataset with only interset values 
names(train1)


train2 <- train1 %>% select(-date, -pickup_longitude, 
                                  -pickup_latitude, -store_and_fwd_flag, -dropoff_longitude,
                                 -dropoff_latitude, -pickup_datetime, -dropoff_datetime)




#data exploration 
summary(train2$trip_duration)

# Would changing seconds to minutes help? 
train2$minutes <- as.numeric(round((train2$trip_duration/60), 3))


#save dataset clean to share ¿?

write.csv(train2, "train2.csv")


# Trip duration 
# A log10 scale is necessary since the change in the variable trip duration is too small 
ggplot(train2, aes(trip_duration)) + 
    geom_histogram(fill="dark blue", bins=200) + scale_x_log10() 
    + xlab("Trip Duration")

ggplot(train2, aes(trip_duration)) + 
  geom_boxplot() + scale_x_log10() 

ggplot(train2, aes(average.temperature)) + 
  geom_histogram(fill="light blue", bins=200) + scale_x_log10() 
  + xlab("Average Temperature")


ggplot(train2, aes(minutes)) + 
  geom_histogram(fill= "dark blue" ,bins=200) + scale_x_log10()


ggplot(train2, aes(minutes)) + geom_density() + scale_x_log10()

ggplot(train2, aes(trip_duration)) + geom_density(color="dark blue")+
  scale_x_log10() + xlab("Trip Duration")

ggplot(train2, aes(pu_date)) +
  geom_histogram(fill = "dark blue", bins = 200) + xlab("Date")

ggplot(train2, aes(passenger_count)) +
  geom_histogram(fill = "dark blue", bins = 200) + xlab("Passenger Count")

ggplot(train2, aes(passenger_count)) +
  geom_boxplot(fill = "dark blue", bins = 200) + xlab("Passenger Count")

ggplot(train2, aes(trip_duration)) + geom_freqpoly() +scale_x_log10()

# Weather 
ggplot(train2, aes(average.temperature)) + 
  geom_histogram()

# Four ouliers for trip duration 
ggplot(train2, aes(x=average.temperature, y=trip_duration)) + geom_point() + scale_y_log10()


p <- train2 %>%
  ggplot(aes(x=pu_date, y=trip_duration)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  scale_y_log10()+
  ylab("Trip Duration") +
  xlab("Date of trip")

p <- ggplotly(p)


# works but takes suepr long time 


library(dygraphs)


don <- xts(x = train2$trip_duration, order.by = train2$pu_date)

# Make the chart
p <- dygraph(don)
p

#Interactive graph aslo takes long time 

# Categorize weather 



