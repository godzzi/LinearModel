rm(list=ls())
setwd("~")

library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)
library(weathermetrics)

setwd("/Users/mariazilli/Desktop/nyc-taxi-trip-duration")

data <- read.csv("train2.csv", header=T)
weather <- read.csv("weather_data_nyc_centralpark_2016(1).csv", header=T)

#weather data 
weather$date <- dmy(weather$date) 
weather1 <- weather %>% filter(date <= "2016-06-30")

weather1$precipitation <- as.numeric(weather1$precipitation)
weather1$precipitation[is.na(weather1$precipitation)] <- 0

weather1$snow.fall <- as.numeric(weather1$snow.fall)
weather1$snow.fall[is.na(weather1$snow.fall)] <- 0

weather1$snow.depth <- as.numeric(weather1$snow.depth)
weather1$snow.depth[is.na(weather1$snow.depth)] <- 0

data1 <- data %>% select("trip_duration", "passenger_count", "pu_date", 
                        "pu_time", "do_date", "do_time", "average.temperature", 
                        "precipitation", "snow.fall")

data1$average.temperature <- as.numeric(data1$average.temperature)

data1$trip_duration <- as.numeric(data1$trip_duration)

data1 <- subset(data1,trip_duration < 86400 & trip_duration >0 )

data1$minutes <- round(data1$trip_duration/60,3)

data1$average.temperature <- fahrenheit.to.celsius(data1$average.temperature, round=2)

weather1$average.temperature <- fahrenheit.to.celsius(weather1$average.temperature, round=2)

write.csv(data1, "nyc_weather.csv")

# Graphs for data exploration 

f1 <- ggplot(data1, aes(trip_duration)) + geom_histogram(fill="dark blue", bins=200)+
  scale_x_log10() + xlab("Trip Duration (seconds)") + ggtitle("Figure 1: Trip Duration Histogram")+
  geom_vline(aes(xintercept=mean(trip_duration)), color="red", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=median(trip_duration)), color="light blue", linetype="dashed", size=1)

f2 <- ggplot(data1, aes(average.temperature)) + geom_histogram(fill="mediumvioletred",bins=200)+
  xlab("Average Temperature (Cº) ") + ggtitle("Figure 2: Average Temperature Histogram")

f3 <- ggplot(weather1, aes(x=average.temperature, y=date)) + geom_line(color="light blue") + 
      scale_y_date() +geom_smooth(method = "loess") + 
      xlab("Average Temperature (Cº) ")+ 
      ylab("Date") +
      ggtitle("Figure 3: Average Temperature")
f3

f4 <- ggplot(data1, aes(x=average.temperature, y=trip_duration, colour=passenger_count)) + geom_point(col="dark blue") +
  xlab("Average Temperature  (ºC)") + ylab("Trip Duration (seconds)") +
  ggtitle("Figure 3: Average Temperature vs. Trip Duration") 

f5 <- ggplot(data1, aes(passenger_count, fill=passenger_count)) + geom_bar()+
  xlab("Passenger Count") + ggtitle("Figure 5: Passenger Count Frequency")+ 
  scale_y_log10()

# Check for passenger counts
data1[data1$passenger_count==9, ] # 1
data1[data1$passenger_count==8, ] # 1
data1[data1$passenger_count==7, ] # 3 
data1[data1$passenger_count==0, ] # 60

#Rain 
data1$precipitation <- as.numeric(data$precipitation)
data1$precipitation[is.na(data1$precipitation)] <- 0
summary(data1$precipitation)

f6 <- ggplot(weather1, aes(x=date, y=precipitation)) + geom_area(fill="light blue")+
  xlab("Date") + ylab("Precipitation(inches)") + ggtitle("Figure 6: Rain level per day")
f6

# Snow 
data1$snow.fall <- as.numeric(data1$snow.fall)
data1$snow.fall[is.na(data1$snow.fall)] <- 0
summary(data1$snow.fall)

f7 <- ggplot(weather1, aes(x=date)) + 
  geom_line( aes(y=snow.fall, color="Snow Fall")) +
  geom_line( aes(y=snow.depth, color="Snow Depth")) +
  xlab("Date") + ylab("Snow Fall (inches)") + ggtitle("Figure 7: Snow fall per day")+
  scale_color_manual(name = "Legend",
                     breaks = c("Snow Fall", "Snow Depth"),
                     values = c("Snow Fall" = "royalblue4", "Snow Depth" = "paleturquoise") )

f7

# Change the format from long to wide in order to see number of trips per day and else 
numberoftrips <- subset(data1[, 1:3])
numberoftrips <- numberoftrips[,-2]

numberoftrips %>% group_by(pu_date)
numberoftrips <- summarize(numberoftrips, trips_per_date = mean(trip_duration)) 
# mean trip durartion per day  not trips per day 
# change variable name 

numberoftrips1 <- subset(data1[, 1:3])
numberoftrips1 <- numberoftrips1[,-2]

numberoftrips1 %>% group_by(pu_date)
numberoftrips1 <- summarize(numberoftrips1, trips_per_date = count(trip_duration))

#Check if this is correct 

firstofjanuary <- data1[data1$pu_date=="2016-01-01", ]
mean(firstofjanuary$trip_duration) # 920.68 

rm(firstofjanuary)

numberoftrips$trips_per_date<- round(numberoftrips$trips_per_date, 2)
numberoftrips$trips_per_date<- as.numeric(numberoftrips$trips_per_date)

  
f8 <- ggplot(numberoftrips, aes(x=pu_date, y=trips_per_date)) +
  geom_line(color="dark green") + xlab("Pick-up Date") +
  ylab("Mean Trip Duration by Date") + scale_x_date(date_labels = "%b") + 
  ggtitle("Figure 8: Mean Trip Duration per Day")
f8

# Number of trips per day 
tripsperday <- data.frame(table(data1$pu_date))
tripsperday$Var1<- as.Date(tripsperday$Var1)
tripsperday$date <- tripsperday$Var1
tripsperday$date <- as.character(tripsperday$date)
numberoftrips$date <- numberoftrips$pu_date
numberoftrips$date <- as.character(numberoftrips$date)


f9 <- ggplot(tripsperday, aes(x=Var1, y=Freq)) +
  geom_line(color="mediumorchid4") + xlab("Date ") +
  ylab("Number of trips per day")  + scale_x_date()+
  ggtitle("Figure 9: Number of Trips per Day ")

f9

tripsperday1 <- tripsperday
tripsperday1 <- tripsperday1[tripsperday1$Freq>6000,]

tripsperday1$date <- as.Date(tripsperday1$date)

f10 <- ggplot(tripsperday1, aes(x=date, y=Freq)) +
  geom_line(color="mediumorchid4") + xlab("Date ") +
  ylab("Number of trips per day")  + scale_x_date()+
  ggtitle("Figure 10: Number of Trips per Day ")

f10

# tripsperdayweighted <- left_join(tripsperday, numberoftrips, by="date")
# tripsperdayweighted <- tripsperdayweighted  %>% select("date","Freq","trips_per_date")

# tripsperdayweighted$weightedtripsperday <- weighted.mean(tripsperdayweighted$trips_per_date,
                                                          #tripsperdayweighted$Freq)

# didnt work dont run 

# Days of the week 

data1$dayofweek <- as.Date(data1$pu_date)
data1$dayofweek <- wday(data1$dayofweek, label=TRUE)

daysofweek <- data1 %>% select("dayofweek")

daysofweek <- daysofweek %>% 
              group_by(dayofweek) %>%
              count()

daysofweek$dayofweek <- ordered(daysofweek$dayofweek,
                                levels=c("Sun","Sat","Fri", "Thu", "Wed","Tue", 
                                         "Mon"))


f11 <- ggplot(daysofweek, aes(x=dayofweek, y=n, color=dayofweek)) + geom_point() +
      coord_flip()  + geom_bar(stat="identity",width=.01)+
      xlab("Number of trips") + ylab("Day of the week") + 
      ggtitle("Figure 11: Number of trips per day of the week") + 
      scale_color_brewer(palette="Dark2") +
      theme(legend.position = "none")
      
f11

f12 <- ggplot(data1, aes(x=average.temperature, y=trip_duration)) + geom_point(col="dark blue") +
  xlab("Average Temperature  (ºC)") + ylab("Trip Duration (seconds)") +
  ggtitle("Figure 12: Average Temperature vs. Trip Duration") 

f12

tripsperday <- tripsperday[,2:3]


weather2 <- select(weather1, "date", "average.temperature", "precipitation", "snow.fall", "snow.depth")
weather2$date <- as.character(weather2$date)

tripsperday <- left_join(tripsperday, weather2, by="date")

write.csv(tripsperday, "FreqTripsPerDay.csv")


