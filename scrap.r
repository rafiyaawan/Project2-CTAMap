library(shiny)
library(ggplot2)
library(shinydashboard)
library(lubridate)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(dplyr)
library(plyr)
library(readr)
library(leaflet)

#read in the data for stations
temp = list.files(pattern="*.csv", full.name = T)
print(temp)
temp = temp[-148] #delete the last file to be read separately
#read in the files
ridership_data = ldply(temp, read_csv)
print(head(ridership_data))

#read in the list of stops and latitudes and longitudes
stopData <- read.csv(file = 'StopList.csv')
print(head(stopData))

#fix dates using lubridate
ridership_data$newDate = as_date(mdy(ridership_data$date))

#add day, month and year data to csv
ridership_data$year = year(ridership_data$newDate)
ridership_data$month = month(ridership_data$newDate, abbr = TRUE, label = TRUE)
ridership_data$wday = weekdays(as.POSIXct(ridership_data$newDate), abbreviate = T)

#changes rides from character to numeric
ridership_data$rides <- as.numeric(gsub(",","",ridership_data$rides))
print(head(ridership_data))

DateSub <- subset(ridership_data, newDate == "2021-08-23")
print(DateSub)
ggplot(DateSub, aes(x = stationname, y = rides)) +
  stat_summary(fun = sum, geom="bar", colour = "red", width = 0.5) + 
  theme_bw() +
  labs(x = "Station name", y ="Rides") + 
  theme(text = element_text(family = "sans", face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12), axis.text.x = element_text(angle = 90))

  w#geom_bar(stat = "identity", fill = "#91b3bb", width=0.8) +
  #labs(x = "Weekday", y ="Rides (in thousands)") + 
  #theme_bw() +
  #theme(text = element_text(family = "sans", face = "bold")) +
  #theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12))  

head(stopData)

m <- leaflet()
m <- addTiles(m)
station_ids = strsplit(temp, "./")
for (i in station_ids){
 i = strsplit(i[2], ".csv")
 a <- subset(stopData, MAP_ID == i)
 string <- a$Location[1]
 print(i)
 mat = matrix(scan(text = gsub("[()]", "", string), sep = ","), 
              ncol = 2, byrow = TRUE, dimnames = list(NULL, c("Lat", "Long")))
 m <- addMarkers(m, lng=mat[1,2], lat=mat[1,1], popup=a$STOP_NAME[1])
}
m

a <- subset(stopData, MAP_ID == i)
a$Location[1]

string <- "(41.88322, -87.626189), (41.79542, -87.631157)"

mat = matrix(scan(text = gsub("[()]", "", string), sep = ","), 
       ncol = 2, byrow = TRUE, dimnames = list(NULL, c("Lat", "Long")))
mat[1,]
