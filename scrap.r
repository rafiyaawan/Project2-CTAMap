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
library(leaflet.providers)

marker_radius <- function(num_rides) {
  radius = 0
    if (num_rides < 1000.0){
      radius = 4
    }
  else if (num_rides < 2000.0){
    radius = 6
  }
  else if (num_rides < 2500.0){
    radius = 8
  }
  else if(num_rides < 4000.0){
    radius = 10
  }
  else{
    radius = 12
  }
  return (radius)
}

#read in the data for stations
temp = list.files(pattern="*.csv", full.name = T)
print(temp)
temp = temp[-148] #delete the last file to be read separately
#read in the files
ridership_data = ldply(temp, read_csv)
print(head(ridership_data))

#read in the list of stops and latitudes and longitudes
stopData <- read.csv(file = 'StopList.csv')
print(stopData)

#fix dates using lubridate
ridership_data$newDate = as_date(mdy(ridership_data$date))

#add day, month and year data to csv
ridership_data$year = year(ridership_data$newDate)
ridership_data$month = month(ridership_data$newDate, abbr = TRUE, label = TRUE)
ridership_data$wday = weekdays(as.POSIXct(ridership_data$newDate), abbreviate = T)

#changes rides from character to numeric
ridership_data$rides <- as.numeric(gsub(",","",ridership_data$rides))
print(head(ridership_data))
print(stopData)
print(stopData$Location[stopData$MAP_ID==40050])

DateSub <- subset(ridership_data, newDate == "2021-08-23")
print(DateSub)
ggplot(DateSub, aes(x = reorder(stationname, rides), y = rides)) +
  stat_summary(fun = sum, geom="bar", colour = "red", width = 0.5) + 
  theme_bw() +
  labs(x = "Station name", y ="Rides") + 
  theme(text = element_text(family = "sans", face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12), axis.text.x = element_text(angle = 90))

  #geom_bar(stat = "identity", fill = "#91b3bb", width=0.8) +
  #labs(x = "Weekday", y ="Rides (in thousands)") + 
  #theme_bw() +
  #theme(text = element_text(family = "sans", face = "bold")) +
  #theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12))  

head(stopData)

str(providers_loaded()$providers$Esri.WorldStreetMap)

DateSub <- subset(ridership_data, newDate == "2021-08-23")
dateSubSums <- aggregate(DateSub$rides, by=list(station_id=DateSub$station_id), FUN=sum)
head(dateSubSums)
print(dateSubSums)
print(nrow(dateSubSums))
print(DateSub$x[DateSub$station_id==40200])
m <- leaflet()
m <- addTiles(m)
#m <- addProviderTiles(m, provider = "Esri.WorldImagery")
station_ids = strsplit(temp, "./")
print(station_ids)

for (i in 1:45){
  print(i)
}

for (i in 1:nrow(dateSubSums)){
  #i = strsplit(i[2], ".csv")
  print(typeof(dateSubSums[i, "x"]))
  a <- subset(stopData, MAP_ID == dateSubSums[i, "station_id"])
  string <- a$Location[1]
  mat = matrix(scan(text = gsub("[()]", "", string), sep = ","), 
               ncol = 2, byrow = TRUE, dimnames = list(NULL, c("Lat", "Long")))
  m <- addCircleMarkers(m, lng=mat[1,2], lat=mat[1,1], popup=a$STOP_NAME[1], radius = marker_radius(dateSubSums[i, "x"]))
}
m

a <- subset(stopData, MAP_ID == i)
a$Location[1]

string <- "(41.88322, -87.626189), (41.79542, -87.631157)"

mat = matrix(scan(text = gsub("[()]", "", string), sep = ","), 
             ncol = 2, byrow = TRUE, dimnames = list(NULL, c("Lat", "Long")))
mat[1,]


dateNew <-ymd("20110604")
date
month(date)
date+1
