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

#For year input
years <- c(2001:2021)

# Create the shiny dashboard
ui <- dashboardPage(
  #change header color
  skin = "black",
  
  dashboardHeader(title = "CTA Ridership Map"),
  dashboardSidebar(disable = FALSE, collapsed = TRUE,
                   
                   sidebarMenu(
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("About", tabName = "About", icon = NULL),
                     menuItem("Data Visualizations", tabName = "Datavisualizations", icon = NULL, selected = TRUE))
                   
  ),
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "About",
              h1("CTA Ridership Map Project"),
              h2("Ridership Data From: Chicago Data Portal at https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f"),
              h2("Stations Location Data From: Chicago Data Portal at https://data.cityofchicago.org/Transportation/CTA-System-Information-List-of-L-Stops/8pix-ypme"),
              h2("Application Written by Ameesha Saxena and Rafiya Awan for UIC CS 424 Spring 2022")
      ), # tabitem About close
      tabItem(tabName = "Datavisualizations",
              tags$style(HTML("
              .box.box-solid.box-primary>.box-header {
              color:#fff;
              background:#003d59
              }
              .box.box-solid.box-primary{
              border-bottom-color:#666666;
              border-left-color:#666666;
              border-right-color:#666666;
              border-top-color:#666666;
              }
              /* body */
              .content-wrapper, .right-side {
              background-color: #f5f5f5;
              }
                              ")
                         ),
              fluidRow(
                column(12,
                       fluidRow(
                         box(title = "Entries at L Stations on August 23, 2021", solidHeader = TRUE, status = "primary", width = 12,
                             plotOutput("initialChart", height = 600)
                         )
                       )
                )
              ),
              fluidRow(
                column(12,
                       fluidRow(
                         box(title = "Map of L Stations on August 23, 2021", solidHeader = TRUE, status = "primary", width = 12,
                             leafletOutput("leaflet", height = 600)
                         )
                       )
                )
              ) #fluidrow
              
              
      ) # tabitem Visualizations close
      
    ) #tabItems
    
  ) #dashboardBody
  
) #dashboardPage



#server functions
server <- function(input, output) {
  
  #Total entries at all L stations for Aug 23, 2021
  output$initialChart <- renderPlot({
    
    
    # Add up entries for each Station on Aug 23, 2021
    ridershipAug23 <- subset(ridership_data, ridership_data$newDate == "2021-08-23")
    byStation <- setNames(aggregate(ridershipAug23$rides, by=list(ridershipAug23$stationname), sum), c("Station", "Entries"))

    ggplot(byStation, aes(x=Station, y=Entries)) +
      geom_bar(stat="identity", width=0.7, fill="steelblue") +
      labs(x=paste("Station Name"), y="Total Entries") +
      theme_bw() +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
  })
  
  
  output$leaflet <- renderLeaflet({
    m <- leaflet()
    m <- addTiles(m)
    station_ids = strsplit(temp, "./")
    for (i in station_ids){
      i = strsplit(i[2], ".csv")
      print(i)
      a <- subset(stopData, MAP_ID == i)
      string <- a$Location[1]
      print(i)
      mat = matrix(scan(text = gsub("[()]", "", string), sep = ","), 
                   ncol = 2, byrow = TRUE, dimnames = list(NULL, c("Lat", "Long")))
      m <- addMarkers(m, lng=mat[1,2], lat=mat[1,1], popup=a$STOP_NAME[1])
    }
    m
    
  })
  
}

shinyApp(ui = ui, server = server)



