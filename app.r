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

#radius of markers function
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
                column(1,
                       fluidRow(
                         style = "padding-left:20px",
                         HTML("<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>"),
                         dateInput("dataDate",
                                   h3("Enter Date"),
                                   value = "2021-08-23",
                                   min = "2001-01-01",
                                   max = "2021-11-30"
                                   ),
                         actionButton("prevButton", "Previous"),
                         actionButton("nextButton", "Next"),
                         radioButtons("sortData", h3("Sort Bars"),
                                      choices = list("Alphabetically" = 1, 
                                                     "Ascending" = 2),selected = 1)
                       ),
                ),
                column(11,
                       fluidRow(
                         #box(title = "Entries at L Stations on August 23, 2021", solidHeader = TRUE, status = "primary", width = 12,
                         box(title = textOutput("barChart"), solidHeader = TRUE, status = "primary", width = 12,
                             plotOutput("initialChart", height = 600)
                         )
                       )
                )
              ),
              fluidRow(
                fluidRow(
                  style = "padding-left:20px",
                  column(12, 
                         radioButtons("mapView", h3("Radio buttons"),
                               choices = list("Street View" = 1, "Terrain View" = 2,
                                              "Transport" = 3),selected = 1, inline = TRUE)
                  )
                ),
                fluidRow(
                  column(12,
                         fluidRow(
                           #box(title = "Map of L Stations on August 23, 2021", solidHeader = TRUE, status = "primary", width = 12,
                           box(title = textOutput("map"), solidHeader = TRUE, status = "primary", width = 12,
                               leafletOutput("leaflet", height = 600)
                               )
                           )
                         )
                  ) #fluidrow
                ) #fluidrow
              
              
      ) # tabitem Visualizations close
      
    ) #tabItems
    
  ) #dashboardBody
  
) #dashboardPage



#server functions
server <- function(input, output, session) {
  
  dateBarChart <- reactive({
    as_date(input$dataDate)
  })
  
  dataSort <- reactive({
    input$sortData
  })
  
  #dateBarChart <- reactiveValues(as_date(input$dataDate))
  
  #Move forward a day
  observeEvent(input$nextButton, {
    nextDate <- input$dataDate + 1
    updateDateInput(session, "dataDate",
                    label = "Enter Date",
                    value = nextDate,
                    min = "2001-01-01",
                    max = "2021-11-30"
    )
  })
  
  #Move backward a day
  observeEvent(input$prevButton, {
    nextDate <- input$dataDate - 1
    updateDateInput(session, "dataDate",
                    label = "Enter Date",
                    value = nextDate,
                    min = "2001-01-01",
                    max = "2021-11-30"
    )
  })
  
  
  #render text
  output$barChart <- renderText({
    return(paste("Entries at L Stations on ", dateBarChart()))
  })
  
  output$map <- renderText({
    return(paste("Map of L Stations on ", dateBarChart()))
  })
  
  #Total entries at all L stations for Date
  output$initialChart <- renderPlot({
    # Add up entries for each Station on Date
    ridershipAug23 <- subset(ridership_data, ridership_data$newDate == dateBarChart())
    byStation <- setNames(aggregate(ridershipAug23$rides, by=list(ridershipAug23$stationname), sum), c("Station", "Entries"))

    if(dataSort() == 1){
      m <- ggplot(byStation, aes(x=Station, y=Entries)) 
    }
    else{
      m <- ggplot(byStation, aes(x=reorder(Station, Entries), y=Entries)) 
    }
    
    m <- m + geom_bar(stat="identity", width=0.7, fill="#33647A") +
      labs(x=paste("Station Name"), y="Total Entries") +
      theme_bw() +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(axis.text.x = element_text(angle = 70, hjust=1))
    m
    
  })
  
  #bad code - can't find how to add a list of markers
  output$leaflet <- renderLeaflet({
    marker_color = "blue"
    m <- leaflet()
    m <- addTiles(m)
    if(input$mapView == 2){
      m <- addProviderTiles(m, provider = "Esri.WorldImagery")
      marker_color = "red"
    }
    else if(input$mapView == 3){
      m <- addProviderTiles(m, provider = "OpenTopoMap")
    }
      
    #TODO add the lat and long for the last four stations as (lat, long)
    #add legend for sizing
    DateSub <- subset(ridership_data, newDate == dateBarChart())
    DateSubSums <- aggregate(DateSub$rides, by=list(station_id=DateSub$station_id), FUN=sum)
    #Pick 3 backgrounds from http://leaflet-extras.github.io/leaflet-providers/preview/
    #m <- addProviderTiles(m, provider = "Esri.WorldImagery") #Thunderforest.Transport
    station_ids = strsplit(temp, "./")
    for (i in 1:nrow(DateSubSums)){
      a <- subset(stopData, MAP_ID == DateSubSums[i, "station_id"])
      string <- a$Location[1]
      mat = matrix(scan(text = gsub("[()]", "", string), sep = ","), 
                   ncol = 2, byrow = TRUE, dimnames = list(NULL, c("Lat", "Long")))
      #m <- addMarkers(m, lng=mat[1,2], lat=mat[1,1], popup=a$STOP_NAME[1])
      m <- addCircleMarkers(m, lng=mat[1,2], lat=mat[1,1], #popup=c(a$STOP_NAME[1], DateSubSums[i, "x"]),
                            popup = paste(sep="",
                                          "<b>", a$STOP_NAME[1],"</b>","<br/>",
                                          "<b>", DateSubSums[i, "x"], "</b>"
                            ), 
                            radius = marker_radius(DateSubSums[i, "x"]), color = marker_color)
    }
    m
  })
  
}

shinyApp(ui = ui, server = server)



