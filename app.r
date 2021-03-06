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
library(viridis)

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
ridership_data$wday = wday(ridership_data$newDate, label=TRUE)

#changes rides from character to numeric
ridership_data$rides <- as.numeric(gsub(",","",ridership_data$rides))
print(head(ridership_data))

#list of stations
station_name <- ridership_data[,3]

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
                column(1,
                       align = "center",
                       fluidRow(
                         style = "padding-left:20px",
                         HTML("<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>"),
                         h3("View station rides"),
                         dateInput("dataDate",
                                   h4("Select a date"),
                                   value = "2021-08-23",
                                   min = "2001-01-01",
                                   max = "2021-11-30"
                                   ),
                         actionButton("prevButton", "Previous date"),
                         actionButton("nextButton", "Next date"),
                         radioButtons("sortData", h4("Sort Bars"),
                                      choices = list("Alphabetically" = 0, 
                                                     "Ascending" = 1),selected = 0),
                         checkboxInput("difference", "View Difference in rides for two dates", value = FALSE),
                         dateInput("dataDate2",
                                   h4("Select second date to view difference in rides"),
                                   value = "2021-08-24",
                                   min = "2001-01-01",
                                   max = "2021-11-30"
                         ),
                         HTML("<br><br>"),
                         #selectInput("stationname", "Select a Station", station_name)
                         h3("Select a station and year to view visualizations"),
                         selectizeInput('stationname', label = NULL, choices = NULL, options = list(placeholder = 'Select a Station Name')),
                         selectInput("year", NULL, years, selected = "2021"),
                         radioButtons("plotOrTable", h3("Plots or Tables?"),
                                      choices = list("Plots" = 0, 
                                                     "Tables" = 1),selected = 0, inline="T")
                       ),
                       ),

                column(8,
                       fluidRow(
                         #box(title = "Entries at L Stations on August 23, 2021", solidHeader = TRUE, status = "primary", width = 12,
                         box(title = textOutput("barChart"), solidHeader = TRUE, status = "primary", width = 12,
                             plotOutput("initialChart", height = 700)
                         )
                       ),
                       fluidRow(
                         column(3,
                                fluidRow(
                                  box(title = textOutput("entriesTable"), solidHeader = TRUE, status = "primary", width = 12,
                                      #TableStationEntries
                                      div(DT::dataTableOutput("TableStationEntries"), style = "font-size:100%")
                                  )
                                )
                                ),
                         conditionalPanel(
                           condition = "input.plotOrTable == 0",
                           column(9,
                            fluidRow(
                               column(4,
                                      fluidRow(
                                        style = "padding-left:20px",
                                        box(title = paste("Rides per Day"), solidHeader = TRUE, status = "primary", width = 40,
                                            plotOutput("AllDays", height = 300)
                                        )
                                      )
                                      ),
                               column(4,
                                      fluidRow(
                                        style = "padding-left:20px",
                                        box(title = paste("Rides per Month"), solidHeader = TRUE, status = "primary", width = 40,
                                            plotOutput("MonthlyData", height = 300)
                                        )
                                      )
                                      ),
                               column(4,
                                      fluidRow(
                                        style = "padding-left:20px",
                                        box(title = paste("Rides per Weekday"), solidHeader = TRUE, status = "primary", width = 40,
                                            plotOutput("WeeklyData", height = 300)
                                        )
                                      )
                                      )
                            ), 
                            fluidRow(
                              style = "padding-left:20px",
                              box(title = paste("Rides per Year"), solidHeader = TRUE, status = "primary", width = 40,
                                  plotOutput("AllYears", height = 300)
                              )
                            )
                           )
                         ), #end of first, plots, conditionalPanel
                         conditionalPanel(
                           condition = "input.plotOrTable == 1",
                           column(3,
                                  fluidRow(
                                    box(title = paste("Rides per Day"), solidHeader = TRUE, status = "primary", width = 12,
                                        div(DT::dataTableOutput("TableAllDays"), style = "font-size:100%")
                                    )
                                  )
                           ),
                           column(3,
                                  fluidRow(
                                    box(title = paste("Rides per Month"), solidHeader = TRUE, status = "primary", width = 12,
                                        div(DT::dataTableOutput("TableMonthlyData"), style = "font-size:100%")
                                    )
                                  )
                           ),
                           column(3,
                                  fluidRow(
                                    box(title = paste("Rides per Weekday"), solidHeader = TRUE, status = "primary", width = 12,
                                        div(DT::dataTableOutput("TableWeeklyData"), style = "font-size:100%")
                                    )
                                  )
                           )
                         )
                       )
                ),
                column(3,
                       align="center",
                       fluidRow(
                         style = "padding-left:20px",
                         #box(title = "Map of L Stations on August 23, 2021", solidHeader = TRUE, status = "primary", width = 12,
                         box(title = textOutput("map"), solidHeader = TRUE, status = "primary", width = 12,
                             leafletOutput("leaflet", height = 1400)
                         )
                       ),
                       fluidRow(
                         style = "padding-left:20px",
                         radioButtons("mapView", ("Select a map background"),
                                      choices = list("Street View" = 1, "Satellite View" = 2,
                                                     "GrayScale" = 3),selected = 1, inline = TRUE)
                       )
                )
              )
              
              
      ) # tabitem Visualizations close
      
    ) #tabItems
    
  ) #dashboardBody
  
) #dashboardPage



#server functions
server <- function(input, output, session) {
  
  
  #date for the initial bar chart
  dateBarChart <- reactive({
    as_date(input$dataDate)
  })
  
  #date for the difference in rides in the bar chart
  dateBarChart2 <- reactive({
    as_date(input$dataDate2)
  })
  
  #to sort or not to sort
  dataSort <- reactive({
    input$sortData
  })
  
  #year for project 1 graphs
  inputYear <- reactive({
    as.numeric(input$year)
  })
  
  #select station for project 1 graphs, convert to subset of dataframe
  stationData <- reactive({
    station_data <- subset(ridership_data, ridership_data$stationname == input$stationname)
  })
  
  #to show the difference in rides for two dates
  dateDifference<- reactive({
    input$difference
  })
  
  #map backgrounds
  mapView <- reactive({
    input$mapView
  })
  
  #stationName selection
  updateSelectizeInput(session, 'stationname', choices = station_name, server = TRUE, selected="UIC-Halsted")
  
  
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
  
  #register marker click and extract station name
  observeEvent(input$leaflet_marker_click, {
    
    click <- input$leaflet_marker_click
    
    if (is.null(click))
      return()
    
    # get latitude and longitude of click
    lat <- substr(click$lat, start=1, stop=7)
    long <- substr(click$lng, start=1, stop=8)
    
    stopsAndLocations <- stopData[ , c("MAP_ID", "Location")]
    head(stopsAndLocations)
    
    # match latitude and longitude with data to get corresponding station ID
    stopsAndLocations$Latitude <- as.numeric(substr(stopsAndLocations$Location, start=2, stop=8))
    stopsAndLocations$Longitude <- sub(".*, ", "", stopsAndLocations$Location)
    stopsAndLocations$Longitude <- as.numeric(substr(stopsAndLocations$Longitude, start=1, stop=8))
    
    # get station name from id
    selectedStationId <- subset(stopsAndLocations, stopsAndLocations$Latitude == lat & stopsAndLocations$Longitude == long, select=1)
    selectedStation <- ridership_data[ridership_data$station_id == selectedStationId[1,], ]
    print(selectedStation[1,]$stationname)
    
    updateSelectizeInput(session, 'stationname', choices = station_name, server = TRUE, selected=selectedStation[1,]$stationname)
    
  })
  
  #render text for box titles
  
  output$barChart <- renderText({
    date <- dateBarChart()
    return(paste("Entries at L Stations on ", date, " - ", weekdays(date)))
  })
  
  output$map <- renderText({
    date <- dateBarChart()
    return(paste("Map of L Stations on ", date, " - ", weekdays(date)))
  })
  
  output$entriesTable <- renderText({
    date <- dateBarChart()
    return(paste("Stations and Entries on ", date, " - ", weekdays(date)))
  })
  
  
  #Total entries at all L stations for Date
  output$initialChart <- renderPlot({
    # Add up entries for each Station on Date
    ridershipDate <- subset(ridership_data, ridership_data$newDate == dateBarChart())
    ridershipDate2 <- subset(ridership_data, ridership_data$newDate == dateBarChart2())
    
    if(dateDifference()){
      ridershipDate$rides <- ridershipDate$rides - ridershipDate2$rides
    }
    
    byStation <- setNames(aggregate(ridershipDate$rides, by=list(ridershipDate$stationname), sum), c("Station", "Entries"))
    
    if(dataSort() == 0){
      m <- ggplot(byStation, aes(x=Station, y=Entries)) +
        geom_bar(stat="identity", width=0.7, fill="#33647A")
    }
    else{
      m <- ggplot(byStation, aes(x=reorder(Station, Entries), y=Entries, fill = Entries)) +
        geom_bar(stat="identity", width=0.7) +
        scale_fill_gradient2(midpoint = 0, mid = '#465767', low = '#7e2f2f', high = '#33647a') 
    }
    
    #m <- m + geom_bar(stat="identity", width=0.7, fill="#33647A") +
      #scale_fill_manual(values = c("(-Inf, 0]" = "#601e1e", "[1, Inf)" = "#153e51")) +
      #scale_fill_gradient2(midpoint = 0, low = '#082b3a', high = '#490f0f') +
      m <- m + theme_bw() +
      labs(x=paste("Station Name"), y="Total Entries") +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(axis.text.x = element_text(angle = 70, hjust=1))
      
    m
  })
  
  
  #output leaflet map with markers based on input values
  output$leaflet <- renderLeaflet({
    marker_color = "#33647A"
    m <- leaflet()
    m <- addTiles(m)
    
    #add backgrounds
    if(mapView() == 1){
      m <- addProviderTiles(m, provider = "Wikimedia")
    }
    else if(mapView() == 2){
      m <- addProviderTiles(m, provider = "Esri.WorldImagery")
    }
    else if(mapView() == 3){
      m <- addProviderTiles(m, provider = "CartoDB.Positron")
    }
    
    #subset for both the dates
    DateSub <- subset(ridership_data, newDate == dateBarChart())
    DateSub2 <- subset(ridership_data,  newDate == dateBarChart2())
    
    #calculate difference
    if(dateDifference()){
      DateSub$rides <- (DateSub$rides - DateSub2$rides) * 10
    }
    
    DateSubSums <- aggregate(DateSub$rides, by=list(station_id=DateSub$station_id), FUN=sum)

    #backgrounds from http://leaflet-extras.github.io/leaflet-providers/preview/
    station_ids = strsplit(temp, "./")
    
    for (i in 1:nrow(DateSubSums)){
      marker_color = "#33647A"
      a <- subset(stopData, MAP_ID == DateSubSums[i, "station_id"])
      string <- a$Location[1]
      mat = matrix(scan(text = gsub("[()]", "", string), sep = ","), 
                   ncol = 2, byrow = TRUE, dimnames = list(NULL, c("Lat", "Long")))
      #m <- addMarkers(m, lng=mat[1,2], lat=mat[1,1], popup=a$STOP_NAME[1])
      if(DateSubSums[i, "x"] < 0){
        marker_color = "#631a1a"
      }
      m <- addCircleMarkers(m, lng=mat[1,2], lat=mat[1,1], #popup=c(a$STOP_NAME[1], DateSubSums[i, "x"]),
                            popup = paste(sep="",
                                          "<b>", a$STOP_NAME[1],"</b>","<br/>",
                                          "<b>", DateSubSums[i, "x"], "</b>"
                            ), 
                            radius = sqrt(abs(DateSubSums[i, "x"]))/2, color = marker_color)
    }
    
    m <- addLegend(m,
                   "bottomright", 
                   colors = paste0(c("#33647A", "#631a1a", "#33647A"), "; border-radius:", c(40, 40, 40), "px; width:", c(20, 20, 20), "px; height:", c(20, 20, 20), "px"),
                   labels= paste0("<div style='display: inline-block;height: ", c(20, 20, 20), "px;margin-top: 4px;line-height: ", c(20, 20, 20), "px;'>", c("difference > 0", "difference < 0", "size proportional to #rides"), "</div>"),
                   title= "Entries",
                   opacity = 1)
    m
  })
  
  #output table for the intitial graph
  output$TableStationEntries <- DT::renderDataTable(
    DT::datatable({ 
      #YearSub <- subset(dataLeft(), year == inputYearLeft())
      ridershipDate <- subset(ridership_data, ridership_data$newDate == dateBarChart())
      ridershipDate2 <- subset(ridership_data, ridership_data$newDate == dateBarChart2())
      
      if(dateDifference()){
        ridershipDate$rides <- ridershipDate$rides - ridershipDate2$rides
      }
      
      byStation <- setNames(aggregate(ridershipDate$rides, by=list(ridershipDate$stationname), sum), c("Station", "Entries"))
      byStation
      #if(nrow(YearSub)){
       # ReturnData <- as.data.frame(aggregate(YearSub$rides, by=list(month=YearSub$month), FUN=sum))
      #}
    }, 
    options = list(searching = FALSE, pageLength = 14, lengthChange = FALSE, order = list(list(dataSort(), 'asc'))
    ), rownames = FALSE 
    )
  )
  
  # Per-Station Data Plots
  
  output$AllDays <- renderPlot({
    #inputYear = as.numeric(input$year)
    col <- c("#3e6a7f", "#749aa6", "#3e6a7f", "#749aa6", "#3e6a7f", "#749aa6", "#3e6a7f", "#749aa6", "#3e6a7f", "#749aa6", "#3e6a7f", "#749aa6")
    YearSub <- subset(stationData(), year == inputYear())
    YearSubSums <- setNames(aggregate(YearSub$rides, by=list(YearSub$newDate), FUN=sum), c("date", "rides"))
    ggplot(YearSubSums, aes(x = date, y = rides, fill = month(date,abbr = TRUE, label = TRUE))) + 
      geom_bar(stat = "identity", show.legend = FALSE) +
      labs(x = "Date", y ="Rides") + 
      theme_bw() +
      #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12)) +
      scale_fill_manual(values = col)
  })
  
  output$MonthlyData <- renderPlot({
    YearSub <- subset(stationData(), year == inputYear())
    YearSubSums <- setNames(aggregate(YearSub$rides, by=list(YearSub$month), FUN=sum), c("month", "rides"))
    ggplot(YearSub, aes(x = month, y = rides/1000)) + geom_bar(stat = "identity", fill = "#33647A", width=0.8) +
      labs(x = "Month", y ="Rides (in thousands)") + 
      theme_bw() +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12))  
  })
  
  output$WeeklyData <- renderPlot({
    YearSub <- subset(stationData(), year == inputYear())
    YearSubSums <- setNames(aggregate(YearSub$rides, by=list(YearSub$wday), FUN=sum), c("wday", "rides"))
    ggplot(YearSubSums, aes(x = wday, y = rides/1000)) + geom_bar(stat = "identity", fill = "#33647A", width=0.8) +
      labs(x = "Weekday", y ="Rides (in thousands)") + 
      theme_bw() +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12))  
  })
  
  output$AllYears <- renderPlot({
    YearSubSums <- setNames(aggregate(stationData()$rides, by=list(stationData()$year), FUN=sum), c("year", "rides"))
    ggplot(YearSubSums, aes(x = year, y = rides/100000)) + geom_bar(stat = "identity", fill = "#91b3bb", width=0.8) +
      labs(x = "Year", y ="Rides (in hundred thousands)") + 
      theme_bw() +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=25), axis.title=element_text(size=15)) 
  })
  
  
  # Per-Station Data Tables
  
  output$TableAllDays <- DT::renderDataTable(
    DT::datatable({ 
      YearSub <- subset(stationData(), year == inputYear())
      YearSubSums <- setNames(aggregate(YearSub$rides, by=list(YearSub$newDate), FUN=sum), c("Date", "Rides"))
      YearSubSums
    }, 
    options = list(searching = FALSE, pageLength = 14, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE 
    )
  )
  
  output$TableMonthlyData <- DT::renderDataTable(
    DT::datatable({ 
      YearSub <- subset(stationData(), year == inputYear())
      YearSubSums <- setNames(aggregate(YearSub$rides, by=list(YearSub$month), FUN=sum), c("Month", "Rides"))
      YearSubSums
    }, 
    options = list(searching = FALSE, pageLength = 14, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE 
    )
  )
  
  output$TableWeeklyData <- DT::renderDataTable(
    DT::datatable({ 
      YearSub <- subset(stationData(), year == inputYear())
      YearSubSums <- setNames(aggregate(YearSub$rides, by=list(YearSub$wday), FUN=sum), c("wday", "rides"))
      YearSubSums
    }, 
    options = list(searching = FALSE, pageLength = 14, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE 
    )
  )
  
}

shinyApp(ui = ui, server = server)



