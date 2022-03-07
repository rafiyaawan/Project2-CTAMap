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
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   
                   sidebarMenu(
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
      tabItem(tabName = "Datavisualizations"
              
      ) # tabitem Visualizations close
      
    ) #tabItems
    
  ) #dashboardBody
  
) #dashboardPage



#server functions
server <- function(input, output) {
  
  
}

shinyApp(ui = ui, server = server)



