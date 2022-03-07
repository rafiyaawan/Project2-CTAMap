library(shiny)
library(ggplot2)
library(shinydashboard)
library(lubridate)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(dplyr)

#read in the data for stations

#fix dates using lubridate

#add year, month and day columns for datasets

#changes rides from character to numeric
# UICHalsted$rides <- as.numeric(gsub(",","",UICHalsted$rides))
# Ohare$rides <- as.numeric(gsub(",","",Ohare$rides))
# Washington$rides <- as.numeric(gsub(",","",Washington$rides))


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



