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
UICHalsted$rides <- as.numeric(gsub(",","",UICHalsted$rides))
Ohare$rides <- as.numeric(gsub(",","",Ohare$rides))
Washington$rides <- as.numeric(gsub(",","",Washington$rides))


#menu options for selecting years
years<-c("2001", "2002", "2003", "2004", "2005",
         "2006", "2007", "2008", "2009", "2010",
         "2011", "2012", "2013", "2014", "2015",
         "2016", "2017", "2018", "2019", "2020", "2021")
dataframe <- c("UICHalsted", "Ohare", "Washington")

# Create the shiny dashboard
ui <- dashboardPage(
  #change header color
  skin = "black",
  
  dashboardHeader(title = "CS 424 Spring 2022 Project 2"),
  
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
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     #TODO add about page
                   )
                   
  ),
  dashboardBody(
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
    )
  ))



#server functions
server <- function(input, output) {
  
  
}

shinyApp(ui = ui, server = server)



