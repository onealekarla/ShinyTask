library(leaflet)
library(tidyverse)
library(DT)
library(shiny)
library(shinydashboard)
library(readxl)
library(DT)
library(openxlsx)
library(shinythemes)
library(shinyalert)
library(terra)

ui <- shinyUI({
  
  #Create the dashboard header, titled 'Vehicle History'
  header <- dashboardHeader(
    title = "Vehicle History"
  )
  
  #Create the dashboard sidebar
  sidebar <- dashboardSidebar(
    #Custom css
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    #Change the look of the dashboard
    shinyDashboardThemes("grey_dark"),
    
    #Add a file input widget for uploading a single *.csv or *.xlsx file
    fileInput('gps_file','Upload Data:', multiple = FALSE, accept = c('.csv', '.xlsx')),
    
    #UI elements for vehicle and date selection
    #Dynamically generated based on whether a user has provided the necessary input
    uiOutput('vehicles'),
    uiOutput('dates'),
    
    #Creates a center-aligned 'Submit' button
    #Dynamically generated based on whether a user has provided the necessary input
    div(
      uiOutput('submit'),
      align = "center"
    ),
    
    #Shows text providing hints / instructions
    div(class="textOutput",
      textOutput('hint')
    )
  )
  
  
  #Create the main body of the dasboard
  body <- dashboardBody(
    id = "body_content",
      
      #Create a row with two value boxes displaying the total 
      #distance and average speed by vehicle and date                  
      fluidRow(
        valueBoxOutput("distance", width = 6),
        valueBoxOutput("speed", width = 6)
      ),
    
      #Create a row with a tabbed interface for the map and table views
      fluidRow(
          tabBox(id = "plot_box",
                 
                 #Map View tab
                 tabPanel("Map View",leafletOutput("plot")),
                 
                 #Table View tab
                 tabPanel("Table View",
                          DT::dataTableOutput("table")),
                 width = 12
          )
      )
  )
  
  #Combine the dashboard elements to create the final UI
  ui <- dashboardPage(header, sidebar, body)
  
})