library(leaflet)
library(tidyverse)
library(DT)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(readxl)
library(DT)
library(openxlsx)
library(shinyalert)
library(shinybusy)
library(terra)
library(plotly)

ui <- shinyUI({
  
  #Create the dashboard header, titled 'Vehicle History'
  header <- dashboardHeader(
    title = "Vehicle History"
  )
  
  #Create the dashboard sidebar
  sidebar <- dashboardSidebar(
    useShinyjs(),
    
    #Custom css
    includeCSS("www/style.css"),  # Include the external CSS file
    
    #Add a file input widget for uploading a single *.csv or *.xlsx file
    fileInput('gps_file','Upload Data:', multiple = FALSE, accept = c('.csv', '.xlsx')),
    
    #UI elements for vehicle and date selection
    #Dynamically generated based on whether a user has provided the necessary input
    uiOutput('vehicles'),
    uiOutput('dates'),
    
    #Creates a center-aligned 'Submit' button
    #Dynamically generated based on whether a user has provided the necessary input
    div(
      class = "button",
      uiOutput('btn_submit'),
      align = "center"
    ),
    
    #Shows text providing hints / instructions
    div(class="textOutput",
      textOutput('hint')
    )
  )
  
  
  # Create the main body of the dashboard
  body <- dashboardBody(
    
    # Add an instructional box
    fluidRow(
      id = "start",
      infoBox(
        title = "Get Started",
        value = HTML("<div class='info-box-title'>Upload a file to get started.</div>"),
        icon = icon("angle-left"),
        color = "purple"
      )
    ),
    fluidRow(
      infoBoxOutput('info')
    ),
    
    # Show a preview of the data while user provides input
    fluidRow(
      id = "preview_row",
      column(width = 6,
             div(
               class = "h3",
               uiOutput("freq_areas_heading")
             ),
             leaflet::leafletOutput("freq_areas", height = "78vh")     
      ),
      column(width = 6,
             div(
               class = "h3",
               uiOutput("daily_distance_heading")
             ),
             plotlyOutput("daily_distance", height = "78vh")
      )
    ),
    
      
    #Create a row with two value boxes displaying the total 
    #distance and average speed by vehicle and date                  
    fluidRow(
      valueBoxOutput("distance", width = 6),
      valueBoxOutput("speed", width = 6)
    ),
    
    #Create a row with a tabbed interface for the map and table views
    fluidRow(
      uiOutput("box")
    )
  )
  
  #Combine the dashboard elements to create the final UI
  ui <- dashboardPage(skin = "yellow", header, sidebar, body)
  
})