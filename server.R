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

server <- function(input, output) {
  
  #Allow for the upload of larger files
  options(shiny.maxRequestSize=30*1024^2)
  
  #Define a reactive function to read and process the uploaded GPS data
  gps_df <<- eventReactive(input$gps_file,{
    #Ensure that file was uploaded
    req(input$gps_file, TRUE, file.exists(input$gps_file$datapath))
    
    #Load data based on file type
    if (input$gps_file$type == 'text/csv') {
      data <- read.csv(input$gps_file$datapath,TRUE, sep=";")
    } else {
      data <- read.xlsx(input$gps_file$datapath, detectDates = FALSE)
      data$GPSDateTime <- as.Date(floor(data$GPSDateTime), origin = '1899-12-30')
    }
    
    return(data)
  })
  
  plot_points <- eventReactive(input$plot,{
  
    #Filter based on user selection of vehicle and date
    filtered_data <- filter(gps_df(), 
                            VEHICLE == input$vehicle, 
                            GPSDateTime == input$date
    )
    
    #Add a column 'timediff' to be used in speed calculation
    filtered_data <- mutate(filtered_data, timediff = c(0,diff(timesecs)))
    
    return(filtered_data)
  })
  
  #Define a dynamic UI element for selecting vehicles based on the data provided
  output$vehicles = renderUI({
    if (!(is.null(input$gps_file) || input$gps_file$name == '')) {
      selectizeInput('vehicle',
                     'Select Vehicle:',
                     unique(gps_df()$VEHICLE), 
                     multiple = FALSE,
                     options = list(
                       placeholder = "Select Vehicle",
                       onInitialize = I('function() { this.setValue(""); }')
                     )
      )
    }
  })
  
  #Define a dynamic dateInput for selecting dates as soon as a vehicle was selected
  output$dates = renderUI({
    if (!is.null(input$vehicle) && input$vehicle != "") {
      dateInput('date','Select Date:', value = '')
    }
  })
  
  #Define a dynamic button to submit user input
  output$submit = renderUI({
    if (length(input$date)>0) {
      actionButton('plot','Submit')  
    }
  })
  
  #Define hint / instructions based on user inputs
  output$hint <- renderText({
    if (is.null(input$gps_file) || input$gps_file$name == '') {
      txt <- "Upload a file to continue."
    } else if (is.null(input$vehicle) || input$vehicle == "") {
      txt <- "Select a vehicle to continue."
    } else if (!(length(input$date)>0))  {
      txt <- "Choose a date to continue."
    } else {
      txt <- ""
    }
    txt
  })
  
  #Calculate total distance when button clicked
  total_distance <- eventReactive(input$plot,{
    sum(plot_points()$Distance)
  })
  
  #Calculate average speed when button clicked
  #Time where Distance is zero, excluded from total travel time
  avg_speed <- eventReactive(input$plot,{
    round(total_distance()/sum(plot_points()$timediff[plot_points()$Distance != 0] / 3600))
  })
  
  #Define output for the speed value box
  output$distance <- renderValueBox(
    valueBox(
      value = paste(
        round(total_distance(),2),
        " km"
      ),
      subtitle = "Distance Travelled",
      icon = icon("route"),
      color = "aqua"
    )
  )
  
  #Set default color and define output for average speed
  box_color <- 'green'
  output$speed <- renderValueBox({
    
    #Show warning and change color of value box to red
    if (avg_speed() > 35) {
      box_color = "red"
      shinyalert(
        title = "Warning!",
        text = "Average speed is too high.",
        type = "warning"
      ) 
    }
    
    #Define value box
    valueBox(
      value = paste(
        avg_speed(),
        " km/h"
      ),
      subtitle = "Average Speed",
      icon = icon("gauge-simple-high"),
      color = box_color
    )
  })
  
  #Define output for data table, excluding empty columns
  output$table <- DT::renderDataTable(options = list(scrollX = TRUE),{
    data <- plot_points() %>%
      select(where(~ any(!is.na(.))))
    data
  })
  
  #Define output for Leaflet Map
  output$plot <- renderLeaflet({
    leaflet(plot_points) %>%
      addTiles() %>%
      
      #Show GPS points based on coordinates
      addCircleMarkers(
        lng = (plot_points()$X),
        lat = (plot_points()$Y),
        radius = 2,
        weight = 2
      ) %>%
      
      #Show lines between points
      addPolylines(plot_points()$X,plot_points()$Y,
                   group = 'Routes',
                   color = 'black'
      ) %>%
      
      #Allow user to show / hide route lines
      addLayersControl(overlayGroups = c('Routes'))
  })
}