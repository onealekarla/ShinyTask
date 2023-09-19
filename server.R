server <- function(input, output) {
  options(shiny.maxRequestSize=30*1024^2)
  
  gps_df <<- reactive({
    req(input$gps_file, TRUE, file.exists(input$gps_file$datapath))
    if (input$gps_file$type == 'text/csv') {
      read.csv(input$gps_file$datapath,TRUE, sep=";") %>%
        mutate(timediff = c(0,diff(timesecs)))
    } else {
      read.xlsx(input$gps_file$datapath, detectDates = FALSE) %>%
        mutate(GPSDateTime = as.Date(floor(GPSDateTime), origin = '1899-12-30'),
               timediff = c(0,diff(timesecs))
        )
    } 
  })
  
  output$vehicles = renderUI({
    selectizeInput('vehicle',
                   'Select Vehicle:',
                   unique(gps_df()$VEHICLE), 
                   multiple = FALSE,
                   options = list(
                     placeholder = "Select Vehicle",
                     onInitialize = I('function() { this.setValue(""); }')
                   )
    )
  })
  
  output$dates = renderUI({
    if (!is.null(input$vehicle) && input$vehicle != "") {
      dateInput('date','Select Date:', value = '')
    }
  })
  
  output$submit = renderUI({
    if (length(input$date)>0) {
      actionButton('plot','Submit')  
    }
  })
  
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
  
  plot_points <- eventReactive(input$plot,{
    filter(gps_df(), 
           VEHICLE == input$vehicle, 
           GPSDateTime == input$date
    )
  })
  
  observe({
    if (!is.null(input$plot)) {
      shinyjs::show("body_content")
    } else {
      shinyjs::hide("body_content")
    }
  })
  
  total_distance <- eventReactive(input$plot,{
    sum(plot_points()$Distance)
  })
  
  avg_speed <- eventReactive(input$plot,{
    round(total_distance()/sum(plot_points()$timediff[plot_points()$Distance != 0] / 3600))
  })
  
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
  
  box_color <- 'green'
    output$speed <- renderValueBox({
    
    if (avg_speed() > 35) {
      box_color = "red"
      shinyalert(
        title = "Warning!",
        text = "Average speed is too high.",
        type = "warning"
      ) 
    }
    
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
  
  output$table <- DT::renderDataTable(options = list(scrollX = TRUE),{
    data <- plot_points() %>%
      select(where(~ any(!is.na(.))))
    data
  })
  
  output$plot <- renderLeaflet({
    leaflet(plot_points) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = (plot_points()$X),
        lat = (plot_points()$Y),
        radius = 2,
        weight = 2
      ) %>%
      addPolylines(plot_points()$X,plot_points()$Y,
                   group = 'Routes',
                   color = 'black'
      ) %>%
      addLayersControl(overlayGroups = c('Routes'))
  })
}