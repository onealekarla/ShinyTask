server <- function(input, output) {
  
  
  # --------------------------------------------------------------------------------------------------------------------------
  # File upload
  # --------------------------------------------------------------------------------------------------------------------------
  
  # Allow for the upload of larger files
  options(shiny.maxRequestSize=30*1024^2)
  
  # The columns needed in the uploaded file
  cols_needed <- c("VEHICLE", "GPSDateTime", "X", "Y", "timesecs", "Distance")
  gps_df <- NULL
  filtered_df <- NULL
  
  
  # Determine file type, trigger events accordingly
  observeEvent(input$gps_file,{
    
    # Loading indicator
    show_modal_spinner(
      spin = "circle",
      color = "#3c8dbc",
      text = NULL,
    )
    reset_inputs("file")
    if (input$gps_file$type == 'text/csv') {
      get_separators()
    } else if (input$gps_file$type == 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet') {
      if (!is.null(read_to_df())) {
        data_preview()
        show_vehicles()
        show_info("Select a vehicle")
      }
    } else {
      shinyalert(
        title = "File Error",
        text = paste("File type not supported.\n",
                     "Please upload a file of type *.csv or *.xlsx."),
        type = "error"
      )
      remove_modal_spinner()
    }
  })
  
  # Show a modal to get the csv file separators from user
  get_separators <- function() {
    showModal(modalDialog(
      title = "Choose Separator and Decimal",
      fluidRow(
        column(width = 5, 
               radioButtons("separator",
                     "Separate fields at:",
                     c("Commas" = ",", "Semi-colons" = ";", "Other" = "custom"),
                     selected=";"),
          conditionalPanel(
            condition = 'input.separator == "custom"',
            textInput("custom_separator",
                      "Enter custom separator character:",
                      value = "|")
          )
        ),
        column(width = 5, 
               radioButtons("decimalSeparator", "Choose Decimal Separator:",
                     choices = list("Period (.)" = ".", "Comma (,)" = ","),
                     selected=",")
        )
      ),
      footer = tagList(actionButton("btn_sep", "OK"))
    ))
  }
  
  
  # --------------------------------------------------------------------------------------------------------------------------
  # Dataframe creation
  # --------------------------------------------------------------------------------------------------------------------------
  
  # When seperator modal is closed
  observeEvent(input$btn_sep,{
    removeModal()
    if (!is.null(read_to_df())) {
      data_preview()
      show_vehicles()
      show_info("Select a vehicle")
    }  
  })
  
  # Function that reads input$gps_file into a dataframe
  read_to_df <- function() {
    
    # Read csv file to dataframe
    if (input$gps_file$type == "text/csv") {
      tryCatch({
        if (input$separator == "custom") {
          gps_df <<- read.csv(input$gps_file$datapath, header=TRUE, sep=input$custom_separator, dec=input$decimalSeparator)
        } else {
          gps_df <<- read.csv(input$gps_file$datapath, header=TRUE, sep=input$separator, dec=input$decimalSeparator)
        }
      },
      
      # Handle error in file separators 
      error = function(cond) {
        shinyalert(
          title = "File Error",
          text = "Please reupload file and ensure that you have selected the correct separators.",
          type = "error"
        )
        show_info("Please retry uploading a file")
        reset_inputs("file")
        return(NULL)
      })
    } else {
      
      # Read xlsx file to dataframe
      gps_df <<- read.xlsx(input$gps_file$datapath, detectDates = FALSE)
    }
    
    # Validate columns
    if (!all(cols_needed %in% colnames(gps_df))) {
      shinyalert(
        title = "File Error",
        text = paste("The uploaded file doesn't contain the necessary fields.",
                     "Please reupload file and ensure that your file contains the following fields:\n",
                     "VEHICLE\n","GPSDateTime\n","X\n","Y\n","Distance\n","timesecs\n"),
        type = "error"
      )
      return(NULL)
    }
    
    # Correct GPSDateTime format
    if (input$gps_file$type == "text/csv") {
      gps_df$GPSDateTime <<- as.Date(gps_df$GPSDateTime, origin = '1899-12-30')
    } else {
      gps_df$GPSDateTime <<- as.Date(floor(gps_df$GPSDateTime), origin = '1899-12-30')
    }
  }
  
  
  # --------------------------------------------------------------------------------------------------------------------------
  # Show a preview of the data
  # --------------------------------------------------------------------------------------------------------------------------
  
  data_preview <- function() {
    
    output$daily_distance_heading <- renderText("Daily Distance Traveled by Vehicle")
    output$freq_areas_heading <- renderText("Frequently Visited Areas")
    
    # Determine areas most frequently visited
    combinations <- gps_df %>%
      mutate(
        # Round to 3 decimals to 'group' close coordinates
        X = round(X, digits = 3),  
        Y = round(Y, digits = 3)   
      ) %>%
      group_by(X, Y) %>%
      summarise(count = n()) %>%
      ungroup()
    
    combinations <- combinations %>%
      arrange(desc(count))
    
    top_10_combinations <- head(combinations, 20)
    
    # Define output for Leaflet Map
    output$freq_areas <- renderLeaflet({
      leaflet(top_10_combinations) %>%
        addTiles() %>%
        
        #Show GPS points based on coordinates
        addCircleMarkers(
          lng = (top_10_combinations$X),
          lat = (top_10_combinations$Y),
          radius = 10,
          color = ~colorQuantile("Blues", count)(count),
          fillOpacity = 1
        )
    })
    
    # Determine daily distances by vehicle
    df <- gps_df %>%
      group_by(VEHICLE, GPSDateTime) %>%
      summarize(TotalDistance = sum(Distance, na.rm = TRUE))
    
    # Show daily distances on a plot
    output$daily_distance <- renderPlotly({
      p <- ggplot(df, aes(x = GPSDateTime, y = TotalDistance, color = VEHICLE, text = paste("VEHICLE:", VEHICLE))) +
        theme_minimal() +
        theme(legend.position = "none") +
        geom_line() +
        xlab("Date") + 
        ylab("Distance (km)")
      
      ggplotly(p, tooltip = "text")
    })
  }
  
  
  # --------------------------------------------------------------------------------------------------------------------------
  # Dynamic UI
  # --------------------------------------------------------------------------------------------------------------------------
  
  # Function to show input$vehicles
  show_vehicles <- function() {
    output$vehicles = renderUI({
      selectizeInput('vehicles',
                     'Select Vehicle:',
                     unique(gps_df$VEHICLE), 
                     multiple = FALSE,
                     options = list(
                       placeholder = "Select Vehicle",
                       onInitialize = I('function() { this.setValue(""); }')
                     )
      )
    })
    shinyjs::show("vehicles")
    remove_modal_spinner()
  }
  
  # Show input$date when user selected a vehicle
  observeEvent(input$vehicles, {
    if (input$vehicles != "") {
      show_info("Select a date")
      output$dates = renderUI({
        dateInput('dates','Select Date:', value = '')
      })
      reset_inputs("dates")
    shinyjs::show("dates")
    }
  })
  
  # Show submit button when user selected a valid date
  observeEvent(input$dates, {
    if (length(input$dates) > 0) {
      
      # Filter dataframe by selected input
      filtered_df <<- filter(gps_df, 
                             VEHICLE == input$vehicles,
                             GPSDateTime == input$dates)
      
      # Validate selected date
      if (!input$dates %in% filtered_df$GPSDateTime) {
        shinyalert(
          title = "Date not Found",
          text = "The selected date was not found for this vehicle.\n
            Please try another date.",
          
          type = "error"
        )
        reset_inputs("dates")
      } else {
        show_btn()
        show_info("Click the 'Submit' button")
      }
    }
  })
  
  show_btn <- function() {
    #Define a dynamic button to submit user input
    output$btn_submit = renderUI({
      if (length(input$dates)>0) {
        actionButton('btn_submit','Submit') 
      }
    })
    shinyjs::show("btn_submit")
  }
  
  
  # --------------------------------------------------------------------------------------------------------------------------
  # Generate outputs based on user input
  # --------------------------------------------------------------------------------------------------------------------------
  
  # Trigger functions when submit button is clicked
  observeEvent(input$btn_submit,{
    filtered_df <<- mutate(filtered_df, timediff = c(0,diff(timesecs)))
    generate_map()
    generate_table()
    show_tabBox()
    show_vals()
    shinyjs::hide("preview_row")
    shinyjs::hide("info")
  })
  
  # Show tabBox containing map and table
  show_tabBox <- function() {
    #Define tabBox output
    output$box <- renderUI({
      tabBox(title = "",
             
             #Map View tab
             tabPanel("Map View",leafletOutput("plot", height = "65vh")),
             
             #Table View tab
             tabPanel("Table View",
                      DT::dataTableOutput("table")),
             width = 12
      )
    })
    shinyjs::show("box")
  }
  
  # Generate datatable
  generate_table <- function() {
    output$table <- DT::renderDataTable(options = list(scrollX = TRUE),{
      data <- filtered_df %>%
        select(where(~ any(!is.na(.))))
      data
    })
  }
  
  # Generate map and plot points
  generate_map <- function() {
    #Define output for Leaflet Map
    output$plot <- renderLeaflet({
      leaflet(filtered_df) %>%
        addTiles() %>%
        
        #Show GPS points based on coordinates
        addCircleMarkers(
          lng = (filtered_df$X),
          lat = (filtered_df$Y),
          radius = 2,
          weight = 2
        ) %>%
        
        #Show lines between points
        addPolylines(filtered_df$X,filtered_df$Y,
                     group = 'Routes',
                     color = 'black'
        ) %>%
        
        #Allow user to show / hide route lines
        addLayersControl(overlayGroups = c('Routes'))
    })
  }
  
  # Show important values - distance and average speed
  show_vals <- function() {
    
    # Calculate total distance travelled by vehicle & date
    total_distance <- sum(filtered_df$Distance, na.rm = TRUE)
    
    # Calculate average speed by vehicle & date
    avg_speed <- round(total_distance
            / sum(filtered_df$timediff[filtered_df$Distance != 0]/ 3600, na.rm = TRUE) )
                  
    
    # Show valueBoxes
    output$distance <- renderValueBox(
      valueBox(
        value = paste(
          round(total_distance,2),
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
      if (avg_speed > 35) {
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
          avg_speed,
          " km/h"
        ),
        subtitle = "Average Speed",
        icon = icon("gauge-simple-high"),
        color = box_color
      )
    })
    shinyjs::show("distance")
    shinyjs::show("speed")
  }
  
  
  # --------------------------------------------------------------------------------------------------------------------------
  # Additional display functions
  # --------------------------------------------------------------------------------------------------------------------------
  
  show_info <- function(info_text) {
    output$info <- renderInfoBox(
      infoBox(
        title = "Next:",
        value = HTML(paste("<div class='info-box-title'>",
                           info_text,
                           "</div>")),
        icon = icon("angle-left"),
        color = "purple"
      )
    )
  }
  
  # Reset the inputs to the specified point
  reset_inputs <- function(input_name) {
    if (input_name == "file") {
      hide("start")
      shinyjs::show("info")
      hide("vehicles")
      hide("dates")
      hide("box")
      hide("distance")
      hide("speed")
      hide("btn_submit")
    } else if (input_name == "dates") {
      shinyjs::show("preview_row")
      hide("btn_submit")
      hide("box")
      hide("distance")
      hide("speed")
      updateDateInput(session = getDefaultReactiveDomain(), "dates", value = NA)
    }
  }
}
  